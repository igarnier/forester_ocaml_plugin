module T = Forester_core.Types
module V = Forester_core.Value
module DSL = Forester_frontend.DSL
module Read = Eio.Buf_read
module Write = Eio.Buf_write

let log to_logfile str =
  Write.string to_logfile str ;
  Write.string to_logfile "\n"

let binary_name = "forester_ocaml_plugin_server"

let read_string =
  Read.bind Read.BE.uint64 @@ fun size -> Read.take (Int64.to_int size)

let write_string write msg =
  let len = Int64.of_int @@ String.length msg in
  Write.BE.uint64 write len ;
  Write.string write msg

let port_counter = ref 0

let base_port = 31001

let next_port () =
  let c = !port_counter in
  incr port_counter ;
  base_port + c

let text_content str = T.Content [T.Text str]

let trim_opt str =
  let str = String.trim str in
  if String.length str = 0 then None else Some str

let code_block (snippet : string) (stdout : string option)
    (output : string option) =
  let open DSL in
  let maybe_stdout =
    Option.bind stdout @@ fun stdout ->
    Option.bind (trim_opt stdout) @@ fun stdout ->
    Option.some (pre [txt stdout])
  in
  let maybe_output = Option.map (fun output -> pre [txt output]) output in
  let blocks =
    [pre [txt (String.trim snippet)]]
    @ Option.to_list maybe_output
    @ Option.to_list maybe_stdout
  in
  p blocks

let redirect (sw : Eio.Switch.t) stdout stderr dst =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Std.Fiber.both
        (fun () -> Eio.Flow.copy stdout dst)
        (fun () -> Eio.Flow.copy stderr dst) ;
      `Stop_daemon)

let fork_server logger env sw ~stdout ~stderr port =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      ignore
      @@ Eio.Process.spawn
           ~sw
           ~stdout
           ~stderr
           proc_mgr
           [binary_name; string_of_int port] ;
      `Stop_daemon) ;
  logger (Printf.sprintf "Started server on port %d" port)

let plugin : Forester_compiler.Plugin.plugin =
 fun (env, sw) ->
  (* Open log file *)
  let path = Eio.Path.(Eio.Stdenv.cwd env / "forester_ocaml_plugin.log") in
  let log_flow =
    Eio.Path.open_out ~sw ~append:true ~create:(`If_missing 0o644) path
  in
  Write.with_flow log_flow @@ fun log_writer ->
  let logger = log log_writer in
  let net = Eio.Stdenv.net env in
  (* Each instance gets a fresh port.
     Ideally we'd ask the OS for one but not clear how to do so in a portable way. *)
  let port = next_port () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  (* Spawn server for this instance *)
  let (server_stdout, stdout) = Eio_unix.pipe sw in
  let (server_stderr, stderr) = Eio_unix.pipe sw in
  fork_server logger env sw ~stdout ~stderr port ;
  let from_server_pipe = Read.of_flow ~max_size:512 server_stdout in
  begin
    (* Read greeting from server. This also allows to synchronize with the server. *)
    try Read.string "forester_ocaml_plugin_server: starting" from_server_pipe
    with Failure _ | End_of_file ->
      Forester_core.(
        Reporter.fatal
          (Reporter.Message.Plugin_initialization_error
             (text_content "ocaml plugin: unexpected greeting from server")))
  end ;
  logger "Received server greeting - plugin initialization successful" ;

  (* Redirect rest of server stdout to logfile *)
  redirect sw server_stdout server_stderr log_flow ;

  logger
  @@ Format.asprintf "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr ;
  let flow = Eio.Net.connect ~sw net addr in

  let parse_options opt =
    match opt with
    | V.Content (T.Content []) -> (false, false, false)
    | V.Content (T.Content [T.Text text]) ->
        let opts = String.split_on_char ',' text in
        let no_stdout = List.mem "no-stdout" opts in
        let no_echo = List.mem "no-echo" opts in
        let silent = List.mem "silent" opts in
        (no_stdout, no_echo, silent)
    | _ ->
        logger
        @@ Format.asprintf
             "Expected option formatted as text content, got %a"
             Forester_core.Value.pp
             opt ;
        (false, false, false)
  in

  (* Before trying to connect, wait on server startup to finish. *)
  let step : Forester_compiler.Plugin.step =
   fun (args : V.t list) ->
    match args with
    | [opt; V.Content (T.Content [T.Text text])] -> (
        let (no_stdout, no_echo, silent) = parse_options opt in
        try
          Write.with_flow flow @@ fun to_server ->
          write_string to_server text ;
          let from_server = Read.of_flow flow ~max_size:32768 in
          let captured_stdout = read_string from_server in
          let captured_stderr = read_string from_server in
          let output = read_string from_server in
          if silent then Result.ok (V.Content (T.Content []))
          else begin
            let code_block =
              code_block
                text
                (if no_stdout then None else Some captured_stdout)
                (if no_echo then None else Some output)
            in
            if String.length captured_stderr <> 0 then begin
              Format.printf
                "forester_ocaml_plugin: while processing \"%s\""
                text ;
              Format.printf
                "forester_ocaml_plugin: stderr = \"%s\""
                captured_stderr
            end ;
            Result.ok (V.Content (T.Content [code_block]))
          end
        with End_of_file ->
          logger "End_of_file caught - continuing" ;
          Result.ok (V.Content (T.Content [T.Text "End_of_file"])))
    | _ ->
        Format.kasprintf
          Result.error
          "Unhandled values: %a"
          (Fmt.Dump.list Forester_core.Value.pp)
          args
  in
  { Forester_compiler.Plugin.step_arity = 2; step }

let () = Forester_compiler.Plugin.register "ocaml" plugin

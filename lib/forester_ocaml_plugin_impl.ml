open Eio.Std
module T = Forester_core.Types
module V = Forester_core.Value
module DSL = Forester_frontend.DSL
module Read = Eio.Buf_read
module Write = Eio.Buf_write

let traceln fmt = traceln ("ocaml plugin: " ^^ fmt)

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

let code_block (snippet : string) (stdout : string) (output : string) =
  let open DSL in
  p (pre [(txt snippet)] ::
     pre [(txt output)] ::
     (if (String.length (String.trim stdout) = 0) then
        []
      else
        [pre [(txt stdout)]]
     )
    )

let redirect (sw : Eio.Switch.t) src dst =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Flow.copy src dst ;
      `Stop_daemon)

let fork_server env sw sink port =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      ignore
      @@ Eio.Process.spawn
           ~sw
           ~stdout:sink
           proc_mgr
           [binary_name; string_of_int port] ;
      `Stop_daemon) ;
  traceln "Started server on port %d" port

let plugin : Forester_compiler.Plugin.plugin =
 fun (env, sw) ->
  let net = Eio.Stdenv.net env in
  let step_arity = 1 in
  (* Each instance gets a fresh port.
     Ideally we'd ask the OS for one but not clear how to do so in a portable way. *)
  let port = next_port () in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  (* Spawn server for this instance *)
  let (server_stdout, sink) = Eio_unix.pipe sw in
  fork_server env sw sink port ;
  let from_server_pipe = Read.of_flow ~max_size:512 server_stdout in
  begin
    (* Read greeting from server. This also allows to syncronize with the server.
       TODO: simpler sync mechanism. *)
    try Read.string "forester_ocaml_plugin_server: starting" from_server_pipe
    with Failure _ | End_of_file ->
      Forester_core.(
        Reporter.fatal
          (Reporter.Message.Plugin_initialization_error
             (text_content "ocaml plugin: unexpected greeting from server")))
  end ;
  traceln "Received server greeting - plugin initialization successful" ;

  (* Redirect rest of server stdout to Forester's stdout *)
  redirect sw server_stdout env#stdout ;

  traceln "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr ;
  let flow = Eio.Net.connect ~sw net addr in

  (* Before trying to connect, wait on server startup to finish. *)
  let step : Forester_compiler.Plugin.step =
   fun (args : V.t list) ->
    match args with
    | [V.Content (T.Content [T.Text text])] -> (
        traceln "@[<v 2>%s@]" text ;
        try
          Write.with_flow flow @@ fun to_server ->
          write_string to_server text ;
          let from_server = Read.of_flow flow ~max_size:32768 in
          let captured_stdout = read_string from_server in
          let output = read_string from_server in
          Result.ok
            (V.Content (T.Content [(code_block text captured_stdout output)]))
        with End_of_file ->
          traceln "End_of_file caught - continuing" ;
          Result.ok (V.Content (T.Content [T.Text "End_of_file"])))
    | _ -> Result.error ""
  in
  { step_arity; step }

let () = Forester_compiler.Plugin.register "ocaml" plugin

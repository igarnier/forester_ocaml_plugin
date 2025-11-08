open Eio.Std

(*
   TODO: use native toplevel (look at how ocamlnat initializes it)
*)

let capture_stdout temp_file_name f =
  let temp_file_fd =
    Unix.openfile temp_file_name [O_RDWR; O_CREAT; O_TRUNC] 0o640
  in
  let stdout_copy = Unix.dup Unix.stdout in
  (* Let stdout point to temp file *)
  Unix.dup2 temp_file_fd Unix.stdout ;
  let res = f () in
  Format.print_flush () ;
  Unix.fsync temp_file_fd ;
  Unix.dup2 stdout_copy Unix.stdout ;
  ignore (Unix.lseek temp_file_fd 0 SEEK_SET) ;
  let contents = In_channel.input_all (Unix.in_channel_of_descr temp_file_fd) in
  (res, contents)

(* Prefix all trace output with "server: " *)
let traceln fmt = traceln ("server: " ^^ fmt)

module Read = Eio.Buf_read
module Write = Eio.Buf_write

let parse_phrase str =
  let lexbuf = Lexing.from_string str in
  try Ok (!Toploop.parse_toplevel_phrase lexbuf)
  with Syntaxerr.Error _ as e -> (
    match Location.error_of_exn e with
    | None -> Error "Unhandled parse error"
    | Some (`Ok err) ->
        Format.kasprintf Result.error "%a" Location.print_report err
    | Some `Already_displayed -> Error "Already displayed")

type outcome = { stdout : string; output : string; success : bool }

let execute_phrase temp_file str =
  let output_buf = Buffer.create 512 in
  Buffer.clear output_buf ;
  let output_fmtr = Format.formatter_of_buffer output_buf in
  Result.bind (parse_phrase str) @@ fun toplevel_phrase ->
  try
    let (success, stdout) =
      capture_stdout temp_file @@ fun () ->
      Toploop.execute_phrase true output_fmtr toplevel_phrase
    in
    let output = Buffer.contents output_buf in
    Result.ok { stdout; output; success }
  with
  | Typecore.Error (loc, env, err) ->
      let report = Typecore.report_error ~loc env err in
      Format.kasprintf Result.error "%a" Location.print_report report
  | Env.Error err -> Format.kasprintf Result.error "%a" Env.report_error err
  | exn ->
      Format.kasprintf
        Result.error
        "Exception caught during phrase evaluation (%s)"
        (Printexc.to_string exn)

let read_string =
  Read.bind Read.BE.uint64 @@ fun size -> Read.take (Int64.to_int size)

let write_string write msg =
  let len = Int64.of_int @@ String.length msg in
  Write.BE.uint64 write len ;
  Write.string write msg

(* Read one line from [client] and respond with "OK". *)
let handle_client flow addr =
  traceln "Accepted connection from %a" Eio.Net.Sockaddr.pp addr ;
  let temp_file_name = Filename.temp_file "forester-ocaml-plugin-" ".tmp" in
  traceln "Using %s as temp file" temp_file_name ;
  (* We use a buffered reader because we may need to combine multiple reads
     to get a single line (or we may get multiple lines in a single read,
     although here we only use the first one). *)
  let from_client = Read.of_flow flow ~max_size:32768 in
  let rec loop () =
    let input = read_string from_client in
    traceln "processing: %S" input ;
    begin
      Write.with_flow flow @@ fun writer ->
      match execute_phrase temp_file_name input with
      | Error msg ->
          traceln "error: %S@." msg ;
          let stdout = "" in
          let output = Format.asprintf "failed to execute phrase: %s" msg in
          write_string writer stdout ;
          write_string writer output
      | Ok { stdout; output; success = _ } ->
          write_string writer stdout ;
          traceln "stdout: %S@." stdout ;
          write_string writer output ;
          traceln "output: %S@." output
    end ;
    loop ()
  in
  let () = Toploop.initialize_toplevel_env () in
  try loop ()
  with End_of_file -> traceln "%a: end of input" Eio.Net.Sockaddr.pp addr

(* Accept incoming client connections on [socket].
   We can handle multiple clients at the same time.
   Never returns (but can be cancelled). *)
let run socket =
  Eio.Net.run_server
    socket
    handle_client
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)
    ~max_connections:1

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Switch.run ~name:"server" @@ fun sw ->
  let listening_socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1 addr
  in
  run listening_socket

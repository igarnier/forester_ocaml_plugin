open Eio.Std

(*
   TODO: use native toplevel (look at how ocamlnat initializes it)
*)


let capture_fd fd temp_file_name f =
  let temp_file_fd =
    Unix.openfile temp_file_name [O_RDWR; O_CREAT; O_TRUNC] 0o640
  in
  let fd_copy = Unix.dup fd in
  (* Let stdout point to temp file *)
  Unix.dup2 temp_file_fd fd ;
  let res = f () in
  Format.print_flush () ;
  Unix.fsync temp_file_fd ;
  Unix.dup2 fd_copy fd ;
  ignore (Unix.lseek temp_file_fd 0 SEEK_SET) ;
  let contents = In_channel.input_all (Unix.in_channel_of_descr temp_file_fd) in
  (res, contents)

let capture_stdout stdout_tmp_file_name f = capture_fd Unix.stdout stdout_tmp_file_name f
let capture_stderr stderr_tmp_file_name f = capture_fd Unix.stderr stderr_tmp_file_name f

(* Prefix all trace output with "server: " *)
let traceln fmt = traceln ("forester_ocaml_plugin_server: " ^^ fmt)

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

type outcome = { stdout : string; stderr : string; output : string; success : bool }

let execute_phrase stdout_tmp_file stderr_tmp_file str =
  let output_buf = Buffer.create 512 in
  Buffer.clear output_buf ;
  let output_fmtr = Format.formatter_of_buffer output_buf in
  Result.bind (parse_phrase str) @@ fun toplevel_phrase ->
  try
    let ((success, stderr), stdout) =
      capture_stdout stdout_tmp_file @@ fun () ->
      capture_stderr stderr_tmp_file @@ fun () ->
      Toploop.execute_phrase true output_fmtr toplevel_phrase
    in
    let output = Buffer.contents output_buf in
    Result.ok { stdout; stderr; output; success }
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
  let stdout_tmp_file_name = Filename.temp_file "forester-ocaml-plugin-stdout" ".tmp" in
  let stderr_tmp_file_name = Filename.temp_file "forester-ocaml-plugin-stderr" ".tmp" in

  let () = Toploop.initialize_toplevel_env () in

  traceln "Accepted connection from %a" Eio.Net.Sockaddr.pp addr ;
  traceln "Using %s as stdout buffer file" stdout_tmp_file_name ;
  traceln "Using %s as stderr buffer file" stderr_tmp_file_name ;

  let rec loop () =
    let from_client = Read.of_flow flow ~max_size:32768 in
    let input = read_string from_client in
    begin
      Write.with_flow flow @@ fun writer ->
      match execute_phrase stdout_tmp_file_name stderr_tmp_file_name input with
      | Error msg ->
          let stdout = "" in
          let stderr = "" in
          let output = Format.asprintf "failed to execute phrase: %s" msg in
          write_string writer stdout ;
          write_string writer stderr ;
          write_string writer output
      | Ok { stdout; stderr; output; success = _ } ->
          write_string writer stdout ;
          write_string writer stderr ;
          write_string writer output
    end ;
    loop ()
  in
  try loop () with
  | End_of_file -> ()
  | exn -> traceln "unhandled exception (%s)" (Printexc.to_string exn)

(* Accept incoming client connections on [socket].
   We can handle multiple clients at the same time.
   Never returns (but can be cancelled). *)
let run socket =
  Eio.Net.run_server
    socket
    handle_client
    ~on_error:(traceln "Error handling connection: %a" Fmt.exn)
    ~max_connections:1

let usage () =
  Format.eprintf "usage: `forester_ocaml_plugin_server <port number>`@."

let () =
  match Array.to_list Sys.argv with
  | [] | [_] | _ :: _ :: _ :: _ ->
      Format.eprintf
        "forester_ocaml_plugin_server: invalid invocation, exiting@." ;
      usage () ;
      exit 1
  | [_; port] -> (
      let error () =
        Format.eprintf
          "forester_ocaml_plugin_server: '%s' is not a valid port, exiting@."
          port ;
        usage () ;
        exit 1
      in
      match int_of_string port with
      | exception Failure _ -> error ()
      | i when i <= 0 -> error ()
      | port ->
          let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
          Eio_main.run @@ fun env ->
          let net = Eio.Stdenv.net env in
          Switch.run ~name:"server" @@ fun sw ->
          let listening_socket =
            Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1 addr
          in
          let greeting = "forester_ocaml_plugin_server: starting" in
          Write.with_flow env#stdout @@ fun stdout ->
          Write.string stdout greeting ;
          (* Start processing *)
          run listening_socket)

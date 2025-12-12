open Eio.Std

(*
   TODO: use native toplevel (look at how ocamlnat initializes it)
*)

module Capture : sig
  type t

  type 'a with_captured = { stdout : string; stderr : string; outcome : 'a }

  val create : unit -> t

  val capture : t -> (unit -> 'a) -> 'a with_captured
end = struct
  type t = { stdout_tmp_file : string; stderr_tmp_file : string }

  type 'a with_captured = { stdout : string; stderr : string; outcome : 'a }

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
    let contents =
      In_channel.input_all (Unix.in_channel_of_descr temp_file_fd)
    in
    (res, contents)

  let capture_stdout stdout_tmp_file_name f =
    capture_fd Unix.stdout stdout_tmp_file_name f

  let capture_stderr stderr_tmp_file_name f =
    capture_fd Unix.stderr stderr_tmp_file_name f

  let create () =
    let stdout_tmp_file =
      Filename.temp_file "forester-ocaml-plugin-stdout" ".tmp"
    in
    let stderr_tmp_file =
      Filename.temp_file "forester-ocaml-plugin-stderr" ".tmp"
    in
    { stdout_tmp_file; stderr_tmp_file }

  let capture { stdout_tmp_file; stderr_tmp_file } f =
    let ((outcome, stderr), stdout) =
      capture_stdout stdout_tmp_file @@ fun () ->
      capture_stderr stderr_tmp_file @@ fun () -> f ()
    in
    { stdout; stderr; outcome }
end

module REPL = struct
  (* code taken from Utop *)
  let split_words str =
    let len = String.length str in
    let is_sep = function
      | ' ' | '\t' | '\r' | '\n' | ',' -> true
      | _ -> false
    in
    let rec skip acc i =
      if i = len then acc
      else if is_sep str.[i] then skip acc (i + 1)
      else extract acc i (i + 1)
    and extract acc i j =
      if j = len then String.sub str i (j - i) :: acc
      else if is_sep str.[j] then skip (String.sub str i (j - i) :: acc) (j + 1)
      else extract acc i (j + 1)
    in
    List.rev (skip [] 0)

  let handle_findlib_error = function
    | Failure msg -> Format.eprintf "%s\n" msg
    | Fl_package_base.No_such_package (pkg, reason) ->
        Printf.eprintf
          "No such package: %s%s\n"
          pkg
          (if reason <> "" then " - " ^ reason else "")
    | Fl_package_base.Package_loop pkg ->
        Printf.eprintf "Package requires itself: %s" pkg
    | exn ->
        Format.eprintf
          "Exception caught during findlib invocation (%s)\n"
          (Printexc.to_string exn)

  let require packages =
    try
      let eff_packages =
        Findlib.package_deep_ancestors !Topfind.predicates packages
      in
      Topfind.load eff_packages
    with exn -> handle_findlib_error exn

  let parse_phrase str =
    let lexbuf = Lexing.from_string str in
    try Ok (!Toploop.parse_toplevel_phrase lexbuf)
    with Syntaxerr.Error _ as e -> (
      match Location.error_of_exn e with
      | None -> Error "Unhandled parse error"
      | Some (`Ok err) ->
          Format.kasprintf Result.error "%a" Location.print_report err
      | Some `Already_displayed -> Error "Already displayed")

  let execute_phrase handle str =
    Capture.capture handle @@ fun () ->
    let output_buf = Buffer.create 512 in
    let output_fmtr = Format.formatter_of_buffer output_buf in
    Result.bind (parse_phrase str) @@ fun toplevel_phrase ->
    try
      let toplevel_phrase =
        Toploop.preprocess_phrase output_fmtr toplevel_phrase
      in
      Ok (Toploop.execute_phrase true output_fmtr toplevel_phrase, output_buf)
    with
    | Typecore.Error (loc, env, err) ->
        let report = Typecore.report_error ~loc env err in
        Format.kasprintf Result.error "%a" Location.print_report report
    | Env.Error err -> Format.kasprintf Result.error "%a" Env.report_error err
    | exn ->
        Format.kasprintf
          Result.error
          "Exception caught during phrase evaluation (%s)\n"
          (Printexc.to_string exn)
end

(* Prefix all trace output with "server: " *)
let traceln fmt = traceln ("forester_ocaml_plugin_server: " ^^ fmt)

module Read = Eio.Buf_read
module Write = Eio.Buf_write

let read_string =
  Read.bind Read.BE.uint64 @@ fun size -> Read.take (Int64.to_int size)

let write_string write msg =
  let len = Int64.of_int @@ String.length msg in
  Write.BE.uint64 write len ;
  Write.string write msg

(* Read one line from [client] and respond with "OK". *)
let handle_client flow addr =
  let capture_handle = Capture.create () in
  let stdout_tmp_file_name =
    Filename.temp_file "forester-ocaml-plugin-stdout" ".tmp"
  in
  let stderr_tmp_file_name =
    Filename.temp_file "forester-ocaml-plugin-stderr" ".tmp"
  in

  let () = Toploop.initialize_toplevel_env () in

  let () =
    Topfind.add_predicates ["byte"; "toploop"] ;
    (* Add findlib path so Topfind is available and it won't be
     initialized twice if the user does [#use "topfind"]. *)
    Topdirs.dir_directory (Findlib.package_directory "findlib") ;
    (* Load system init files. *)
    (match
       try Some (Sys.getenv "OCAML_TOPLEVEL_PATH") with Not_found -> None
     with
    | Some dir -> Topdirs.dir_directory dir
    | None -> ()) ;

    Toploop.add_directive
      "require"
      (Toploop.Directive_string (fun str -> REPL.require (REPL.split_words str)))
      { Toploop.section = "forester_ocaml_plugin_server"; doc = "" }
  in
  traceln "Accepted connection from %a" Eio.Net.Sockaddr.pp addr ;
  traceln "Using %s as stdout buffer file" stdout_tmp_file_name ;
  traceln "Using %s as stderr buffer file" stderr_tmp_file_name ;

  let rec loop () =
    let from_client = Read.of_flow flow ~max_size:32768 in
    let input = read_string from_client in
    begin
      Write.with_flow flow @@ fun writer ->
      let { Capture.stdout; stderr; outcome } =
        REPL.execute_phrase capture_handle input
      in
      match outcome with
      | Error msg ->
          let output = Format.asprintf "failed to execute phrase: %s" msg in
          write_string writer "" ;
          write_string writer "" ;
          write_string writer output
      | Ok (_success, output) ->
          write_string writer stdout ;
          write_string writer stderr ;
          write_string writer (Buffer.contents output)
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

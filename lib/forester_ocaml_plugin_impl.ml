open Eio.Std
module T = Forester_core.Types
module V = Forester_core.Value

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

module Read = Eio.Buf_read
module Write = Eio.Buf_write

let traceln fmt = traceln ("ocaml plugin: " ^^ fmt)

let read_string =
  Read.bind Read.BE.uint64 @@ fun size -> Read.take (Int64.to_int size)

let write_string write msg =
  let len = Int64.of_int @@ String.length msg in
  Write.BE.uint64 write len ;
  Write.string write msg

let plugin : Forester_compiler.Plugin.plugin =
 fun env ->
  let net = Eio.Stdenv.net env in
  (module struct
    let step_arity = 1

    let step sw : Forester_compiler.Plugin.step =
     fun (args : V.t list) ->
      match args with
      | [V.Content (T.Content [T.Text text])] -> (
          traceln "@[<v 2>%s@]" text ;
          try
            traceln "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr ;
            let flow = Eio.Net.connect ~sw net addr in
            Write.with_flow flow @@ fun to_server ->
            write_string to_server text ;
            let from_server = Read.of_flow flow ~max_size:32768 in
            let captured_stdout = read_string from_server in
            let output = read_string from_server in
            Result.ok
              (V.Content (T.Content [T.Text captured_stdout; T.Text output]))
          with End_of_file ->
            traceln "End_of_file caught - continuing" ;
            Result.ok (V.Content (T.Content [T.Text "End_of_file"])))
      | _ -> Result.error ""

    let scope : type a. (Forester_compiler.Plugin.step -> a) -> a =
     fun f ->
      try Switch.run ~name:"client" @@ fun sw -> f (step sw)
      with End_of_file ->
        traceln "End_of_file caught - continuing" ;
        Forester_core.(
          Reporter.fatal Reporter.Message.Plugin_step_error "End_of_file")
  end)

let () = Forester_compiler.Plugin.register "ocaml" plugin

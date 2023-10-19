open Riot

module Base : Cohttp.Client.BASE = struct
  type 'a io = 'a
  type body = Bigstringaf.t
  type 'a with_context = 'a

  let map_context v f net ~sw = f (v net ~sw)

  let call ?headers ?body ?(chunked = false) meth uri =
    let addr = Net.Addr.of_uri uri in
    let socket = Socket.connect addr in

    let body_length =
      if chunked then None
      else
        match body with
        | None -> Some 0L
        | Some (Eio.Resource.T (body, ops)) ->
            let module X = (val Eio.Resource.get ops Eio.Flow.Pi.Source) in
            List.find_map
              (function
                | Body.String m -> Some (String.length (m body) |> Int64.of_int)
                | _ -> None)
              X.read_methods
    in
    let request =
      Cohttp.Request.make_for_client ?headers
        ~chunked:(Option.is_none body_length)
        ?body_length meth uri
    in

    Eio.Buf_write.with_flow socket @@ fun output ->
    let () =
      Eio.Fiber.fork ~sw @@ fun () ->
      Io.Request.write
        (fun writer ->
          match body with
          | None -> ()
          | Some body -> flow_to_writer body writer Io.Request.write_body)
        request output
    in
    let input = Eio.Buf_read.of_flow ~max_size:max_int socket in
    match Io.Response.read input with
    | `Eof -> failwith "connection closed by peer"
    | `Invalid reason -> failwith reason
    | `Ok response -> (
        match Cohttp.Response.has_body response with
        | `No -> (response, Eio.Flow.string_source "")
        | `Yes | `Unknown ->
            let body =
              let reader = Io.Response.make_body_reader response input in
              flow_of_reader (fun () -> Io.Response.read_body_chunk reader)
            in
            (response, body))
end

module IO: Cohttp.S.IO = struct end


module Client = Cohttp.Client.Make (Base) (IO)

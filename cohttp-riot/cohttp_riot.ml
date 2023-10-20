open Riot

module IO : Cohttp.S.IO = struct
  type 'a t = 'a

  let ( >>= ) v f = f v
  let return v = v

end

module Request = Cohttp.Request.Private.Make(IO)
module Response = Cohttp.Request.Private.Make(IO)

module Flow = struct
  type Riot.Message.t += Wakeup_reader | Wakeup_writer
  type read_flow = [ `Read | `Close | `Yield ]

  type write_flow =
    [ `Close of int | `Write of Bigstringaf.t Faraday.iovec list | `Yield ]

  type t = {
    next_read_operation : unit -> read_flow;
    next_write_operation : unit -> write_flow;
    read : buf:Bigstringaf.t -> len:int -> unit;
    read_eof : buf:Bigstringaf.t -> len:int -> unit;
    report_write_result : [ `Ok of int | `Closed ] -> unit;
    yield_reader : (unit -> unit) -> unit;
    yield_writer : (unit -> unit) -> unit;
  }

  (** Write flow. Drives http/af to write to a Unix socket. *)
  let rec write (conn : Net.stream_socket) flow =
    match flow.next_write_operation () with
    | `Write io_vecs -> do_write conn flow io_vecs
    | `Close len -> do_close_write conn flow len
    | `Yield -> do_yield conn flow

  and do_yield conn flow =
    debug (fun f -> f "yield writer");
    let writer_pid = self () in
    flow.yield_writer (fun () ->
        send writer_pid Wakeup_writer;
        yield ());
    let Wakeup_writer = receive () in
    trace (fun f -> f "resumed writing");
    write conn flow

  and do_write conn flow io_vecs =
    debug (fun f -> f "do write");
    let rec write_all iovs total =
      match iovs with
      | [] -> `Ok total
      | Faraday.{ buffer; off; len } :: iovs -> (
          let data = Bigstringaf.sub buffer ~off ~len in
          match Socket.send data conn with
          | Error _ -> failwith "something went wrong"
          | Ok len ->
              debug (fun f ->
                  f "wrote %d bytes: %s" len (Bigstringaf.to_string data));
              write_all iovs (total + len))
    in
    let result = try write_all io_vecs 0 with End_of_file -> `Closed in
    flow.report_write_result result;
    write conn flow

  and do_close_write conn _flow _len =
    debug (fun f -> f "writer: closing %a" Pid.pp (self ()));
    Socket.close conn

  (** Read flow. Drives http/af to read from a Unix socket. *)
  let rec read (conn : Net.stream_socket) flow =
    match flow.next_read_operation () with
    | `Read -> do_read conn flow
    | `Close -> do_close conn flow
    | `Yield -> do_yield conn flow

  and do_yield conn flow =
    debug (fun f -> f "yield reader");
    let reader_pid = self () in
    flow.yield_reader (fun () ->
        send reader_pid Wakeup_reader;
        yield ());
    let Wakeup_reader = receive () in
    trace (fun f -> f "resumed reading");
    read conn flow

  and do_read conn flow =
    debug (fun f -> f "do read");
    match Socket.receive ~len:128 conn with
    | Ok data when Bigstringaf.length data = 0 ->
        flow.read_eof ~buf:Bigstringaf.empty ~len:0;
        read conn flow
    | Ok buf ->
        flow.read ~buf ~len:(Bigstringaf.length buf);
        read conn flow
    | Error `Closed ->
        error (fun f -> f "connection unexpectedly closed");
        do_close conn flow
    | Error `Timeout ->
        error (fun f -> f "timeout");
        do_close conn flow
    | Error (`Unix_error err) ->
        error (fun f -> f "unix error: %s" (Unix.error_message err));
        do_close conn flow

  and do_close conn _flow =
    debug (fun f -> f "reader: closing %a" Pid.pp (self ()));
    Socket.close conn
end

module Base : Cohttp.Client.BASE = struct
  type 'a io = 'a
  type body = Bigstringaf.t
  type 'a with_context = 'a

  let map_context x = x

  type Message.t += Finished of Bigstringaf.t

  let call ?headers ?body ?(_chunked = false) meth uri =
    let (Some addr) = Net.Addr.of_uri uri in
    let (Ok socket) = Socket.connect addr in

    let body_length =
      Option.map (fun body -> Bigstringaf.length body |> Int64.of_int) body
    in

    let request =
      Cohttp.Request.make_for_client ?headers
        ~chunked:(Option.is_none body_length)
        ?body_length meth uri
    in

    let write_flow = spawn_link (fun () -> Flow.write socket request ) in
    let this = self () in
    let read_flow = spawn_link (fun () -> Flow.read socket this ) in
    wait_pids [ write_flow; read_flow ]
end
module Client = Cohttp.Client.Make (Base) (IO)

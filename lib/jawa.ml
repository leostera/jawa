open Riot

module Scraper = struct
  type 'data scrape_result =
    | Done of { next_requests : string list; data : 'data }
    | Retry
    | Abort

  type 'data on_page = Response.t -> 'data scrape_result
  type t = Scraper : { name : string; handle_response : 'data on_page } -> t

  let handle_response t res = t.handle_response res
  let make ~name handle_response = Scraper { name; handle_response }
end

module Runner = struct
  type t = { scraper : Scraper.t; request_queue : string Queue.t }

  let handle_request t request =
    match fetch request with
    | Err _ -> ()
    | Ok response -> (
        match Scraper.handle_response t.scraper response with
        | Done _ | Retry | Abort -> ())

  let rec loop t =
    match Queue.pop t.request_queue with
    | Some request -> handle_request t request
    | None ->
        ();
        loop t

  let start_link (t : t) =
    let pid = spawn_link (fun () -> loop t) in
    Ok pid

  let child_spec t = Supervisor.child_spec ~start_link t

  module Sup = struct
    type args = { runner_limit : int; runner : t }

    let start_link args =
      let child_specs =
        List.init args.runner_limit (fun _idx -> child_spec args.runner)
      in
      Supervisor.start_link ~strategy:One_for_one ~child_specs ()

    let child_spec args = Supervisor.child_spec ~start_link args
  end
end

let start_link ~scrapers =
  let request_queue = Riot.Queue.create () in

  let scrapers =
    List.map
      (fun scraper ->
        let args =
          Runner.Sup.
            { runner_limit = 10; runner = Runner.{ scraper; request_queue } }
        in
        Runner.Sup.child_spec args)
      scrapers
  in

  let child_specs = scrapers in

  Supervisor.start_link ~child_specs ()

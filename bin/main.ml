[@@@warning "-8"]

open Riot

let main () =
  let profile_scraper =
    Jawa.Scraper.make ~name:"profile_scraper" @@ fun _response ->
    Done { data = [ 12 ]; next_requests = [] }
  in

  let feed_scraper =
    Jawa.Scraper.make ~name:"profile_scraper" @@ fun _response ->
    Done { data = [ true ]; next_requests = [] }
  in

  let (Ok pid) = Jawa.start_link ~scrapers:[ profile_scraper; feed_scraper ] in

  wait_pids [ pid ]

let () = Riot.run @@ main

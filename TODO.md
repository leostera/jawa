webscraper:

queues - a kind of scraper

linkedin:
- search scraper
- profile scraper
- 

```ocaml
let profile_queue = Jawa.Queue.make_name "profile_queue" in
let search_queue = Jawa.Queue.make_name "search_queue" in

let scraper = Jawa.Scraper.make ~on_page:(fun response ->
    let Ok document = Lambdasoup.parse response.body in
    ...
    Ok {
        items: <stuff>;
        new_requests: (url*profile_queue) list;
    }
)

let profile_queue = Jawa.Queue.make ~name:profile_queue ~scraper in

let search_queue = Jawa.Queue.make ~scraper in

in

Jawa.start_link ~start_url:"lnkedin.com/leostera" ~queues:[search_scraper;profile_scraper]
```

url
-> 200
  -> 200 processes

## How does crawly work?

Crawly: Application
Crawly_RequestsStorage_WorkerSup
      --> Crawly_RequestsStorage_Worker
Crawly_DataStorage_WorkerSup
      --> Crawly_DataStorage_Worker
Crawly_Engine (gen_server)
Crawly_EngineSup (dynamic supervisor)
   --> Crawly_ManagerSup (supervisors)
      --> Crawly_Manager 
         --> Crawly_RequestsStorage_Worker
         --> Crawly_DataStorage_Worker

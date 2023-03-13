open Cohttp_lwt_unix
open Cohttp
open Yojson
open Lwt
open Unit

let url = Uri.of_string "https://api.example.com/data"

let calltest =
  "https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/2023-01-09/2023-01-09?apiKey=jn_NAmtAD16hk6azpunzVK1TEvKiu5vy"

let headers =
  Header.init_with "Authorization" "Bearer jn_NAmtAD16hk6azpunzVK1TEvKiu5vy"

let request =
  Client.get ~headers url print_endline
    "Put the ticker symbol in of the stock you would like to view."

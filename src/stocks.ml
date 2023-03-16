open Lwt
open Cohttp_lwt_unix
open Cohttp
open Yojson
open Unit
open Yojson.Basic.Util

let base_url = "https://api.polygon.io"

(*Personal key for our team. Max calls per min = 40*)
let api_key = "jn_NAmtAD16hk6azpunzVK1TEvKiu5vy"

(*[get_ticker_price] grabs symbol [ticker] and retrieves the pricing from
  yesterday*)
let get_ticker_price ticker =
  (* let endpoint = "/v2/aggs/ticker/" ^ ticker ^
     "/range/1/day/2023-01-09/2023-01-09?adjusted=true&sort=asc&limit=120" and
     query_params = [ ("apiKey", api_key) ] in *)
  let uri =
    (* Uri.with_query' (Uri.of_string (base_url ^ endpoint)) query_params *)
    (* Uri.with_query *)
    Uri.of_string
      "https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/2023-01-09/2023-01-09?adjusted=true&sort=asc&limit=120&apiKey=jn_NAmtAD16hk6azpunzVK1TEvKiu5vy"
  in
  let%lwt response, body = Client.get uri in
  let%lwt body_str = Cohttp_lwt.Body.to_string body in
  let body_json = Yojson.Basic.from_string body_str in
  let ticker_price =
    body_json |> member "results" |> to_list |> List.hd |> member "c"
    |> to_float
  in
  Lwt.return ticker_price

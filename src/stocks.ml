open Lwt
open Cohttp_lwt_unix
open Cohttp
open Yojson
open Unit
open Yojson.Basic.Util
open Unix

let base_url = "https://api.polygon.io"

(*Personal key for our team. Max calls per min = 5*)
let api_key = "jn_NAmtAD16hk6azpunzVK1TEvKiu5vy"

(**Gets the uppercase ticker symbol of the stock the user wants info on.*)
let ticker =
  String.uppercase_ascii
    (print_endline "Enter the ticker symbol of a stock: \n";
     print_string "> ";
     read_line ())

(**Formats the [year] properly because Unix has some interesting formatting for
   years.*)
let proper_year year = string_of_int (year + 1900)

(**If the date field [date] has only one digit, this method places a zero before
   that digit.*)
let proper_date date =
  if date < 10 then "0" ^ string_of_int date else string_of_int date

(**Returns the current date in year-month-date format*)
let get_date =
  let t = Unix.localtime (Unix.time ()) in
  proper_year t.tm_year ^ "-"
  ^ proper_date (t.tm_mon + 1)
  ^ "-"
  ^ proper_date (t.tm_mday - 1)

(*[get_ticker_price] grabs symbol [ticker] and retrieves the closing price from
  yesterday*)
let get_ticker_price ticker =
  let endpoint =
    "/v2/aggs/ticker/" ^ ticker ^ "/range/1/day/" ^ get_date ^ "/" ^ get_date
    ^ "?adjusted=true&sort=asc&limit=120"
  and query_params = [ ("apiKey", api_key) ] in
  let uri =
    Uri.with_query' (Uri.of_string (base_url ^ endpoint)) query_params
  in
  let%lwt response, body = Client.get uri in
  let%lwt body_str = Cohttp_lwt.Body.to_string body in
  let body_json = Yojson.Basic.from_string body_str in
  let view_info =
    body_json |> member "results" |> to_list |> List.hd |> member "c"
    |> to_float
  in
  Lwt.return view_info

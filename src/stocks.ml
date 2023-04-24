open Lwt
open Cohttp_lwt_unix
open Cohttp
open Yojson
open Unit
open Yojson.Basic.Util
open CalendarLib

exception NoSuchStock of string
(** Raised when this ticker symbol does not exist. *)

(** - Returns: [get_ticker_price ticker] grabs the symbol [ticker] and retrieves
      the closing price from yesterday.
    - Raise: exception [NoSuchStock] if ticker symbol does not exist. *)
let base_url = "https://api.polygon.io"

(** Personal key for our team. Max calls per min = 5*)
let api_key = "C6yMimbn5IYvWAeJHniypzmv7eROtIpU"

(** Gets the uppercase ticker symbol of the stock the user wants info on.*)
let ticker =
  String.uppercase_ascii
    (print_endline "\nEnter the ticker symbol of a stock: \n";
     print_string "> ";
     read_line ())

(** - Returns: [proper_year year] the stringified year of the time field [year].
    - Example: [proper_year 123] is "2023". *)
let proper_year year = string_of_int (year + 1900)

(** - Returns: [proper_date date] the stringified integer which always has two
      digits [date].
    - Requires: [date] is an integer from 0 to 31.
    - Example: [proper_date 8] is "08". *)
let proper_date date =
  if date < 10 then "0" ^ string_of_int date else string_of_int date

open CalendarLib
(** - Returns: [get_date] the current date in year-month-day format
    - Example: [get_date] today is 2023-03-19. *)

let get_date =
  let t = Unix.localtime (Unix.time ()) in
  let day_of_week = t.tm_wday in
  let hour = t.tm_hour in
  let is_past_market_close = hour >= 16 in
  let days_to_subtract =
    match (day_of_week, is_past_market_close) with
    | 0, true -> 3 (* Sunday after 4 PM *)
    | 0, false -> 2 (* Sunday before 4 PM *)
    | 6, _ -> 1 (* Saturday *)
    | _, true -> 1 (* Monday-Friday after 4 PM *)
    | _, false -> 0 (* Monday-Friday before 4 PM *)
  in
  let date = CalendarLib.Calendar.from_unixfloat (Unix.time ()) in
  let days_period = CalendarLib.Calendar.Period.day (-days_to_subtract) in
  let new_date = CalendarLib.Calendar.add date days_period in
  let year = CalendarLib.Calendar.year new_date in
  let month = CalendarLib.Calendar.month new_date in
  let day = CalendarLib.Calendar.day_of_month new_date in
  Printf.sprintf "%04d-%02d-%02d" year (Date.int_of_month month) day

let get_ticker_price ticker =
  try
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
  with exc -> raise (NoSuchStock ticker)

open Yojson
open Yojson.Basic.Util
open Unix
open CalendarLib

exception NoSuchOption of string

let base_url = "https://api.polygon.io"
let api_key = "jn_NAmtAD16hk6azpunzVK1TEvKiu5vy"

let get_option_contract ticker =
  try
    let endpoint =
      "/v3/reference/options/contracts/" ^ ticker ^ "?apiKey=" ^ api_key
    in
    let uri =
      Uri.with_query'
        (Uri.of_string (base_url ^ endpoint))
        [ ("apiKey", api_key) ]
    in
    let url = Uri.to_string uri in
    let cmd = "curl -s '" ^ url ^ "'" in
    let ic = open_process_in cmd in
    let body_str = input_line ic in
    let body_json = Yojson.Basic.from_string body_str in
    let contract = body_json |> member "results" in
    let symbol = contract |> member "ticker" |> to_string in
    let expiration_date = contract |> member "expiration_date" |> to_string in
    let strike_price = contract |> member "strike_price" |> to_float in
    ignore (close_process_in ic);
    (symbol, expiration_date, strike_price)
  with exc -> raise (NoSuchOption ticker)

let erf x =
  let a = 0.147 in
  let x_sq = x *. x in
  let p = 4. /. (Float.pi +. (a *. x_sq)) in
  let y = x_sq *. p in
  let erf = x *. exp (-.x_sq *. y) /. (1. +. (x_sq *. y)) in
  erf

let norm_cdf x =
  let sign = if x < 0.0 then -1.0 else 1.0 in
  let y = 1.0 +. erf (sign *. x /. sqrt 2.0) in
  0.5 *. y

let black_scholes_call s x t r sigma =
  let d1 =
    (log (s /. x) +. ((r +. ((sigma ** 2.0) /. 2.0)) *. t)) /. (sigma *. sqrt t)
  and d2 =
    (log (s /. x) +. ((r -. ((sigma ** 2.0) /. 2.0)) *. t)) /. (sigma *. sqrt t)
  in
  let nd1 = norm_cdf d1 and nd2 = norm_cdf d2 in
  (s *. nd1) -. (x *. exp (-.r *. t) *. nd2)

let black_scholes_put s x t r sigma =
  let call_price = black_scholes_call s x t r sigma in
  call_price -. s +. (x *. exp (-.r *. t))

let compute_greeks s x t r sigma =
  let d1 =
    (log (s /. x) +. ((r +. ((sigma ** 2.0) /. 2.0)) *. t)) /. (sigma *. sqrt t)
  and d2 =
    (log (s /. x) +. ((r -. ((sigma ** 2.0) /. 2.0)) *. t)) /. (sigma *. sqrt t)
  in

  let delta = norm_cdf d1
  and gamma =
    exp (-.(d1 ** 2.0) /. 2.0) /. (s *. sigma *. sqrt (2.0 *. Float.pi *. t))
  and vega = s *. sqrt t *. exp (-.(d1 ** 2.0) /. 2.0) /. sqrt (2.0 *. Float.pi)
  and theta =
    -.s *. sigma
    *. exp (-.(d1 ** 2.0) /. 2.0)
    /. (2.0 *. sqrt (2.0 *. Float.pi *. t))
    -. (r *. x *. exp (-.r *. t) *. norm_cdf d2)
  and rho = t *. x *. exp (-.r *. t) *. norm_cdf d2 in

  (delta, gamma, vega, theta, rho)

let expiration_date_from_ticker ticker =
  let date_str = String.sub ticker 4 6 in
  let year = int_of_string (String.sub date_str 0 2) + 2000 in
  let month = int_of_string (String.sub date_str 2 2) in
  let day = int_of_string (String.sub date_str 4 2) in
  Calendar.Date.make year month day

let time_to_expiration_years ticker =
  let expiration_date = expiration_date_from_ticker ticker in
  let today = Calendar.Date.today () in
  let days_to_expiration = Calendar.Date.sub expiration_date today in
  let days_diff = Calendar.Date.Period.nb_days days_to_expiration in
  let days_in_year = 365.0 in
  float days_diff /. days_in_year

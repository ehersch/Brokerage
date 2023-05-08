open Unix
open Stocks

exception Broke
exception OverMemory

type stock = {
  ticker : string;
  price : float;
}

type account = {
  stock_balance : float;
  cash_balance : float;
  portfolio : (stock * float) list;
  transaction_log : transaction list;
}

and transaction = {
  time : string;
  type_of_transaction : string;
  share : float;
  stock : stock;
}

(** Converts the time into an easy-to-read month/day/year string format*)
let convert_unix_time (t : float) : string =
  let tm = localtime t in
  let day = string_of_int tm.tm_mday in
  let month = string_of_int (tm.tm_mon + 1) in
  let year = string_of_int (tm.tm_year + 1900) in
  day ^ "/" ^ month ^ "/" ^ year

let withdraw (amt : float) (acc : account) =
  let b1 = acc.stock_balance in
  let b2 = acc.cash_balance -. amt in
  if amt > acc.cash_balance then raise Broke
  else
    {
      stock_balance = b1;
      cash_balance = b2;
      portfolio = acc.portfolio;
      transaction_log = acc.transaction_log;
    }

let deposit (amt : float) (acc : account) =
  let b1 = acc.stock_balance in
  let b2 = acc.cash_balance +. amt in
  {
    stock_balance = b1;
    cash_balance = b2;
    portfolio = acc.portfolio;
    transaction_log = acc.transaction_log;
  }

let stock_to_string_pair stk = (stk.ticker, string_of_float stk.price)

let transaction_to_string_quint trans =
  let pp = stock_to_string_pair trans.stock in
  match pp with
  | a, b ->
      let tick = a in
      let pr = b in
      ( trans.time,
        trans.type_of_transaction,
        string_of_float trans.share,
        tick,
        pr )

let balance acc = string_of_float (acc.stock_balance +. acc.cash_balance)
let stock_balance acc = string_of_float acc.stock_balance

let only_stocks (acc : account) =
  List.map (fun ({ ticker; price }, _) -> ticker) acc.portfolio

let rec portfolio (port : (stock * float) list) =
  match port with
  | [] -> []
  | (h, q) :: t -> (h.ticker, h.price, q) :: portfolio t

let port_to_string port =
  let full_port = portfolio port in
  let rec to_string = function
    | [] -> ""
    | (ticker, price, q) :: t ->
        "(ticker = " ^ ticker ^ ", " ^ "average price = "
        ^ string_of_float price ^ ", " ^ "number of shares = "
        ^ string_of_float q ^ ") " ^ "\n" ^ to_string t
  in
  let list_text = to_string full_port in
  "{ " ^ list_text ^ "}"

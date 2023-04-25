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
}

type transaction = {
  time : float;
  type_of_transaction : string;
  change : float;
}

let withdraw (amt : float) (acc : account) =
  if amt > acc.cash_balance then raise Broke
  else
    {
      stock_balance = acc.stock_balance;
      cash_balance = acc.cash_balance -. amt;
      portfolio = acc.portfolio;
    }

let deposit (amt : float) (acc : account) =
  {
    stock_balance = acc.stock_balance;
    cash_balance = acc.cash_balance +. amt;
    portfolio = acc.portfolio;
  }

let balance acc = string_of_float (acc.stock_balance +. acc.cash_balance)

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

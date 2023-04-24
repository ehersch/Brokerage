open Stocks

exception Broke
exception OverMemory

type account = {
  stock_balance : float;
  cash_balance : float;
  portfolio : (string * float) list;
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
      stock_balance = acc.stock_balance -. amt;
      cash_balance = acc.cash_balance -. amt;
      portfolio = acc.portfolio;
    }

let deposit (amt : float) (acc : account) =
  {
    acc with
    cash_balance = acc.cash_balance +. amt;
    stock_balance = acc.stock_balance +. amt;
  }

let balance acc = string_of_float acc.stock_balance

let rec portfolio (port : (string * float) list) =
  match port with
  | [] -> []
<<<<<<< HEAD
  | (h, q) :: t -> (h, q) :: portfolio t
=======
  | (h, q) :: t -> (h.ticker, string_of_float h.price, q) :: portfolio t
>>>>>>> refs/remotes/origin/main

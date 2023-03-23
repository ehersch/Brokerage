open Stocks

exception Broke
(** Raised when attempting to withdraw more money than available.*)

exception OverMemory
(** Raised when a user attempts to deposit money into an account resulting in an
    integer the brokerage cannot handle: 1073741823 which leads to overmemory *)

type stock = {
  ticker : string;
  price : float;
}
(** Type [stock] contains the ticker symbol [ticker] and current price [price]. *)

type account = {
  stock_balance : float;
  cash_balance : float;
  portfolio : (stock * float) list;
}
(** Type [account] contains the floats [stock_balance] and [cash_balance] and
    the set-like-list of stocks [portfolio] and how many shares owned. *)

type transaction = {
  time : float;
  type_of_transaction : string;
  change : float;
}

(* Returns: [account amt acc] a record with updated [stock_balance] and
   [cash_balance] after withdrawing the specified amount and requires: - [amt]
   is a non-negative float representing the amount to be withdrawn. - [acc]: A
   valid account record with at least the same amount of cash_balance as the
   requested withdrawal amount. - Raises: [Broke] when the requested withdrawal
   amount is greater than the available cash_balance in the account. - Example:
   given an account with [cash_balance] = 1000.0, [stock_balance] = 500.0, and
   portfolio, withdrawing 200.0 will result in an account with cash_balance =
   800.0 and stock_balance = 300.0. *)
let withdraw (amt : float) (acc : account) =
  if amt > acc.cash_balance then raise Broke
  else
    {
      stock_balance = acc.stock_balance -. amt;
      cash_balance = acc.cash_balance -. amt;
      portfolio = acc.portfolio;
    }

(* Returns: [deposit amt acc] a new account record with an updated cash_balance
   after depositing the specified amount and requires: - [amt]: A non-negative
   float representing the amount to be deposited. - [acc]: A valid account
   record. - The total account stock_balance cannot be over 1073741823 - Raises:
   OverMemory - Examples: Given an account with cash_balance = 1000.0,
   depositing 200.0 will result in an account with cash_balance = 1200.0. If the
   deposit amount is 0.0, the function will return an account with the same
   cash_balance. *)

let deposit (amt : float) (acc : account) =
  {
    acc with
    cash_balance = acc.cash_balance +. amt;
    stock_balance = acc.stock_balance +. amt;
  }

(** [balance acc] is the cash and stock balance of [acc] in a float array which allows for constant
    time extraction of balances when parsing -bal in the main UI. 
    Example: balance {stock_balance = 500.0; cash_balance = 500.0 ; portfolio = []} is 
    "500.0"*)
let balance acc = string_of_float acc.stock_balance

(** [portfolio port] is a string list list of each ticker with its associated
    average price and quantity of shares from account's portfolio. Example:
    portfolio
    [{ticker = AAPL; price = 125.0} 3.0); ({ticker = META; price = 175.0}, 2.0)]
    is[\["AAPL";"125.0";"3.0"\];\["META";"175.0";"2.0"\]] *)
let rec portfolio (port : (stock * float) list) =
  match port with
  | [] -> []
  | (h, q) :: t -> (h.ticker, h.price, q) :: portfolio t

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
(** Type [transaction] contains the floats [time] and [change] and the string
    [type_of_transaction]. *)

val withdraw : float -> account -> account
(** [withdraw amt acc] is a record with updated [stock_balance] and
    [cash_balance] after withdrawing the specified amount [amt]. Raises [Broke]
    if the requested withdrawal amount is greater than the available
    cash_balance in the account. Example: given an account with [cash_balance] =
    1000.0, [stock_balance] = 500.0, and portfolio, withdrawing 200.0 will
    result in an account with cash_balance = 800.0 and stock_balance = 300.0.*)

val deposit : float -> account -> account
(** [deposit amt acc] is a new account record with an updated cash_balance after
    depositing the specified amount [amt]. Raises [OverMemory] if the total
    account stock_balance would be over 1073741823. Examples: Given an account
    with cash_balance = 1000.0, depositing 200.0 will result in an account with
    cash_balance = 1200.0. If the deposit amount is 0.0, the function will
    return an account with the same cash_balance.*)

val balance : account -> string
(** [balance acc] is the cash and stock balance of [acc] in a string form.
    Example: balance \{stock_balance = 500.0; cash_balance = 500.0 ; portfolio =
    []\} is "500.0"*)

val portfolio : (stock * float) list -> (string * float * float) list
(** [portfolio port] is a list of each ticker with its associated average price
    and quantity of shares from the account's portfolio. Example: portfolio
    \[\{ticker = AAPL; price = 125.0\} 3.0); (\{ticker = META; price = 175.0\},
    2.0)\] is \[\["AAPL";"125.0";"3.0"\];\["META";"175.0";"2.0"\]\]*)

val port_to_string : (stock * float) list -> string
(** [port_to_string port] is a string representation of an account's portfolio. Example: portfolio 
[{ticker = AAPL; price = 125.0}, 3.0 ); {ticker = META; price = 175.0}, 2.0] is 
{(AAPL,125.0,3.0), (META,175.0,2.0)} *)

val only_stocks : account -> string list

(** An account to make transactions with.*)

exception Broke
(** Raised when attempting to withdraw more money than available.*)

exception OverMemory
(** Raised when a user attempts to deposit money into an account resulting in an
    integer the brokerage cannot handle: 1073741823 which leads to overmemory *)

exception StockNotFound
(** Raised when a user attempts to remove a ticker from their watchlist that is
    not in their watchlist *)

type stock = {
  ticker : string;
  price : float;
}
(** Type [stock] contains the ticker symbol [ticker] and current price [price]. *)

type account = {
  stock_balance : float;
  cash_balance : float;
  portfolio : (stock * float) list;
  transaction_log : transaction list;
  watchlist : stock list;
}

(** Type [account] contains the floats [stock_balance], [cash_balance] and the
    set-like-list of stocks [portfolio] and how many shares owned. It
    additionally contains a list of transactions anf balances named
    [transaction_log] and [balance_log], respectively*)

and transaction = {
  time : string;
  type_of_transaction : string;
  share : float;
  stock : stock;
}

(**Type [transaction] any transaction has a [time], [type_of_transaction],
   [share], and [stock] field. *)

(** Type [transaction] contains the floats [time] and [change] and the string
    [type_of_transaction]. *)

val stock_to_string_pair : stock -> string * string

(** [stock_to_string_pair stk] returns the tuple of strings where the first
    element is the ticker and the second element is the price*)

val transaction_to_string_quint :
  transaction -> string * string * string * string * string

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

val stock_balance : account -> string
(** [stock_balance acc] is the stock balance of the account in string form. *)

val portfolio : (stock * float) list -> (string * float * float) list
(** [portfolio port] is a list of each ticker with its associated average price
    and quantity of shares from the account's portfolio. Example: portfolio
    \[\{ticker = AAPL; price = 125.0\} 3.0); (\{ticker = META; price = 175.0\},
    2.0)\] is \[\["AAPL";"125.0";"3.0"\];\["META";"175.0";"2.0"\]\]*)

val port_to_string : (stock * float) list -> string
(** [port_to_string port] is a string representation of an account's portfolio.
    Example: portfolio
    [{ticker = AAPL; price = 125.0}, 3.0 ); {ticker = META; price = 175.0}, 2.0]
    is \{(AAPL,125.0,3.0), (META,175.0,2.0)\} *)

val only_stocks : account -> string list

(** Returns only the stocks in an account. *)
val add_watchlist : string -> account -> account
(** [add_watchlist ticker acc] adds the selected ticker and its current price
    into the account's watchlist or doesn't change the account's watchlist if
    the ticker isn't found. Example: adding MSFT into a watchlist of
    [{ticker = AAPL; price = 125.0}; {ticker = META; price = 175.0}] is
    [{ticker = AAPL; price = 125.0}; {ticker = META; price = 175.0 }; {ticker = MSFT; price = 150.0}] *)

val remove_watchlist : string -> account -> account
(** [remove_watchlist ticker acc] removes the selected ticker from the account's
    watchlist or raises StockNotFound if the selected ticker is not in the
    account's watchlist. Example: removing AAPL from a watchlist of
    [{ticker = AAPL; price = 125.0}; {ticker = META; price = 175.0}] is
    [{ticker = META; price = 175.0 }] *)

val watch_to_string : stock list -> string
(** [watch_to_string watchlist] is a string representation of an account's
    watchlist. Example: watchlist
    [{ticker = AAPL; price = 125.0}; {ticker = META; price = 175.0}] is
    \{(AAPL,125.0), (META,175.0)\}*)

type command =
  | Bal
  | Portfolio
  | Dep of string list
  | Withdraw of string list
  | View of string list
  | Help
  | Quit

exception Empty
exception Invalid

(** [only_spaces list] takes a string list as input and returns a bool of
    whether or not the list contains only empty strings*)
let rec only_spaces list =
  match list with
  | [] -> true
  | h :: t -> h = "" && only_spaces t

(** [real_words list acc] returns a list of strings that are not empty strings
    in list. If there are no non-empty strings in the list, returns the empty
    list*)
let rec real_words list acc =
  match list with
  | [] -> List.rev acc
  | h :: t ->
      if h <> "" then
        let new_acc = h :: acc in
        real_words t new_acc
      else real_words t acc

(** [get_first list] returns the the tail of the list where the head is "go" or
    "quit". If the list is only empty strings then returns the original list. If
    there is no instance of "go" or "quit found in the list, returns a string
    list that will raise malformed in parse str"*)
let rec get_first list =
  if only_spaces list then list
  else
    match list with
    | [] -> []
    | h :: t ->
        if
          h = "-bal" || h = "-portfolio" || h = "-dep" || h = "-withdraw"
          || h = "-view" || h = "-help" || h = "-quit"
        then list
        else if h = "" then get_first t
        else [ "quit"; "invalid" ]

(** [command_help str output] returns what command type the given string is if
    it requires extra parameters given the first word of the user input as str
    and the rest of the words as output *)
let command_help str output =
  match str with
  | "-bal" -> Bal
  | "-portfolio" -> Portfolio
  | "-dep" -> Dep output
  | "-withdraw" -> Withdraw output
  | "-view" -> View output
  | _ -> raise Invalid

(** [comp_mem str] checks if the given first word of a user input is a valid
    command type listed in -help that needs extra parameters. Examples: -dep
    500. or -withdraw 200.*)
let comp_mem str =
  str = "-bal" || str = "-dep" || str = "-withdraw" || str = "-portfolio"
  || str = "-view"

(** [com_mem str] checks whether the given first word of a user input is a
    command type listed in -help that does not need extra parameters. Examples:
    \-bal or -portfolio *)
let com_mem str =
  str = "-bal" || str = "-portfolio" || str = "-quit" || str = "-help"

(** [parse str] parses a user's input into a [command]. The first word that is
    not an empty string becomes the command type listed in -help and the rest of
    the words if any become the phrase. Example:

    - parse " -dep 500." is [Dep \["500."\]]
    - parse "-bal" is [Bal] *)
let parse str =
  let words = String.split_on_char ' ' str in
  let actual_words = get_first words in
  match actual_words with
  | h :: t ->
      if only_spaces actual_words then raise Empty
      else if com_mem h then
        if only_spaces t then command_help h [] else raise Invalid
      else if comp_mem h then
        let rest = real_words t [] in
        if rest = [] then raise Empty else command_help h rest
      else raise Invalid
  | _ -> raise Invalid

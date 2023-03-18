type command =
  | Bal of string list
  | Portfolio of string list
  | Dep of string list
  | Withdraw of string list
  | View of string list
  | Help
  | Quit

exception Empty
exception Invalid

(** only_spaces [list] takes a string list as input and returns a bool of
    whether or not the list contains only empty strings*)
let rec only_spaces list =
  match list with
  | [] -> true
  | h :: t -> h = "" && only_spaces t

(** real_words [list] [acc] returns a list of strings that are not empty strings
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

let parse str =
  let words = String.split_on_char ' ' str in
  let actual_words = get_first words in
  match actual_words with
  | [ "-quit" ] -> Quit
  | "-quit" :: t -> if only_spaces t then Quit else raise Invalid
  | [ "-help" ] -> Help
  | "-help" :: t -> if only_spaces t then Help else raise Invalid
  | _ -> raise Invalid
  | _ :: t -> if only_spaces words then raise Empty else raise Invalid
  | [] -> raise Empty

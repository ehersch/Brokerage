type command =
  | Bal
  | Portfolio
  | Dep of float
  | Withdraw of float
  (* | View of string *)
  (*Feature disabled temporarily for UI*)
  | Quit
  | Help

exception Invalid
exception Empty

(** [parse str] parses a user's input into a [command]. The first word that is
    not an empty string becomes the command type listed in -help and the rest of
    the words if any become the phrase. Raises [Invalid] when input is not in
      the format of -command [number if required]. Example:

    - parse " -dep 500." is [Dep 500.0]]
    - parse "-bal" is [Bal] *)
let parse str =
  let split_str =
    List.filter
      (fun x -> String.length x > 0)
      (List.map String.trim (String.split_on_char ' ' str))
  in
  match split_str with
  | [ "-bal" ] -> Bal
  | [ "-portfolio" ] -> Portfolio
  | "-dep" :: [ amt ] -> Dep (float_of_string amt)
  | "-withdraw" :: [ amt ] ->
      Withdraw (float_of_string amt)
      (* | "-view" :: [ ticker ] -> View ticker *)
      (*Feature disabled temporarily for UI*)
  | [ "-help" ] -> Help
  | [ "-quit" ] -> Quit
  | [ _ ] -> raise Invalid
  | _ :: tl -> raise Invalid
  | [] -> raise Empty

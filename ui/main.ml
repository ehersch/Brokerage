let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Jame Street.\n";
  print_endline
    "We are the fastest growing trading firm in the industry using\n\
     innovative technology and fast user service";
  print_endline "Are you a returning user or new user?";
  print_string "> ";
  match read_line () with
  | h -> print_endline "Have to implement still"

(* Execute the game engine. *)
let () = main ()

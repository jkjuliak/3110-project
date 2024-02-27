open Crossword
open Read
module WordMap = Map.Make (String)

(**This module contains all the helpers and main functions that support playing
   the Crossword game on the terminal, all modes included*)
module Play = struct
  (**[quit] contains all the interactive prompts shown when the user quits any
     crossword of any difficulty mode. The user is asked to give a rating of the
     game, and asks for extra feedback when the user enters a rating less than
     6. After the rating and the feedback are entered, the game terminates.*)
  let rec quit () =
    print_endline
      "Thanks for playing! Would you like to rate the game? Enter <yes> or <no>";
    match String.trim (read_line ()) with
    | "yes" ->
        print_endline "Great! Please give us a rating from 1 to 10.";
        let rating = String.trim (read_line ()) in
        if int_of_string rating > 5 then begin
          print_endline "I'm glad you enjoyed the game!";
          exit 0
        end
        else begin
          print_endline "Can you provide us with additional feedback?";
          match String.trim (read_line ()) with
          | _ ->
              print_endline "Great, we will keep this suggestion in mind!";
              print_endline "Thanks for playing!";
              exit 0
        end
    | "no" ->
        print_endline "Okay, thanks for playing!";
        exit 0
    | _ ->
        print_endline "Invalid input.";
        quit ()

  (**[check_eq] checks whether all key-value bindings of type string in a list
     are equal to each other. In context, this function is a helper for
     functions handling a correct guess of a crossword word. By comparing the
     key-value bindings of player display, we can tell if the user has guessed
     all the words and therefore make the decision to quit the game
     automatically since the game is over*)
  let rec check_eq (starter : (string * string) list) : bool =
    match starter with
    | [] -> false
    | [ (k, v) ] -> if String.equal k v then true else false
    | (k, v) :: t -> if String.equal k v then check_eq t else false

  (**[check_eq] checks whether a string value [value] already exists as a
     key-value binding of type string in a (string * string) list are equal to
     each other. *)
  let rec check_one value string_list =
    match string_list with
    | [] -> false
    | (k, v) :: tl -> if value = v then true else check_one value tl

  (**[correct_guess] handles correct guesses of words in a crossword of
     [EasyMode]. If all words are guessed, invokes [quit]. Else, prompts user to
     keep guessing the other words.*)
  let correct_guess starter game user =
    if check_one user (Read.EasyMode.bindings_display !starter) then
      print_endline
        "You have already guessed this word. Try guessing another word to \
         continue playing the game."
    else begin
      print_endline "Congrats, this is correct! Here is your updated crossword";
      let new_crossword = Read.EasyMode.update !starter user in
      Read.EasyMode.print_a new_crossword;
      starter := new_crossword;
      let pass = !starter in
      let bound = Read.EasyMode.bindings_display pass in
      if check_eq bound then begin
        print_endline "Congrats, you have completed the crossword! Game Over.";
        quit ()
      end
      else ()
    end

  (**[incorrect_guess_sec] handles an incorrect guess of a word in a crossword
     of [EasyMode], when the user guess a word wrong after the second hint
     offered to them, the vowel hint. Helper method for [vowel]*)
  let rec incorrect_guess_sec counter user starter game =
    counter := !counter + 1;
    print_endline "Incorrect. This word is not in the crossword. Try Again.";
    user := String.trim (read_line ());
    if Read.EasyMode.check game !user then correct_guess starter game !user
    else incorrect_guess_sec counter user starter game

  (**[vowel] reveals the vowel hint on the Crossword board, after the user uses
     their first hint and still guesses the Crossword word wrong.*)
  let vowel counter meaning user starter game =
    print_endline "Revealing vowels of your word. Try Again.";
    print_endline ("Meaning: " ^ meaning);
    let vowels = Read.EasyMode.return_vowel meaning game in
    let new_crossword =
      Read.EasyMode.update_vowel !starter game vowels meaning
    in
    Read.EasyMode.print_a new_crossword;
    starter := new_crossword;
    user := String.trim (read_line ());
    (* print_endline ("User's new guess: " ^ !user); *)
    if Read.EasyMode.check game !user then begin
      (* print_endline "Correct guess!"; *)
      correct_guess starter game !user;
      ()
    end
    else begin
      (* print_endline "Incorrect guess!"; *)
      incorrect_guess_sec counter user starter game
    end

  (**[incorrect_guess_sec] handles a guess of a word in a crossword of
     [EasyMode], after the first hint is revealed to the user. If the user
     guesses the word correctly, handles answer with [correct_guess]. If word is
     guessed incorrectly invokes [vowel] to reveal the second hint, Helper
     method for [hint]*)
  let rec hint_handler counter user starter game =
    user := String.trim (read_line ());
    let meaning = !user in
    let hint = Read.EasyMode.return_hint meaning game in
    if String.compare hint "None" <> 0 then begin
      counter := !counter + 1;
      print_endline
        ("Here is your hint: " ^ hint ^ ". Type in your new guess now!");
      user := String.trim (read_line ());
      if Read.EasyMode.check game !user then begin
        correct_guess starter game !user;
        ()
      end
      else vowel counter meaning user starter game
    end
    else begin
      print_endline
        "Invalid entry. Type your meaning in again, pay attention to spaces.";
      hint_handler counter user starter game
    end

  (**[hint] searches the answer key for the hint that the user is guessing. The
     user is prompted for the specific meaning of the word they are trying to
     guess, as is displayed to them. Uses [EasyMode]'s [return_hint] to return
     the corresponding hint to display to the user*)
  let hint counter user starter game =
    print_endline
      "Which word are you guessing? Type in the meaning of the word you are \
       guessing. Remember that this is case sensitive, no extra spaces before \
       and after your entry";
    user := String.trim (read_line ());
    let meaning = !user in
    let hint = Read.EasyMode.return_hint meaning game in
    if String.compare hint "None" <> 0 then begin
      print_endline
        ("Here is your hint: " ^ hint ^ ". Type in your new guess now!");
      user := String.trim (read_line ());
      if Read.EasyMode.check game !user then begin
        correct_guess starter game !user;
        ()
      end
      else vowel counter meaning user starter game
    end
    else begin
      print_endline
        "Invalid entry. Type your meaning in again, pay attention to spaces.";
      hint_handler counter user starter game
    end

  (**[incorrect_handler] handles invalid inputs to [incorrect_guess], loops
     until yes or no is entered. If the user enters <yes>, invokes [hint] to
     display the first hint. If the user enters <no>, the player can continue to
     guess the word without the hint, or guess another word. Also handles
     invalid inputs. *)
  let rec incorrect_handler counter user starter game =
    user := String.trim (read_line ());
    if String.compare !user "yes" = 0 then hint counter user starter game
    else if String.compare !user "no" = 0 then begin
      print_endline "Try Again"
    end
    else (
      print_endline "Invalid input. Please enter <yes> or <no>.";
      incorrect_handler counter user starter game)

  (**[incorrect_guess] handles the first incorrect guess the user makes. Asks if
     they want to use hints for their guesses or not. If the user enters <yes>,
     invokes [hint] to display the first hint. If the user enters <no>, the
     player can continue to guess the word without the hint, or guess another
     word. Also handles invalid inputs, by invoking [incorrect_handler] *)
  let rec incorrect_guess counter user starter game =
    counter := !counter + 1;
    print_endline
      "Incorrect. This word is not in the crossword. Would you like a hint? \
       Enter <yes> for a hint or <no> to guess another word";
    user := String.trim (read_line ());
    if String.compare !user "yes" = 0 then hint counter user starter game
    else if String.compare !user "no" = 0 then
      incorrect_handler counter user starter game
    else (
      print_endline "Invalid input. Please enter <yes> or <no>.";
      incorrect_handler counter user starter game)

  (**[guess_loop] is the main user input loop for the game. Is the first prompt
     made to the user when the start guessing words or guess a new word. Handles
     correct and incorrect guesses using [correct_guess] and [incorrect_guess].
     Allows user to quit using [quit] if they need to end the game early.*)
  let rec guess_loop game starter counter =
    print_endline
      "Type in a word you think is in the crossword! When you are finished \
       guessing words, type 'quit': ";
    let user = ref (String.trim (read_line ())) in

    if String.compare !user "quit" = 0 then quit ()
    else if Read.EasyMode.check game !user then correct_guess starter game !user
    else if not (Read.EasyMode.check game !user) then
      incorrect_guess counter user starter game
    else ();

    guess_loop game starter counter

  (**[check_easy] Starts a new [EasyMode] game, displays the initial empty
     Crossword game board, corresponding word meanings and invokes [guess_loop]
     to allow the user to start guessing words.*)
  let check_easy (game : EasyMode.t) (player : EasyMode.a) =
    print_endline "Here is the list of hidden words in your crossword: ";
    Read.EasyMode.print_a player;
    print_endline "Here is a list of meanings of these words: ";
    Read.EasyMode.print_t game;

    let starter = ref player in
    let counter = ref 0 in

    guess_loop game starter counter

  (**[set_easy] creates a new [EasyMode] game based on a specific txt file
     [filename], initializes corresponding answer key and player display to be
     used during the rest of the game*)
  let set_easy (filename : string) =
    let game = Read.EasyMode.make_mappings filename in
    let player = Read.EasyMode.initial game in
    check_easy game player

  (**[check_medium] starts a new [MediumMode] game, displays the initial empty
     Crossword game board, corresponding word meanings and invokes inbuilt
     [guess_loop] to allow the user to start guessing words. Here [guess_loop]
     employs a guess limit of 5 guesses, which decrements if the user guesses
     the word incorrectly. The user has the option to use a hint when they
     incorrectly guesses a word once, at the expense of the # guesses
     decrementing by 2 instead of 1. Game quits when all words are guessed or if
     user enters quit. Also handles invalid inputs, looping and prompting user
     to input correctly until they do*)
  let check_medium (game : MediumMode.t) (player : MediumMode.a) =
    print_endline "Here is the list of hidden words in your crossword: ";
    Read.MediumMode.print_a player;
    print_endline "Here is a list of meanings of these words: ";
    Read.MediumMode.print_t game;
    print_endline
      "You will get 5 guesses per word. If you run out of guesses, it is game \
       over.";

    let rec guess_loop remaining_guesses player hinted =
      if remaining_guesses > 0 then begin
        Printf.printf "You have %d guess%s left.\n" remaining_guesses
          (if remaining_guesses > 1 then "es" else "");
        print_endline
          "Type in a word you think is in the crossword! When you are finished \
           guessing words, type 'quit': ";
        let user_1 = ref (String.trim (read_line ())) in

        if String.compare !user_1 "quit" = 0 then quit ()
        else if check_one !user_1 (Read.MediumMode.bindings_display player) then begin
          print_endline
            "You have already guessed this word. Try guessing another word to \
             continue playing the game.";
          guess_loop 5 player false
        end
        else if Read.MediumMode.check game !user_1 then begin
          print_endline
            "Congrats, this is correct! Here is your updated crossword";
          let new_crossword = Read.MediumMode.update player !user_1 in
          Read.MediumMode.print_a new_crossword;
          let pass = new_crossword in
          let bound = Read.MediumMode.bindings_display pass in
          if check_eq bound then begin
            print_endline
              "Congrats, you have completed the crossword! Game Over.";
            quit ()
          end
          else guess_loop 5 new_crossword false
        end
        else if not (Read.MediumMode.check game !user_1) then begin
          print_endline "Incorrect. This word is not in the crossword.";
          if not hinted then begin
            print_endline
              "Would you like a hint? Enter <yes> or <no>. Choosing <yes> will \
               decrease the number of guesses left by 2.";
            user_1 := String.trim (read_line ());
            let rec invalid_handler () =
              match String.trim !user_1 with
              | "yes" ->
                  print_endline
                    "Which word are you guessing? Type in the meaning of the \
                     word you are guessing. Remember that this is case \
                     sensitive, no extra spaces before and after your entry";
                  user_1 := String.trim (read_line ());
                  let meaning = !user_1 in
                  let hint = Read.MediumMode.return_hint meaning game in
                  if String.compare hint "None" <> 0 then begin
                    print_endline
                      ("Here is your hint: " ^ hint
                     ^ ". Type in your new guess now!");
                    guess_loop (remaining_guesses - 2) player true
                  end
                  else begin
                    print_endline "Invalid entry.";
                    guess_loop remaining_guesses player hinted
                  end
              | "no" ->
                  print_endline "Try Again";
                  guess_loop (remaining_guesses - 1) player hinted
              | _ ->
                  print_endline "Invalid input. Please enter <yes> or <no>.";
                  user_1 := String.trim (read_line ());
                  invalid_handler ()
            in
            invalid_handler ()
          end
          else begin
            print_endline "Try Again";
            guess_loop (remaining_guesses - 1) player hinted
          end
        end
      end
      else begin
        print_endline "You've reached the maximum number of guesses. Game Over.";
        quit ()
      end
    in
    guess_loop 5 player false

  (**[set_medium] creates a new [MediumMode] game based on a specific txt file
     [filename], initializes corresponding answer key and player display to be
     used during the rest of the game*)
  let set_medium (filename : string) =
    let game = Read.MediumMode.make_mappings filename in
    let player = Read.MediumMode.initial game in
    check_medium game player

  (**[check_hard] starts a new [HardMode] game, displays the initial empty
     Crossword game board, corresponding word meanings and invokes inbuilt
     [guess_loop] to allow the user to start guessing words. Here [guess_loop]
     employs a guess limit of 3 guesses, which decrements if the user guesses
     the word incorrectly. Unlike [EasyMode] and [MediumMode], the player does
     not have any hints to help them guess the word. Game quits when all words
     are guessed or if user enters quit. Also handles invalid inputs, looping
     and prompting user to input correctly until they do*)
  let check_hard (game : HardMode.t) (player : HardMode.a) =
    print_endline "Here is the list of hidden words in your crossword: ";
    Read.HardMode.print_a player;
    print_endline "";
    print_endline "Here is a list of meanings of these words: ";
    Read.HardMode.print_t game;
    print_endline
      "You will get 3 guesses per word. If you run out of guesses, it is game \
       over";
    let rec guess_loop remaining_guesses player =
      if remaining_guesses > 0 then begin
        Printf.printf "You have %d guess%s left.\n" remaining_guesses
          (if remaining_guesses > 1 then "es" else "");
        print_endline
          "Type in a word you think is in the crossword! To quit the game, \
           type <quit> ";
        let user_guess = ref (String.trim (read_line ())) in

        if String.compare !user_guess "quit" = 0 then quit ()
        else if check_one !user_guess (Read.HardMode.bindings_display player)
        then begin
          print_endline
            "You have already guessed this word. Try guessing another word to \
             continue playing the game.";
          guess_loop 3 player
        end
        else if Read.HardMode.check game !user_guess then begin
          print_endline
            "Congrats, this is correct! Here is your updated crossword";
          let new_crossword = Read.HardMode.update player !user_guess in
          Read.HardMode.print_a new_crossword;
          let pass = new_crossword in
          let bound = Read.HardMode.bindings_display pass in
          if check_eq bound then begin
            print_endline
              "Congrats, you have completed the crossword! Game Over.";
            quit ()
          end
          else guess_loop 3 new_crossword
        end
        else begin
          print_endline "Incorrect. This word is not in the crossword.";
          guess_loop (remaining_guesses - 1) player
        end
      end
      else
        print_endline "You've reached the maximum number of guesses. Game Over."
    in

    guess_loop 3 player

  (**[set_hard] creates a new [HardMode] game based on a specific txt file
     [filename], initializes corresponding answer key and player display to be
     used during the rest of the game*)
  let set_hard (filename : string) =
    let game = Read.HardMode.make_mappings filename in
    let player = Read.HardMode.initial game in
    check_hard game player

  (**[launch] starts the entire crossword game, when the user inputs <make play>
     in the terminal. The user is prompted to choose their game difficulty and
     the specific game of their chose difficulty. Invokes [set_easy],
     [set_medium] or [set_hard] based on the users choices to create the
     corresponding answer keys and player displays. Also handles invalid user
     inputs.*)
  let rec launch (new_game : bool) =
    if new_game then
      print_string
        "Hello user! Welcome to the crossword game! In this game, please pick \
         a difficulty level. Enter <easy>, <medium> or <hard>: "
    else print_endline "Do you want to play again? (yes or no)";

    let diff = String.trim (read_line ()) in

    if
      String.compare diff "easy" <> 0
      && String.compare diff "hard" <> 0
      && String.compare diff "medium" <> 0
    then begin
      print_endline "Invalid entry. Type <easy> or <hard> to continue";
      launch new_game
    end
    else begin
      print_endline ("You have chosen the " ^ diff ^ " mode!");
      if new_game then print_endline "Pick a game! Options are game1 or game2";

      let rec get_game_choice () =
        let choice = String.trim (read_line ()) in
        if String.equal choice "game1" || String.equal choice "game2" then begin
          print_endline ("You have chosen the " ^ diff ^ " " ^ choice ^ "!");
          if String.equal diff "easy" then
            let file_to_read = "data/" ^ choice ^ "_easy.txt" in
            set_easy file_to_read
          else if String.equal diff "medium" then
            let file_to_read = "data/" ^ choice ^ "_medium.txt" in
            set_medium file_to_read
          else
            let file_to_read = "data/" ^ choice ^ "_hard.txt" in
            set_hard file_to_read
        end
        else begin
          print_endline "Invalid entry. Type <game1> or <game2> to continue";
          get_game_choice ()
        end
      in
      get_game_choice ()
    end
end

let () = Play.launch true

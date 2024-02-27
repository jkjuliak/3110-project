(*TEST PLAN: We used OUnit to test out Read module, which includes the HardMode,
  EasyMode, and MediumMode. We went about this mainly through glass box testing,
  and tried to test all edge cases for the functions that we tested. We used
  OUnit to test how specific functions work (like make_mappings, load_words,
  etc.), but we manually tested the full functionality of the game, from start
  to finish. For example, we manually tested user input, and all the ways they
  can play the game, because it was more efficient to do this through the
  terminal. This demonstrates the correctness of the system because we showed
  how many of the functions in the modules work, and the rest can be tested
  through the terminal, by running 'make play.'*)

open OUnit2
open Crossword
open Read
open HardMode
open Stdlib
module WordMap = Map.Make (String)

(*defining hard mode variables*)

let game1_hard_path = "data/game1_hard.txt"
let game1_hard = HardMode.make_mappings game1_hard_path
let game1_hard_initial = HardMode.initial game1_hard
let load_game1_hard = HardMode.load_words game1_hard_path

let expected_words_game1_hard =
  [
    [ "oxymoron"; "a figure of speech that combines contradictory terms" ];
    [ "fumble"; "to use hands clumsily while handling something" ];
    [ "river"; "a large stream of watter flowing into the sea" ];
    [ "never"; "will not happen" ];
    [ "open"; "to unfold something" ];
  ]

let list_words_game1_hard =
  [
    ("oxymoron", "a figure of speech that combines contradictory terms");
    ("fumble", "to use hands clumsily while handling something");
    ("river", "a large stream of watter flowing into the sea");
    ("never", "will not happen");
    ("open", "to unfold something");
  ]

let index_game1_hard =
  HardMode.index "a figure of speech that combines contradictory terms"
    list_words_game1_hard

let return_hint_game1_hard =
  HardMode.return_hint "to use hands clumsily while handling something"
    game1_hard

let return_vowel_game1_hard =
  HardMode.return_vowel "a figure of speech that combines contradictory terms"
    game1_hard

(*defining easy mode variables*)

let game1_easy_path = "data/game1_easy.txt"
let game1_easy = EasyMode.make_mappings game1_easy_path
let game1_easy_initial = EasyMode.initial game1_easy
let load_game1_easy = EasyMode.load_words game1_easy_path

let expected_words_game1_easy =
  [
    [ "odor"; "a distinctive smell"; "the 'O' in B.O." ];
    [ "aloevera"; "a green succulent"; "used for many pharmaceutical purposes" ];
    [
      "freeze";
      "be turned into a solid as a result of extreme cold";
      "the process of water turning into ice";
    ];
    [ "orchid"; "colorful flowering plant"; "often pink" ];
    [ "cloaked"; "to cover something"; "ends with 'ed'" ];
    [
      "acid";
      "a chemical substance that neutralizes alkalis";
      "pH of less than 7";
    ];
    [ "whole"; "a thing that is complete"; "the antonym of incomplete" ];
    [ "grows"; "to become larger"; "associated with getting taller" ];
  ]

let list_words_game1_easy =
  [
    ("odor", "a distinctive smell");
    ("aloevera", "a green succulent");
    ("freeze", "be turned into a solid as a result of extreme cold");
    ("orchid", "colorful flowering plant");
    ("cloaked", "to cover something");
    ("acid", "a chemical substance that neutralizes alkalis");
    ("whole", "a thing that is complete");
    ("grows", "to become larger");
  ]

let index_game1_easy =
  EasyMode.index "a distinctive smell" list_words_game1_easy

let index_game1_easy_2 =
  EasyMode.index "colorful flowering plant" list_words_game1_easy

let index_game1_easy_3 =
  EasyMode.index "a chemical substance that neutralizes alkalis"
    list_words_game1_easy

let index_game1_easy_none =
  EasyMode.index "not real definition" list_words_game1_easy

let index_game1_easy_word = EasyMode.index "acid" list_words_game1_easy

let element_position_game1_easy =
  EasyMode.element_at_position 0 list_words_game1_easy

let element_position_game1_easy_2 =
  EasyMode.element_at_position 4 list_words_game1_easy

let element_position_game1_easy_3 =
  EasyMode.element_at_position 2 list_words_game1_easy

let element_position_game1_easy_error =
  EasyMode.element_at_position 100 list_words_game1_easy

let element_position_game1_easy_error_2 =
  EasyMode.element_at_position 8 list_words_game1_easy

let return_hint_game1_easy =
  EasyMode.return_hint "a distinctive smell" game1_easy

let return_hint_game1_easy_2 =
  EasyMode.return_hint "colorful flowering plant" game1_easy

let return_hint_game1_easy_3 =
  EasyMode.return_hint "a thing that is complete" game1_easy

let return_hint_game1_easy_none =
  EasyMode.return_hint "not a definition" game1_easy

let return_hint_game1_easy_word = EasyMode.return_hint "orchid" game1_easy

let return_vowel_game1_easy =
  EasyMode.return_vowel "a distinctive smell" game1_easy

let return_vowel_game1_easy_2 =
  EasyMode.return_vowel "be turned into a solid as a result of extreme cold"
    game1_easy

let return_vowel_game1_easy_3 =
  EasyMode.return_vowel "a chemical substance that neutralizes alkalis"
    game1_easy

(*defining medium mode variables*)

let game1_medium_path = "data/game1_medium.txt"
let game1_medium = MediumMode.make_mappings game1_medium_path
let game1_medium_initial = MediumMode.initial game1_medium
let load_game1_medium = MediumMode.load_words game1_medium_path

let expected_words_game1_medium =
  [
    [ "adjacent"; "next to"; "alongside" ];
    [ "cajole"; "to persuade or coax someone"; "has a j in the word" ];
    [
      "linger";
      "to stay somewhere for longer than necessary";
      "rhymes with finger";
    ];
    [ "gnarled"; "twisted"; "can be used to describe an old person's fingers" ];
    [ "complacent"; "satisfied with yourself"; "rhymes with adjacent" ];
    [
      "chug";
      "to swallow a drink quickly";
      "can also be used to describe a train that moves along slowly";
    ];
  ]

let list_words_game1_medium =
  [
    ("adjacent", "next to");
    ("cajole", "to persuade or coax someone");
    ("linger", "to stay somewhere for longer than necessary");
    ("gnarled", "twisted");
    ("complacent", "satisfied with yourself");
    ("chug", "to swallow a drink quickly");
  ]

let index_game1_medium = MediumMode.index "next to" list_words_game1_medium

let index_game1_medium_2 =
  MediumMode.index "to swallow a drink quickly" list_words_game1_medium

let index_game1_medium_none =
  MediumMode.index "not real definition" list_words_game1_medium

let index_game1_medium_word = MediumMode.index "gnarled" list_words_game1_medium

let element_position_game1_medium =
  MediumMode.element_at_position 0 list_words_game1_medium

let element_position_game1_medium_2 =
  MediumMode.element_at_position 4 list_words_game1_medium

let element_position_game1_medium_3 =
  MediumMode.element_at_position 3 list_words_game1_medium

let element_position_game1_medium_error =
  MediumMode.element_at_position 100 list_words_game1_medium

let return_hint_game1_medium = MediumMode.return_hint "next to" game1_medium

let return_hint_game1_medium_2 =
  MediumMode.return_hint "to stay somewhere for longer than necessary"
    game1_medium

let return_hint_game1_medium_3 =
  MediumMode.return_hint "to swallow a drink quickly" game1_medium

let return_hint_game1_medium_none =
  MediumMode.return_hint "not a definition" game1_medium

let return_hint_game1_medium_word = MediumMode.return_hint "chug" game1_medium

(*test cases*)

let hard_mode_tests =
  [
    ( "check HardMode true" >:: fun _ ->
      assert_equal true (HardMode.check game1_hard "oxymoron") );
    ( "check HardMode false" >:: fun _ ->
      assert_equal false (HardMode.check game1_hard "hello") );
    ( "check HardMode true (later word)" >:: fun _ ->
      assert_equal true (HardMode.check game1_hard "fumble") );
    ( "check HardMode false (word in definition)" >:: fun _ ->
      assert_equal false (HardMode.check game1_hard "speech") );
    ( "load_words HardMode game 1" >:: fun _ ->
      assert_equal expected_words_game1_hard load_game1_hard );
    ( "index HardMode game 1 (None)" >:: fun _ ->
      assert_equal None index_game1_hard );
    ( "return_hint HardMode game 1 (empty string)" >:: fun _ ->
      assert_equal "" return_hint_game1_hard );
    ( "return_vowel HardMode game 1 (empty string)" >:: fun _ ->
      assert_equal "" return_vowel_game1_hard );
  ]

let easy_mode_tests =
  [
    ( "check EasyMode true" >:: fun _ ->
      assert_equal true (EasyMode.check game1_easy "odor") );
    ( "check EasyMode false" >:: fun _ ->
      assert_equal false (EasyMode.check game1_easy "hi") );
    ( "check EasyMode true (later word)" >:: fun _ ->
      assert_equal true (EasyMode.check game1_easy "acid") );
    ( "check EasyMode false (word in hint)" >:: fun _ ->
      assert_equal false (EasyMode.check game1_easy "getting") );
    ( "check EasyMode false (word in definition)" >:: fun _ ->
      assert_equal false (EasyMode.check game1_easy "larger") );
    ( "load_words EasyMode game 1" >:: fun _ ->
      assert_equal expected_words_game1_easy load_game1_easy );
    ( "index EasyMode game 1 (first definition)" >:: fun _ ->
      assert_equal (Some 0) index_game1_easy );
    ( "index EasyMode game 1 (other definition)" >:: fun _ ->
      assert_equal (Some 3) index_game1_easy_2 );
    ( "index EasyMode game 1 (other definition 2)" >:: fun _ ->
      assert_equal (Some 5) index_game1_easy_3 );
    ( "index EasyMode game 1 (not real definition)" >:: fun _ ->
      assert_equal None index_game1_easy_none );
    ( "index EasyMode game 1 (word)" >:: fun _ ->
      assert_equal None index_game1_easy_word );
    ( "element_at_position EasyMode game 1 (first definition)" >:: fun _ ->
      assert_equal "a distinctive smell" element_position_game1_easy );
    ( "element_at_position EasyMode game 1 (fifth definition)" >:: fun _ ->
      assert_equal "to cover something" element_position_game1_easy_2 );
    ( "element_at_position EasyMode game 1 (other definition)" >:: fun _ ->
      assert_equal "be turned into a solid as a result of extreme cold"
        element_position_game1_easy_3 );
    ( "element_at_position EasyMode game 1 (error)" >:: fun _ ->
      assert_equal "ERROR" element_position_game1_easy_error );
    ( "element_at_position EasyMode game 1 (error 2)" >:: fun _ ->
      assert_equal "ERROR" element_position_game1_easy_error_2 );
    ( "return_hint EasyMode game 1 (first definition)" >:: fun _ ->
      assert_equal "the 'O' in B.O." return_hint_game1_easy );
    ( "return_hint EasyMode game 1 (later definition)" >:: fun _ ->
      assert_equal "often pink" return_hint_game1_easy_2 );
    ( "return_hint EasyMode game 1 (later definition 3)" >:: fun _ ->
      assert_equal "the antonym of incomplete" return_hint_game1_easy_3 );
    ( "return_hint EasyMode game 1 (wrong definition)" >:: fun _ ->
      assert_equal "None" return_hint_game1_easy_none );
    ( "return_hint EasyMode game 1 (word instead of definition)" >:: fun _ ->
      assert_equal "None" return_hint_game1_easy_word );
    ( "return_vowel EasyMode game 1" >:: fun _ ->
      assert_equal "oXoX" return_vowel_game1_easy );
    ( "return_vowel EasyMode game 1 (two)" >:: fun _ ->
      assert_equal "XXeeXe" return_vowel_game1_easy_2 );
    ( "return_vowel EasyMode game 1 (three)" >:: fun _ ->
      assert_equal "aXiX" return_vowel_game1_easy_3 );
  ]

let medium_mode_tests =
  [
    ( "check MediumMode true" >:: fun _ ->
      assert_equal true (MediumMode.check game1_medium "adjacent") );
    ( "check MediumMode false" >:: fun _ ->
      assert_equal false (MediumMode.check game1_medium "hi") );
    ( "check MediumMode true (later word)" >:: fun _ ->
      assert_equal true (MediumMode.check game1_medium "linger") );
    ( "check MediumMode false (word in definition)" >:: fun _ ->
      assert_equal false (MediumMode.check game1_medium "coax") );
    ( "check MediumMode false (word in hint)" >:: fun _ ->
      assert_equal false (MediumMode.check game1_medium "describe") );
    ( "load_words MediumMode game 1" >:: fun _ ->
      assert_equal expected_words_game1_medium load_game1_medium );
    ( "index MediumMode game 1 (first definition)" >:: fun _ ->
      assert_equal (Some 0) index_game1_medium );
    ( "index MediumMode game 1 (other definition)" >:: fun _ ->
      assert_equal (Some 5) index_game1_medium_2 );
    ( "index MediumMode game 1 (not real definition)" >:: fun _ ->
      assert_equal None index_game1_medium_none );
    ( "index MediumMode game 1 (word)" >:: fun _ ->
      assert_equal None index_game1_medium_word );
    ( "element_at_position MediumMode game 1 (first definition)" >:: fun _ ->
      assert_equal "next to" element_position_game1_medium );
    ( "element_at_position MediumMode game 1 (fifth definition)" >:: fun _ ->
      assert_equal "satisfied with yourself" element_position_game1_medium_2 );
    ( "element_at_position MediumMode game 1 (fourth definition)" >:: fun _ ->
      assert_equal "twisted" element_position_game1_medium_3 );
    ( "element_at_position MediumMode game 1 (error)" >:: fun _ ->
      assert_equal "ERROR" element_position_game1_medium_error );
    ( "return_hint MediumMode game 1 (first definition)" >:: fun _ ->
      assert_equal "alongside" return_hint_game1_medium );
    ( "return_hint MediumMode game 1 (later definition)" >:: fun _ ->
      assert_equal "rhymes with finger" return_hint_game1_medium_2 );
    ( "return_hint MediumMode game 1 (later definition 3)" >:: fun _ ->
      assert_equal
        "can also be used to describe a train that moves along slowly"
        return_hint_game1_medium_3 );
    ( "return_hint MediumMode game 1 (wrong definition)" >:: fun _ ->
      assert_equal "None" return_hint_game1_medium_none );
    ( "return_hint MediumMode game 1 (word instead of definition)" >:: fun _ ->
      assert_equal "None" return_hint_game1_medium_word );
  ]

let suite =
  "suite"
  >::: List.flatten [ hard_mode_tests; easy_mode_tests; medium_mode_tests ]

let () = run_test_tt_main suite

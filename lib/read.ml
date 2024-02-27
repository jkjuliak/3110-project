open List
open Stdlib
open String

module WordMap = Map.Make (String)
(**This map is what the answer key and the player display uses, maps string keys
   to string values.*)

module type Game = sig
  type t
  type a

  val load_words : string -> string list list
  val make_mappings : string -> t
  val index : string -> (string * string) list -> int option
  val initial : t -> a
  val element_at_position : int -> (string * string) list -> string
  val update : a -> string -> a
  val update_vowel : a -> t -> string -> string -> a
  val return_hint : string -> t -> string
  val return_vowel : string -> t -> string
  val check : t -> string -> bool
  val print_t : t -> unit
  val print_a : a -> unit
  val bindings_display : a -> (string * string) list
end

(**Defines the module Hardmode, which uses the Game Signature*)

module EasyMode : Game = struct
  type t = {
    word_meaning : string WordMap.t;
    word_hints : string WordMap.t;
    meaning_vowel : string WordMap.t;
  }
  (**The Game answer key of representation type t for [EasyMode] is a record
     with three mappings of type string WordMap, [word_meaning], [word_hints]
     and [meaning_vowel], which map string keys to string values. [word_meaning]
     maps a word in the crossword to its corresponding meaning. [word_hints]
     maps the same words to their corresponding hints. [meaning_vowel] maps
     meanings of the words in the crossword to their vowel hints.*)

  type a = string WordMap.t
  (**The player display for [EasyMode] is implemented the same way as [HardMode]*)

  (* let print_string_list elements = List.iter (fun str -> print_endline str)
     elements *)

  let load_words (filename : string) : string list list =
    let data = open_in filename in
    let rec read_lines (lines : string list list) =
      try
        let line = input_line data in
        let lst = Str.split (Str.regexp ",") line in
        (* print_string_list lst; *)
        read_lines (lst :: lines)
      with End_of_file ->
        close_in data;
        List.rev lines
    in
    read_lines []

  let rec extract_words (lst : 'a list list) (acct : string WordMap.t) :
      string WordMap.t =
    match lst with
    | [] -> acct
    | h :: t ->
        (* print_string_list h; *)
        let word, meaning = (List.nth h 0, List.nth h 1) in
        extract_words t (WordMap.add word meaning acct)

  (**Helper method for val [make_mappings]. Maps each word to its hint,
     accumulates and returns them in a Map of type string WordMap.t*)
  let rec extract_hints (lst : 'a list list) (acch : string WordMap.t) :
      string WordMap.t =
    match lst with
    | [] -> acch
    | h :: t ->
        (*print_string_list h;*)
        let word, hints = (List.nth h 0, List.nth h 2) in
        extract_hints t (WordMap.add word hints acch)

  (**Helper method for val [make_mappings]. Maps each word meaning to its vowel
     hint, accumulates and returns them in a Map of type string WordMap.t. To
     understand the implementation, here is an example. If ant is the word and
     "an tiny six-legged insect" is its meaning, [extract_vowel] maps "a tiny
     six-legged insect" to "aXX". A vowel hint is essentially the word with its
     consonants hidden*)
  let rec extract_vowels (words : (string * 'a) list) (acch : string WordMap.t)
      : string WordMap.t =
    match words with
    | [] -> acch
    | (word, meaning) :: t ->
        let vowelled =
          let is_vowel c =
            match Char.lowercase_ascii c with
            | 'a' | 'e' | 'i' | 'o' | 'u' -> false
            | _ -> true
          in
          String.map (fun c -> if is_vowel c then 'X' else c) word
        in
        let updated_acch = WordMap.add meaning vowelled acch in
        (* print_endline ("Word: " ^ word ^ ", Vowelled: " ^ vowelled); *)
        extract_vowels t updated_acch

  let print_t (main : t) =
    let current = main.word_meaning in
    WordMap.iter (fun _ value -> print_endline value) current

  (**val make_mappings is defined for [HardMode]. Assigns the word-to-meaning
     WordMap returned by helper [extract_words], [extract_hints] and
     [extract_vowels] to the record attributes [word_meaning], [word_hints] and
     [meaning_vowel] respectively*)
  let make_mappings (filename : string) : t =
    let txt_output = load_words filename in
    let words = extract_words txt_output WordMap.empty in
    let hints = extract_hints txt_output WordMap.empty in
    let vowels = extract_vowels (WordMap.bindings words) WordMap.empty in
    let final =
      { word_meaning = words; word_hints = hints; meaning_vowel = vowels }
    in
    final

  let rec ex n = if n <= 0 then "" else "X" ^ ex (n - 1)

  let initial (main : t) : a =
    let rec generate (bindings : (string * string) list) (acc : a) : a =
      match bindings with
      | [] -> acc
      | h :: t ->
          let word_len = String.length (fst h) in
          generate t (WordMap.add (fst h) (ex word_len) acc)
    in
    generate (WordMap.bindings main.word_meaning) WordMap.empty

  (**val [index] is defined for [EasyMode]. Implementation specified in read.mli*)
  let rec index (a : string) (lst : (string * string) list) : int option =
    match lst with
    | [] -> None
    | (k, v) :: t -> (
        if v = a then Some 0
        else
          match index a t with
          | None ->
              (* print_endline "None"; *)
              None
          | Some i ->
              (* print_endline (string_of_int i); *)
              Some (i + 1))

  (**val [element_at_position] is defined for [EasyMode]. Implementation
     specified in read.mli*)
  let element_at_position (a : int) (lst : (string * string) list) =
    match List.nth_opt lst a with
    | Some (k, v) -> v
    | None -> "ERROR"

  let update (current : a) (input : string) : a =
    WordMap.update input (fun _ -> Some input) current

  (**Helper method for [update_vowel]. Looks up [value] of type string in [map]
     of type string WordMap, returns its corresponding Some key if found, None
     if not found*)
  let find_key_by_value (value : string) (map : string WordMap.t) :
      string option =
    let is_value_equal (_, v) = v = value in
    match List.find_opt is_value_equal (WordMap.bindings map) with
    | Some (k, _) -> Some k
    | None -> None

  (**val [update_vowel] is defined for [EasyMode]. Updates the value in player
     display [current] with the vowel hint form of the obscured word, which is
     found using [meaning] from the [meaning_vowel] mapping in [main]*)
  let update_vowel (current : a) (main : t) (input : string) (meaning : string)
      : a =
    match find_key_by_value meaning main.word_meaning with
    | Some c -> WordMap.update c (fun _ -> Some input) current
    | None -> current

  (**val [return_hint] is defined for [EasyMode]. Returns the corresponding hint
     of a word from [main]'s [word_hint] mapping.*)
  let return_hint (input : string) (main : t) : string =
    let meanings = WordMap.bindings main.word_meaning in
    let meaning_index = index input meanings in
    match meaning_index with
    | Some x -> element_at_position x (WordMap.bindings main.word_hints)
    | None -> "None"

  (**val [return_vowel] is defined for [EasyMode]. Returns the corresponding
     hint of a word [current] from [main]'s [meaning_vowel] mapping.*)
  let return_vowel (current : string) (main : t) : string =
    (*print_endline ("current string: " ^ current); print_endline ( WordMap.find
      current main.meaning_vowel);*)
    WordMap.find current main.meaning_vowel

  (**Helper function, returns the binding of the current printer display*)
  let bindings_display (display : a) = WordMap.bindings display

  (**Helper function, returns the values of a key-value list as a string list*)
  let rec get_word_list (lst : (string * string) list) : string list =
    match lst with
    | [] -> [] (* Base case: empty list *)
    | (_, word) :: rest -> word :: get_word_list rest

  (**Helper method, returns the letter at [strpos] of a word at [lstpos] of a
     string list [lst]*)
  let locate lst lstpos strpos = String.sub (List.nth lst lstpos) strpos 1

  (**[game1_ez_printer] and [game2_ez_printer] return a string list list of the
     [EasyMode] crossword board strictly formatted based on the words in the
     corresponding txt file. *)
  let game1_ez_printer (current : a) =
    let bin = bindings_display current in
    let lst = get_word_list bin in
    let f = locate lst 3 0 in
    let c = locate lst 2 0 in
    let r = String.sub (List.nth lst 3) 2 1 in
    let o = String.sub (List.nth lst 6) 0 1 in
    let a = String.sub (List.nth lst 1) 0 1 in
    let l =
      if String.equal (String.sub (List.nth lst 2) 1 1) "X" then
        String.sub (List.nth lst 1) 1 1
      else String.sub (List.nth lst 2) 1 1
    in
    let o1 = String.sub (List.nth lst 1) 2 1 in
    let e =
      if String.equal (String.sub (List.nth lst 3) 2 1) "X" then
        String.sub (List.nth lst 1) 3 1
      else String.sub (List.nth lst 3) 2 1
    in
    let v = String.sub (List.nth lst 1) 4 1 in
    let e1 = String.sub (List.nth lst 1) 5 1 in
    let r1 =
      if String.equal (String.sub (List.nth lst 1) 6 1) "X" then
        String.sub (List.nth lst 6) 1 1
      else String.sub (List.nth lst 1) 6 1
    in
    let a1 = String.sub (List.nth lst 1) 7 1 in
    let o2 = String.sub (List.nth lst 2) 2 1 in
    let e2 = String.sub (List.nth lst 3) 3 1 in
    let c1 = String.sub (List.nth lst 6) 2 1 in
    let a2 = String.sub (List.nth lst 2) 3 1 in
    let z = String.sub (List.nth lst 3) 4 1 in
    let h = String.sub (List.nth lst 6) 3 1 in
    let k = String.sub (List.nth lst 2) 4 1 in
    let e3 = String.sub (List.nth lst 3) 5 1 in
    let i = String.sub (List.nth lst 6) 4 1 in
    let g = String.sub (List.nth lst 4) 0 1 in
    let e4 = String.sub (List.nth lst 2) 5 1 in
    let o3 = locate lst 5 0 in
    let d =
      if String.equal (locate lst 5 1) "X" then locate lst 6 5
      else locate lst 5 1
    in
    let o4 = locate lst 5 2 in
    let r2 =
      if String.equal (locate lst 5 3) "X" then locate lst 4 1
      else locate lst 5 3
    in
    let a3 = locate lst 0 0 in
    let c2 = locate lst 0 1 in
    let i2 = locate lst 0 2 in
    let d2 =
      if String.equal (locate lst 0 3) "X" then locate lst 2 6
      else locate lst 0 3
    in
    let o5 = locate lst 4 2 in
    let w =
      if String.equal (locate lst 7 0) "X" then locate lst 4 3
      else locate lst 7 0
    in
    let h1 = locate lst 7 1 in
    let o6 = locate lst 7 2 in
    let l2 = locate lst 7 3 in
    let e5 = locate lst 7 4 in
    let s = locate lst 4 4 in
    let crossword =
      [
        [ " * * * * * " ^ f ^ " * * * * * * * * *" ];
        [ " * * * " ^ c ^ " * " ^ r ^ " * * " ^ o ^ " * * * * * *" ];
        [
          " * * " ^ a ^ " " ^ l ^ " " ^ o1 ^ " " ^ e ^ " " ^ v ^ " " ^ e1 ^ " "
          ^ r1 ^ " " ^ a1 ^ " * * * * *";
        ];
        [ " * * * " ^ o2 ^ " * " ^ e2 ^ " * * " ^ c1 ^ " * * * * * *" ];
        [ " * * * " ^ a2 ^ " * " ^ z ^ " * * " ^ h ^ " * * * * * *" ];
        [ " * * * " ^ k ^ " * " ^ e3 ^ " * * " ^ i ^ " * " ^ g ^ " * * * *" ];
        [
          " * * * " ^ e4 ^ " * * * " ^ o3 ^ " " ^ d ^ " " ^ o4 ^ " " ^ r2
          ^ " * * * *";
        ];
        [
          " " ^ a3 ^ " " ^ c2 ^ " " ^ i2 ^ " " ^ d2 ^ " * * * * * * " ^ o5
          ^ " * * * *";
        ];
        [
          " * * * * * * * * * * " ^ w ^ " " ^ h1 ^ " " ^ o6 ^ " " ^ l2 ^ " "
          ^ e5;
        ];
        [ " * * * * * * * * * * " ^ s ^ " * * * *" ];
      ]
    in
    crossword

  let game2_ez_printer (current : a) =
    let bin = bindings_display current in
    let lst = get_word_list bin in
    let c = locate lst 0 0 in
    let h = locate lst 2 0 in
    let l = locate lst 0 1 in
    let o1 = locate lst 2 1 in
    let o2 = locate lst 0 2 in
    let p = locate lst 3 0 in
    let u = locate lst 3 1 in
    let m = locate lst 3 2 in
    let p2 =
      if String.equal (locate lst 3 3) "X" then locate lst 2 2
      else locate lst 3 3
    in
    let k = locate lst 3 4 in
    let i = locate lst 3 5 in
    let n =
      if String.equal (locate lst 3 6) "X" then locate lst 0 3
      else locate lst 3 6
    in
    let p3 = locate lst 2 3 in
    let e = locate lst 0 4 in
    let i2 = locate lst 2 4 in
    let d = locate lst 1 0 in
    let u2 = locate lst 1 1 in
    let n1 =
      if String.equal (locate lst 1 2) "X" then locate lst 2 5
      else locate lst 1 2
    in
    let e2 = locate lst 1 3 in
    let g = locate lst 2 6 in
    let crossword =
      [
        [ " * * * * * * " ^ c ];
        [ " * * * " ^ h ^ " * * " ^ l ];
        [ " * * * " ^ o1 ^ " * * " ^ o2 ];
        [ " " ^ p ^ " " ^ u ^ " " ^ m ^ " " ^ p2 ^ " " ^ k ^ " " ^ i ^ " " ^ n ];
        [ " * * * " ^ p3 ^ " * * " ^ e ];
        [ " * * * " ^ i2 ^ " * * *" ];
        [ " * " ^ d ^ " " ^ u2 ^ " " ^ n1 ^ " " ^ e2 ^ " * *" ];
        [ " * * * " ^ g ^ " * * *" ];
      ]
    in

    crossword

  (**[standard_printer] returns which crossword board to print based on the
     first word, or key in the player display [current]*)
  let standard_printer (current : a) : string list list =
    let pointer = bindings_display current in
    let word, _ = List.nth pointer 0 in
    match word with
    | "clone" -> game2_ez_printer current
    | "acid" -> game1_ez_printer current
    | _ -> [ [] ]

  (**Prints the values of a string list list, with each element on a new line,
     to resemble the crossword board*)
  let printer llst =
    List.iter
      (fun inner_list ->
        List.iter (fun str -> Printf.printf "%s " str) inner_list;
        print_newline ())
      llst

  (**prints the player display [current] for the user in crossword board format.
     Uses helpers [printer] and [standard_printer] to find the corresponding
     crossword game board to format [current] into, based on the game that is
     being played *)
  let print_a (current : a) =
    (* WordMap.iter (fun _ value -> print_endline value) current *)
    printer (standard_printer current)

  let check (main : t) (input : string) : bool =
    WordMap.mem input main.word_meaning
end

module MediumMode : Game = struct
  type t = {
    word_meaning : string WordMap.t;
    word_hints : string WordMap.t;
  }
  (**The Game answer key of representation type t for [MediumMode] is a record
     with two mappings of type string WordMap, [word_meaning] and [word_hints],
     which map string keys to string values. [word_meaning] maps a word in the
     crossword to its corresponding meaning. [word_hints] maps the same words to
     their corresponding hints. *)

  type a = string WordMap.t
  (**The player display for [MediumMode] is implemented the same way as
     [HardMode] and [EasyMode]*)

  (**The rest of the methods are implemented similarly to [HardMode] and
     [EasyMode], manipulating relevant mappings and displays to this mode. Vowel
     hints are not used in [MediumMode], but the standard hints are*)

  let print_string_list elements =
    List.iter (fun str -> print_endline str) elements

  let load_words (filename : string) : string list list =
    let data = open_in filename in
    let rec read_lines (lines : string list list) =
      try
        let line = input_line data in
        let lst = Str.split (Str.regexp ",") line in
        print_string_list lst;
        read_lines (lst :: lines)
      with End_of_file ->
        close_in data;
        List.rev lines
    in
    read_lines []

  let rec extract_words (lst : 'a list list) (acct : string WordMap.t) :
      string WordMap.t =
    match lst with
    | [] -> acct
    | h :: t ->
        (*print_string_list h;*)
        let word, meaning = (List.nth h 0, List.nth h 1) in
        extract_words t (WordMap.add word meaning acct)

  let rec extract_hints (lst : 'a list list) (acch : string WordMap.t) :
      string WordMap.t =
    match lst with
    | [] -> acch
    | h :: t ->
        (*print_string_list h;*)
        let word, hints = (List.nth h 0, List.nth h 2) in
        extract_hints t (WordMap.add word hints acch)

  let print_t (main : t) =
    let current = main.word_meaning in
    WordMap.iter (fun _ value -> print_endline value) current

  let make_mappings (filename : string) : t =
    let txt_output = load_words filename in
    let words = extract_words txt_output WordMap.empty in
    let hints = extract_hints txt_output WordMap.empty in
    let final = { word_meaning = words; word_hints = hints } in
    final

  let rec ex n = if n <= 0 then "" else "X" ^ ex (n - 1)

  let initial (main : t) : a =
    let rec generate (bindings : (string * string) list) (acc : a) : a =
      match bindings with
      | [] -> acc
      | h :: t ->
          let word_len = String.length (fst h) in
          generate t (WordMap.add (fst h) (ex word_len) acc)
    in
    generate (WordMap.bindings main.word_meaning) WordMap.empty

  let rec index (a : string) (lst : (string * string) list) : int option =
    match lst with
    | [] -> None
    | (k, v) :: t -> (
        if v = a then Some 0
        else
          match index a t with
          | None -> None
          | Some i -> Some (i + 1))

  let element_at_position (index : int) (lst : (string * string) list) =
    match List.nth_opt lst index with
    | Some (k, v) -> v
    | None -> "ERROR"

  let update (current : a) (input : string) : a =
    WordMap.update input (fun _ -> Some input) current

  let update_vowel (current : a) (main : t) (input : string) (meaning : string)
      : a =
    current

  let return_hint (current : string) (main : t) : string =
    let meanings = WordMap.bindings main.word_meaning in
    let meaning_index = index current meanings in
    match meaning_index with
    | Some x -> element_at_position x (WordMap.bindings main.word_hints)
    | None -> "None"

  let return_vowel (current : string) (main : t) : string = current
  let bindings_display (display : a) = WordMap.bindings display

  let rec get_word_list (lst : (string * string) list) : string list =
    match lst with
    | [] -> [] (* Base case: empty list *)
    | (_, word) :: rest -> word :: get_word_list rest

  (**[game1_med_printer] and [game2_med_printer] return a string list list of
     the [MediumMode] crossword board strictly formatted based on the words in
     the corresponding txt file. *)
  let game1_med_printer (current : a) =
    let bin = bindings_display current in
    let lst = get_word_list bin in
    let a = String.sub (List.nth lst 0) 0 1 in
    let d = String.sub (List.nth lst 0) 1 1 in
    let j = String.sub (List.nth lst 0) 2 1 in
    let c =
      if String.equal (String.sub (List.nth lst 2) 0 1) "X" then
        String.sub (List.nth lst 3) 0 1
      else String.sub (List.nth lst 2) 0 1
    in
    let h = String.sub (List.nth lst 2) 1 1 in
    let u = String.sub (List.nth lst 2) 2 1 in
    let g = String.sub (List.nth lst 2) 3 1 in
    let c2 = String.sub (List.nth lst 1) 0 1 in
    let a2 =
      if String.equal (String.sub (List.nth lst 1) 1 1) "X" then
        String.sub (List.nth lst 0) 3 1
      else String.sub (List.nth lst 1) 1 1
    in
    let j2 = String.sub (List.nth lst 1) 2 1 in
    let o = String.sub (List.nth lst 1) 3 1 in
    let l =
      if String.equal (String.sub (List.nth lst 1) 4 1) "X" then
        String.sub (List.nth lst 5) 0 1
      else String.sub (List.nth lst 1) 4 1
    in
    let e = String.sub (List.nth lst 1) 5 1 in
    let o2 = String.sub (List.nth lst 3) 1 1 in
    let c3 = String.sub (List.nth lst 0) 4 1 in
    let i = String.sub (List.nth lst 5) 1 1 in
    let m = String.sub (List.nth lst 3) 2 1 in
    let e2 = String.sub (List.nth lst 0) 5 1 in
    let n = String.sub (List.nth lst 5) 2 1 in
    let p = String.sub (List.nth lst 3) 3 1 in
    let n2 = String.sub (List.nth lst 0) 6 1 in
    let g2 =
      if String.equal (String.sub (List.nth lst 5) 3 1) "X" then
        String.sub (List.nth lst 4) 0 1
      else String.sub (List.nth lst 5) 3 1
    in
    let n3 = String.sub (List.nth lst 4) 1 1 in
    let a3 = String.sub (List.nth lst 4) 2 1 in
    let r = String.sub (List.nth lst 4) 3 1 in
    let l2 =
      if String.equal (String.sub (List.nth lst 4) 4 1) "X" then
        String.sub (List.nth lst 3) 4 1
      else String.sub (List.nth lst 4) 4 1
    in
    let e3 = String.sub (List.nth lst 4) 5 1 in
    let d2 = String.sub (List.nth lst 4) 6 1 in
    let t = String.sub (List.nth lst 0) 7 1 in
    let e4 = String.sub (List.nth lst 5) 4 1 in
    let a4 = String.sub (List.nth lst 3) 5 1 in
    let r2 = String.sub (List.nth lst 5) 5 1 in
    let c4 = String.sub (List.nth lst 3) 6 1 in
    let e5 = String.sub (List.nth lst 3) 7 1 in
    let n4 = String.sub (List.nth lst 3) 8 1 in
    let t2 = String.sub (List.nth lst 3) 9 1 in
    let crossword =
      [
        [ " * " ^ a ^ " * * * * * * * * * *" ];
        [ " * " ^ d ^ " * * * * * * * * * *" ];
        [ " * " ^ j ^ " * * * * * * " ^ c ^ " " ^ h ^ " " ^ u ^ " " ^ g ];
        [
          " " ^ c2 ^ " " ^ a2 ^ " " ^ j2 ^ " " ^ o ^ " " ^ l ^ " " ^ e ^ " * * "
          ^ o2 ^ " * * *";
        ];
        [ " * " ^ c3 ^ " * * " ^ i ^ " * * * " ^ m ^ " * * *" ];
        [ " * " ^ e2 ^ " * * " ^ n ^ " * * * " ^ p ^ " * * *" ];
        [
          " * " ^ n2 ^ " * * " ^ g2 ^ " " ^ n3 ^ " " ^ a3 ^ " " ^ r ^ " " ^ l2
          ^ " " ^ e3 ^ " " ^ d2 ^ " *";
        ];
        [ " * " ^ t ^ " * * " ^ e4 ^ " * * * " ^ a4 ^ " * * *" ];
        [ " * * * * " ^ r2 ^ " * * * " ^ c4 ^ " * * *" ];
        [ " * * * * * * * * " ^ e5 ^ " * * *" ];
        [ " * * * * * * * * " ^ n4 ^ " * * *" ];
        [ " * * * * * * * * " ^ t2 ^ " * * *" ];
      ]
    in

    crossword

  let game2_med_printer (current : a) =
    let bin = bindings_display current in
    let lst = get_word_list bin in
    let m = String.sub (List.nth lst 1) 0 1 in
    let e = String.sub (List.nth lst 1) 1 1 in
    let c = String.sub (List.nth lst 0) 0 1 in
    let s = String.sub (List.nth lst 3) 0 1 in
    let a =
      if String.equal (String.sub (List.nth lst 3) 1 1) "X" then
        String.sub (List.nth lst 1) 2 1
      else String.sub (List.nth lst 3) 1 1
    in
    let t = String.sub (List.nth lst 3) 2 1 in
    let u = String.sub (List.nth lst 3) 3 1 in
    let r =
      if String.equal (String.sub (List.nth lst 3) 4 1) "X" then
        String.sub (List.nth lst 0) 1 1
      else String.sub (List.nth lst 3) 4 1
    in
    let d = String.sub (List.nth lst 3) 5 1 in
    let a2 = String.sub (List.nth lst 3) 6 1 in
    let y =
      if String.equal (String.sub (List.nth lst 3) 7 1) "X" then
        String.sub (List.nth lst 5) 0 1
      else String.sub (List.nth lst 3) 7 1
    in
    let n = String.sub (List.nth lst 1) 3 1 in
    let a3 = String.sub (List.nth lst 0) 2 1 in
    let e1 = String.sub (List.nth lst 5) 1 1 in
    let d1 = String.sub (List.nth lst 1) 4 1 in
    let t1 =
      if String.equal (String.sub (List.nth lst 0) 3 1) "X" then
        String.sub (List.nth lst 4) 0 1
      else String.sub (List.nth lst 0) 3 1
    in
    let e2 = String.sub (List.nth lst 4) 1 1 in
    let l = String.sub (List.nth lst 4) 2 1 in
    let l2 =
      if String.equal (String.sub (List.nth lst 4) 3 1) "X" then
        String.sub (List.nth lst 5) 2 1
      else String.sub (List.nth lst 4) 3 1
    in
    let s2 = String.sub (List.nth lst 4) 4 1 in
    let e3 = String.sub (List.nth lst 1) 5 1 in
    let e4 = String.sub (List.nth lst 0) 4 1 in
    let l3 = String.sub (List.nth lst 5) 3 1 in
    let r2 = String.sub (List.nth lst 1) 6 1 in
    let o = String.sub (List.nth lst 5) 4 1 in
    let m2 = String.sub (List.nth lst 2) 0 1 in
    let e5 = String.sub (List.nth lst 2) 1 1 in
    let l4 = String.sub (List.nth lst 2) 2 1 in
    let l5 = String.sub (List.nth lst 2) 3 1 in
    let o2 = String.sub (List.nth lst 2) 4 1 in
    let w =
      if String.equal (String.sub (List.nth lst 2) 5 1) "X" then
        String.sub (List.nth lst 5) 5 1
      else String.sub (List.nth lst 2) 5 1
    in
    let crossword =
      [
        [ " * " ^ m ^ " * * * * * * *" ];
        [ " * " ^ e ^ " * * " ^ c ^ " * * * *" ];
        [
          " " ^ s ^ " " ^ a ^ " " ^ t ^ " " ^ u ^ " " ^ r ^ " " ^ d ^ " " ^ a2
          ^ " " ^ y ^ " *";
        ];
        [ " * " ^ n ^ " * * " ^ a3 ^ " * * " ^ e1 ^ " *" ];
        [ " * " ^ d1 ^ " * * " ^ t1 ^ " " ^ e2 ^ " " ^ l ^ " " ^ l2 ^ " " ^ s2 ];
        [ " * " ^ e3 ^ " * * " ^ e4 ^ " * * " ^ l3 ^ " *" ];
        [ " * " ^ r2 ^ " * * * * * " ^ o ^ " *" ];
        [
          " * * " ^ m2 ^ " " ^ e5 ^ " " ^ l4 ^ " " ^ l5 ^ " " ^ o2 ^ " " ^ w
          ^ " *";
        ];
      ]
    in

    crossword

  let standard_printer (current : a) : string list list =
    let pointer = bindings_display current in
    let word, _ = List.nth pointer 0 in
    match word with
    | "crate" -> game2_med_printer current
    | "adjacent" -> game1_med_printer current
    | _ -> [ [] ]

  (**Prints the values of the current player display [current] for the user*)
  let printer llst =
    List.iter
      (fun inner_list ->
        List.iter (fun str -> Printf.printf "%s " str) inner_list;
        print_newline ())
      llst

  let print_a (current : a) =
    (* WordMap.iter (fun _ value -> print_endline value) current *)
    printer (standard_printer current)

  let check (main : t) (input : string) : bool =
    WordMap.mem input main.word_meaning
end

module HardMode : Game = struct
  type t = { word_meaning : string WordMap.t }
  (**The Game answer key of representation type t for [HardMode] is a record
     with one mapping of type string WordMap, [word_meaning], which masp string
     keys to string values. [word_meaning] maps a word in the crossword to its
     corresponding meaning*)

  type a = string WordMap.t
  (**The Game player display of representation type a for [HardMode] is defined
     to be a mapping of type string WordMap. The player display maps a word of
     type string to its corresponding string display that is printed during the
     game*)

  (**[print_list] is a helper method for debugging purposes, prints out a list
     of strings, each of its elements on a new line*)
  let rec print_list lst =
    match lst with
    | [] -> print_endline "empty"
    | h :: t ->
        print_endline h;
        print_list t

  (**val [load_words] is defined for [HardMode]. Returns a string list list,
     which contains a list of each line that is read from the wordbank txt file,
     each element being a two-element list containing a word and its meaning. *)
  let load_words (filename : string) : string list list =
    let data = open_in filename in
    let rec read_lines (lines : string list list) =
      try
        let line = input_line data in
        (* Process the line here *)
        let lst = Str.split (Str.regexp ",") line in
        read_lines (lst :: lines)
      with End_of_file ->
        close_in data;
        List.rev lines
    in
    read_lines []

  (**Helper method for val [make_mappings]. Maps each word to its meaning,
     accumulates and returns them in a Map of type string WordMap.t*)
  let rec extract_words (lst : 'a list list) (acct : string WordMap.t) :
      string WordMap.t =
    match lst with
    | [] -> acct
    | h :: t ->
        let word, meaning = (List.nth h 0, List.nth h 1) in
        extract_words t (WordMap.add word meaning acct)

  (**val make_mappings is defined for [HardMode]. Assigns the word-to-meaning
     WordMap returned by helper [extract_words] to the record attribute
     [word_meaning]*)
  let make_mappings (filename : string) : t =
    let txt_output = load_words filename in
    let words = extract_words txt_output WordMap.empty in
    let final = { word_meaning = words } in
    final

  (**val [index] is not used in [HardMode], so it returns an empty int option*)
  let rec index (a : string) (lst : (string * string) list) : int option = None

  (**Helper method for [initial]. Given an int [n], returns a string of X's of
     length n. For example, if [n] = 3, then returns string "XXX"*)
  let rec ex n = if n <= 0 then "" else "X" ^ ex (n - 1)

  (**val [intial] is defined for [HardMode]. This is the intial player display
     when a new game is started. Given an answer key [main] of type t, [intial]
     copies the keys in the word_meaning mapping, obscures them using [ex] and
     adds them to player display as values. The player display of type a that is
     returned is a string WordMap that maps a word in the crossword, to an
     hidden version of itself. For example
     [("ant" : "XXX"); ("player" : "XXXX"); ...]. This player display is edited
     throughout the game, as the player guesses the words*)
  let initial (main : t) : a =
    let rec generate (bindings : (string * string) list) (acc : a) : a =
      match bindings with
      | [] -> acc
      | h :: t ->
          let word_len = String.length (fst h) in
          generate t (WordMap.add (fst h) (ex word_len) acc)
    in
    generate (WordMap.bindings main.word_meaning) WordMap.empty

  (**val [index] is not used in [HardMode], so it returns an empty string*)
  let element_at_position (a : int) (lst : (string * string) list) = ""

  (**val [update] is defined for [HardMode]. Uses the standard update function
     available in the Map interface*)
  let update (current : a) (input : string) : a =
    WordMap.update input (fun _ -> Some input) current

  (**val [index] is not used in [HardMode], so it returns input player display
     [current]*)
  let update_vowel (current : a) (main : t) (input : string) (meaning : string)
      : a =
    current

  (**val [return_hint] and [return_vowel] are not used in [HardMode], so they
     return an empty string*)
  let return_hint (current : string) (main : t) : string = ""

  let return_vowel (current : string) (main : t) : string = ""

  (**val [check] is defined for [HardMode]. Checks if [input] is a key in answer
     key [main]'s [word_meaning] mapping, using the standard WordMap.mem
     function from the Map interface*)
  let check (main : t) (input : string) : bool =
    WordMap.mem input main.word_meaning

  (**Helper function, returns the binding of the current printer display*)
  let bindings_display (display : a) = WordMap.bindings display

  let rec get_word_list (lst : (string * string) list) : string list =
    match lst with
    | [] -> [] (* Base case: empty list *)
    | (_, word) :: rest -> word :: get_word_list rest

  (**[game1_hard_printer] and [game2_hard_printer] return a string list list of
     the [HardMode] crossword board strictly formatted based on the words in the
     corresponding txt file. *)
  let game1_hard_printer (current : a) =
    let bin = bindings_display current in
    let lst = get_word_list bin in
    let f = String.sub (List.nth lst 0) 0 1 in
    let u = String.sub (List.nth lst 0) 1 1 in
    let r = String.sub (List.nth lst 4) 0 1 in
    let o = String.sub (List.nth lst 3) 0 1 in
    let x = String.sub (List.nth lst 3) 1 1 in
    let y = String.sub (List.nth lst 3) 2 1 in
    let m =
      if String.equal (String.sub (List.nth lst 3) 3 1) "X" then
        String.sub (List.nth lst 0) 2 1
      else String.sub (List.nth lst 3) 3 1
    in
    let o2 = String.sub (List.nth lst 3) 4 1 in
    let r2 = String.sub (List.nth lst 3) 5 1 in
    let o3 =
      if String.equal (String.sub (List.nth lst 3) 6 1) "X" then
        String.sub (List.nth lst 2) 0 1
      else String.sub (List.nth lst 3) 6 1
    in
    let n = String.sub (List.nth lst 3) 7 1 in
    let i = String.sub (List.nth lst 4) 1 1 in
    let b = String.sub (List.nth lst 0) 3 1 in
    let p = String.sub (List.nth lst 2) 1 1 in
    let v = String.sub (List.nth lst 4) 2 1 in
    let l = String.sub (List.nth lst 0) 4 1 in
    let e = String.sub (List.nth lst 2) 2 1 in
    let e1 = String.sub (List.nth lst 4) 3 1 in
    let e2 = String.sub (List.nth lst 0) 5 1 in
    let n2 =
      if String.equal (String.sub (List.nth lst 2) 3 1) "X" then
        String.sub (List.nth lst 1) 0 1
      else String.sub (List.nth lst 2) 3 1
    in
    let e3 = String.sub (List.nth lst 1) 1 1 in
    let v2 = String.sub (List.nth lst 1) 2 1 in
    let e4 = String.sub (List.nth lst 1) 3 1 in
    let r3 = String.sub (List.nth lst 1) 4 1 in

    let crossword =
      [
        [ " * * * " ^ f ^ " * * * * * * *" ];
        [ " * * * " ^ u ^ " * * * * * * " ^ r ];
        [
          " " ^ o ^ " " ^ x ^ " " ^ y ^ " " ^ m ^ " " ^ o2 ^ " " ^ r2 ^ " " ^ o3
          ^ " " ^ n ^ " * * " ^ i;
        ];
        [ " * * * " ^ b ^ " " ^ "* * " ^ p ^ " * * * " ^ v ];
        [ " * * * " ^ l ^ " " ^ "* * " ^ e ^ " * * * " ^ e1 ];
        [
          " * * * " ^ e2 ^ " " ^ "* * " ^ n2 ^ " " ^ e3 ^ " " ^ v2 ^ " " ^ e4
          ^ " " ^ r3;
        ];
      ]
    in

    crossword

  let game2_hard_printer (current : a) =
    let bin = bindings_display current in
    let lst = get_word_list bin in
    let c = String.sub (List.nth lst 0) 0 1 in
    let v = String.sub (List.nth lst 4) 0 1 in
    let f = String.sub (List.nth lst 2) 0 1 in
    let l =
      if String.equal (String.sub (List.nth lst 2) 1 1) "X" then
        String.sub (List.nth lst 0) 1 1
      else String.sub (List.nth lst 2) 1 1
    in
    let o = String.sub (List.nth lst 2) 2 1 in
    let u = String.sub (List.nth lst 2) 3 1 in
    let r = String.sub (List.nth lst 2) 4 1 in
    let i =
      if String.equal (String.sub (List.nth lst 2) 5 1) "X" then
        String.sub (List.nth lst 4) 1 1
      else String.sub (List.nth lst 2) 5 1
    in
    let s = String.sub (List.nth lst 2) 6 1 in
    let h = String.sub (List.nth lst 2) 7 1 in
    let o_2 = String.sub (List.nth lst 0) 2 1 in
    let b = String.sub (List.nth lst 4) 2 1 in
    let u_2 = String.sub (List.nth lst 0) 3 1 in
    let r_2 =
      if String.equal (String.sub (List.nth lst 4) 3 1) "X" then
        String.sub (List.nth lst 3) 0 1
      else String.sub (List.nth lst 4) 3 1
    in
    let a = String.sub (List.nth lst 3) 1 1 in
    let i_2 = String.sub (List.nth lst 3) 2 1 in
    let l_2 = String.sub (List.nth lst 3) 3 1 in
    let r_3 = String.sub (List.nth lst 3) 4 1 in
    let o_3 = String.sub (List.nth lst 3) 5 1 in
    let a_2 = String.sub (List.nth lst 3) 6 1 in
    let d = String.sub (List.nth lst 3) 7 1 in
    let d_2 = String.sub (List.nth lst 0) 4 1 in
    let a_3 = String.sub (List.nth lst 4) 4 1 in
    let n = String.sub (List.nth lst 4) 5 1 in
    let e = String.sub (List.nth lst 1) 0 1 in
    let n_2 = String.sub (List.nth lst 1) 1 1 in
    let c_2 = String.sub (List.nth lst 1) 2 1 in
    let h_2 = String.sub (List.nth lst 1) 3 1 in
    let a_4 = String.sub (List.nth lst 1) 4 1 in
    let n_3 = String.sub (List.nth lst 1) 5 1 in
    let t =
      if String.equal (String.sub (List.nth lst 1) 6 1) "X" then
        String.sub (List.nth lst 4) 6 1
      else String.sub (List.nth lst 1) 6 1
    in

    let crossword =
      [
        [ " * * " ^ c ^ " * * * " ^ v ^ " * * * * * * *" ];
        [
          " * " ^ f ^ " " ^ l ^ " " ^ o ^ " " ^ u ^ " " ^ r ^ " " ^ i ^ " " ^ s
          ^ " " ^ h ^ " * * * * *";
        ];
        [ " * * " ^ o_2 ^ " * * * " ^ b ^ " * * * * * * *" ];
        [
          " * * " ^ u_2 ^ " * * * " ^ r_2 ^ " " ^ a ^ " " ^ i_2 ^ " " ^ l_2
          ^ " " ^ r_3 ^ " " ^ o_3 ^ " " ^ a_2 ^ " " ^ d;
        ];
        [ " * * " ^ d_2 ^ " * * * " ^ a_3 ^ " * * * * * * *" ];
        [ " * * * * * * " ^ n ^ " * * * * * * *" ];
        [
          " " ^ e ^ " " ^ n_2 ^ " " ^ c_2 ^ " " ^ h_2 ^ " " ^ a_4 ^ " " ^ n_3
          ^ " " ^ t ^ " * * * * * * *";
        ];
      ]
    in
    crossword

  let standard_printer (current : a) : string list list =
    let pointer = bindings_display current in
    let word, _ = List.nth pointer 0 in
    match word with
    | "cloud" -> game2_hard_printer current
    | "fumble" -> game1_hard_printer current
    | _ -> [ [] ]

  (**Prints the values of the current player display [current] for the user*)
  let printer llst =
    List.iter
      (fun inner_list ->
        List.iter (fun str -> Printf.printf "%s " str) inner_list;
        print_newline ())
      llst

  let print_a (current : a) =
    (* WordMap.iter (fun _ value -> print_endline value) current *)
    printer (standard_printer current)

  (**Prints the values of the [HardMode] answer key's [word_meaning] mapping for
     the debugging purposes.*)
  let print_t (main : t) =
    let current = main.word_meaning in
    WordMap.iter (fun _ value -> print_endline value) current
end

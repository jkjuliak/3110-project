(** The signature of a Crossword game of modifiable difficulty *)
module type Game = sig
  type t
  (** Representation type of the Crossword answer key *)

  type a
  (** Representation type of the player's display when the game is played *)

  val load_words : string -> string list list
  (** Given a string [filename], locates the corresponding wordbank text file in
      the data folder, and reads each line, storing the each line as elements in
      a list, whose elements are string lists of each word/phrase in the line *)

  val make_mappings : string -> t
  (**Given a string [filename], invokes [load_words] to load the Crossword game
     text. Generates the Crossword answer key from the loaded text, of type t*)

  val index : string -> (string * string) list -> int option
  (**Given a string value [a] and a (string * string) list [lst], returns the
     index at which this value exists in [lst]*)

  val initial : t -> a
  (**Given a Crossword answer key of type t, generates the player's display of
     type a*)

  val element_at_position : int -> (string * string) list -> string
  (**Given a int [index] and a (string * string) list [lst], returns the
     corresponding value at position [index] in [lst]*)

  val update : a -> string -> a
  (**Given a player display [current], and string [input], updates a mapping in
     [current] with [input]*)

  val update_vowel : a -> t -> string -> string -> a
  (** Relevant to games implementing a vowel hint. Given a player display
      [current], an answer key [main], a string [input] and a string [meaning],
      searches [main]'s word-meaning mappings for a value the equal to
      [meaning], returning the corresponding key. Updates this same key in
      [current] with [input]*)

  val return_hint : string -> t -> string
  (**Given a string [input] and an answer key [main], searches [main]'s mappings
     with [input] to return its corresponding value, of type string *)

  val return_vowel : string -> t -> string
  (**Relevant to games implementing a vowel hint. Given a string [input] and an
     answer key [main], searches [main]'s mappings with [input] to return its
     corresponding value, of type string *)

  val check : t -> string -> bool
  (**Given a string [input] and an answer key [main], checks if [main]'s
     mappings contains [input]*)

  val print_t : t -> unit
  (**Prints the values from one of answer key [main] for debugging purposes*)

  val print_a : a -> unit
  (**Prints the values of player display, for the player to keep track of their
     guessing progress*)

  val bindings_display : a -> (string * string) list
  (**Returns the bindings of the player display of type a*)
end

(** The following modules represent a crossword game of an [easy], [medium], and
    [hard] mode respectively *)

module HardMode : Game
module MediumMode : Game
module EasyMode : Game

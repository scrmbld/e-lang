
(** Represents a variable in the syntax tree. The content of the string is the name of the variable. *)
type e_ident = string

(** Represents a type variable in our rules. The content of the string is the name of the variable. *)
type t_ident = string

(** Used for all of our terms. This pulls double duty for both our rules, and representing the AST of our program. This is why it contains the "I" terms also -- these are used in instruction rules. Note that, in rules, the contents of literal terms E_Num and E_Str are used as identifiers instead of values. *)
type e_term =
    | E_Ident of e_ident
    | E_Num of int
    | E_Str of string
    | Plus of (e_term * e_term)
    | Times of (e_term * e_term)
    | Cat of (e_term * e_term)
    | Len of e_term
    | Let of (e_term * e_ident * e_term)

(** Converts terms into strings to make ASTs and rules readable *)
let rec string_of_e_term e = match e with
    | E_Ident x -> x
    | E_Num n -> Printf.sprintf "Num(%d)" n
    | E_Str s -> Printf.sprintf "Str(%s)" s
    | Plus (e1, e2) -> Printf.sprintf "Plus(%s; %s)" (string_of_e_term e1) (string_of_e_term e2)
    | Times (e1, e2) -> Printf.sprintf "Times(%s; %s)" (string_of_e_term e1) (string_of_e_term e2)
    | Cat (e1, e2) -> Printf.sprintf "Cat(%s; %s)" (string_of_e_term e1) (string_of_e_term e2)
    | Len e -> Printf.sprintf "Len(%s)" (string_of_e_term e)
    | Let (e1, x, e2) -> Printf.sprintf "Let(%s; %s.%s)" (string_of_e_term e1) x (string_of_e_term e2)

(** Used for all of our types. *)
type e_type =
    | T_Num
    | T_Str
    | T_Var of e_ident

(** Converts types into strings to make rules readable. *)
let string_of_e_type t = match t with
    | T_Num -> "num"
    | T_Str -> "str"
    | T_Var t -> t

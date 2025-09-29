open Ast

module StringMap = Map.Make(String)

let empty_type_context : e_type StringMap.t = StringMap.empty
let empty_eval_context : e_term StringMap.t = StringMap.empty

let rec derive_type (e : e_term) (gamma : e_type StringMap.t) : (e_type, string) result =
    let ident_type x gamma = match StringMap.find_opt x gamma with
        | None -> Error "malformed type"
        | Some t -> Ok t
    and let_type e1 x e2 gamma =
        let x_type = derive_type e1 gamma in
            match x_type with
                | Error s -> Error s
                | Ok x_type -> derive_type e2 (StringMap.add x x_type gamma)
    in match e with
    | E_Ident x -> ident_type x gamma
    | E_Str _ -> Ok T_Str
    | E_Num _ -> Ok T_Num
    | Plus (e1, e2) -> if ((derive_type e1 gamma) = Ok T_Num) && ((derive_type e2 gamma) = Ok T_Num) then Ok T_Num else Error "malformed type"
    | Times (e1, e2) -> if ((derive_type e1 gamma) = Ok T_Num) && ((derive_type e2 gamma) = Ok T_Num) then Ok T_Num else Error "malformed type"
    | Cat (e1, e2) -> if ((derive_type e1 gamma) = Ok T_Str) && ((derive_type e2 gamma) = Ok T_Str) then Ok T_Str else Error "malformed type"
    | Len (e1) -> if (derive_type e1 gamma) = Ok T_Str then Ok T_Num else Error "malformed type"
    | Let (e1, x, e2) -> let_type e1 x e2 gamma

(* TODO: cases for all of the execution rules *)
let rec eval (e : e_term) (gamma : e_term StringMap.t) : (e_term, string) result =
    let ident_x x gamma = match StringMap.find_opt x gamma with
        | Some e -> eval e gamma
        | None -> Error (Printf.sprintf "invalid identifier: %s" x)
    and plus_e1 e1 e2 gamma = match eval e1 gamma with
        | Ok e1p -> eval (Plus (e1p, e2))  gamma
        | Error s -> Error s
    and plus_e2 e1 e2 gamma = match eval e2 gamma with
        | Ok e2p -> eval (Plus (e1, e2p)) gamma
        | Error s -> Error s
    and times_e1 e1 e2 gamma = match eval e1 gamma with
        | Ok e1p -> eval (Times (e1p, e2))  gamma
        | Error s -> Error s
    and times_e2 e1 e2 gamma = match eval e2 gamma with
        | Ok e2p -> eval (Times (e1, e2p)) gamma
        | Error s -> Error s
    and cat_e1 e1 e2 gamma = match eval e1 gamma with
        | Ok e1p -> eval (Cat (e1p, e2)) gamma
        | Error s -> Error s
    and cat_e2 e1 e2 gamma = match eval e2 gamma with
        | Ok e2p -> eval (Cat (e1, e2p)) gamma
        | Error s -> Error s
    and len_e e gamma = match eval e gamma with
        | Ok ep -> eval (Len ep) gamma
        | Error s -> Error s
    and let_e e1 x e2 gamma = eval e2 (StringMap.add x e1 gamma)
    in match e with
        | E_Num n -> Ok (E_Num n)
        | E_Str s -> Ok (E_Str s)
        | E_Ident x -> ident_x x gamma
        | Plus (E_Num n1, E_Num n2) -> Ok (E_Num (n1 + n2))
        | Plus (E_Num n1, e2) -> plus_e2 (E_Num n1) e2 gamma
        | Plus (e1, e2) -> plus_e1 e1 e2 gamma
        | Times (E_Num n1, E_Num n2) -> Ok (E_Num (n1 * n2))
        | Times (E_Num n1, e2) -> times_e2 (E_Num n1) e2 gamma
        | Times (e1, e2) -> times_e1 e1 e2 gamma
        | Cat (E_Str s1, E_Str s2) -> Ok (E_Str (s1 ^ s2))
        | Cat (E_Str s1, e2) -> cat_e2 (E_Str s1) e2 gamma
        | Cat (e1, e2) -> cat_e1 e1 e2 gamma
        | Len (E_Str s) -> Ok (E_Num (String.length s))
        | Len e -> len_e e gamma
        | Let (e1, x, e2) -> let_e e1 x e2 gamma

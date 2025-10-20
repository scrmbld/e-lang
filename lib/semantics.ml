open Ast

module StringMap = Map.Make(String)

let empty_type_context : e_type StringMap.t = StringMap.empty
let empty_eval_context : e_term StringMap.t = StringMap.empty

let rec derive_type (e : e_term) (gamma : e_type StringMap.t) : (e_type, string) result =
    let ident_type x gamma = match StringMap.find_opt x gamma with
        | Some t -> Ok t
        | None -> Error (Printf.sprintf "unrecognized identifier %s" x)
    and let_type e1 x e2 gamma =
        let x_type = derive_type e1 gamma in
            match x_type with
                | Ok x_type -> derive_type e2 (StringMap.add x x_type gamma)
                | Error s -> Error s
    and lam_type t x e gamma =
        let e_type = derive_type e (StringMap.add x t gamma) in
            match e_type with
                | Ok e_t -> Ok (T_Arr (t, e_t))
                | Error s -> Error s
    in match e with
    | E_Ident x -> ident_type x gamma
    | E_Str _ -> Ok T_Str
    | E_Num _ -> Ok T_Num
    | E_Plus (e1, e2) -> if ((derive_type e1 gamma) = Ok T_Num) && ((derive_type e2 gamma) = Ok T_Num) then Ok T_Num else Error "expected nat in plus"
    | E_Times (e1, e2) -> if ((derive_type e1 gamma) = Ok T_Num) && ((derive_type e2 gamma) = Ok T_Num) then Ok T_Num else Error "expected nat in times"
    | E_Cat (e1, e2) -> if ((derive_type e1 gamma) = Ok T_Str) && ((derive_type e2 gamma) = Ok T_Str) then Ok T_Str else Error "expected string in cat"
    | E_Len (e1) -> if (derive_type e1 gamma) = Ok T_Str then Ok T_Num else Error "expected string in len"
    | E_Let (e1, x, e2) -> let_type e1 x e2 gamma
    | E_Lam (t, x, e) -> lam_type t x e gamma
    | E_App (e1, e2) -> match derive_type e1 gamma with
            | Ok (T_Arr (_, e2')) -> Ok e2'
            | Ok _ -> Error "expected arrow in app"
            | Error s -> Error s

let rec step (e : e_term) (delta : e_term StringMap.t) : ((e_term * e_term StringMap.t), string) result =
    let ident_x x delta = match StringMap.find_opt x delta with
        | Some e -> Ok (e, delta)
        | None -> Error (Printf.sprintf "invalid identifier: %s" x)
    and plus_e1 e1 e2 delta = match step e1 delta with
        | Ok (e1p, gp) -> Ok (E_Plus (e1p, e2), gp)
        | err -> err
    and plus_e2 e1 e2 delta = match step e2 delta with
        | Ok (e2p, gp) -> Ok (E_Plus (e1, e2p), gp)
        | err -> err
    and times_e1 e1 e2 delta = match step e1 delta with
        | Ok (e1p, gp) -> Ok (E_Times (e1p, e2), gp)
        | err -> err
    and times_e2 e1 e2 delta = match step e2 delta with
        | Ok (e2p, gp) -> Ok (E_Times (e1, e2p), gp)
        | err -> err
    and cat_e1 e1 e2 delta = match step e1 delta with
        | Ok (e1p, gp) -> Ok (E_Cat (e1p, e2), gp)
        | err -> err
    and cat_e2 e1 e2 delta = match step e2 delta with
        | Ok (e2p, gp) -> Ok (E_Cat (e1, e2p), gp)
        | err -> err
    and len_e e delta = match step e delta with
        | Ok (ep, gp) -> Ok (E_Len (ep), gp)
        | err -> err
    in match e with
        | E_Ident x -> ident_x x delta
        | E_Num n -> Ok ((E_Num n), delta)
        | E_Str s -> Ok (E_Str s, delta)
        | E_Plus (E_Num n1, E_Num n2) -> Ok (E_Num (n1 + n2), delta)
        | E_Plus (E_Num n1, e2) -> plus_e2 (E_Num n1) e2 delta
        | E_Plus (e1, e2) -> plus_e1 e1 e2 delta
        | E_Times (E_Num n1, E_Num n2) -> Ok (E_Num (n1 * n2), delta)
        | E_Times (E_Num n1, e2) -> times_e2 (E_Num n1) e2 delta
        | E_Times (e1, e2) -> times_e1 e1 e2 delta
        | E_Cat (E_Str s1, E_Str s2) -> Ok (E_Str (s1 ^ s2), delta)
        | E_Cat (E_Str s1, e2) -> cat_e2 (E_Str s1) e2 delta
        | E_Cat (e1, e2) -> cat_e1 e1 e2 delta
        | E_Len (E_Str s) -> Ok (E_Num (String.length s), delta)
        | E_Len e -> len_e e delta
        | E_Let (e1, x, e2) -> Ok (e2, (StringMap.add x e1 delta))

let rec eval (e : e_term) (delta : e_term StringMap.t) : (e_term, string) result =
    let ident_x x delta = match StringMap.find_opt x delta with
        | Some e -> eval e delta
        | None -> Error (Printf.sprintf "invalid identifier: %s" x)
    and plus_e1 e1 e2 delta = match eval e1 delta with
        | Ok e1p -> eval (E_Plus (e1p, e2))  delta
        | Error s -> Error s
    and plus_e2 e1 e2 delta = match eval e2 delta with
        | Ok e2p -> eval (E_Plus (e1, e2p)) delta
        | Error s -> Error s
    and times_e1 e1 e2 delta = match eval e1 delta with
        | Ok e1p -> eval (E_Times (e1p, e2))  delta
        | Error s -> Error s
    and times_e2 e1 e2 delta = match eval e2 delta with
        | Ok e2p -> eval (E_Times (e1, e2p)) delta
        | Error s -> Error s
    and cat_e1 e1 e2 delta = match eval e1 delta with
        | Ok e1p -> eval (E_Cat (e1p, e2)) delta
        | Error s -> Error s
    and cat_e2 e1 e2 delta = match eval e2 delta with
        | Ok e2p -> eval (E_Cat (e1, e2p)) delta
        | Error s -> Error s
    and len_e e delta = match eval e delta with
        | Ok ep -> eval (E_Len ep) delta
        | Error s -> Error s
    and let_e e1 x e2 delta = eval e2 (StringMap.add x e1 delta)
    in match e with
        | E_Num n -> Ok (E_Num n)
        | E_Str s -> Ok (E_Str s)
        | E_Ident x -> ident_x x delta
        | E_Plus (E_Num n1, E_Num n2) -> Ok (E_Num (n1 + n2))
        | E_Plus (E_Num n1, e2) -> plus_e2 (E_Num n1) e2 delta
        | E_Plus (e1, e2) -> plus_e1 e1 e2 delta
        | E_Times (E_Num n1, E_Num n2) -> Ok (E_Num (n1 * n2))
        | E_Times (E_Num n1, e2) -> times_e2 (E_Num n1) e2 delta
        | E_Times (e1, e2) -> times_e1 e1 e2 delta
        | E_Cat (E_Str s1, E_Str s2) -> Ok (E_Str (s1 ^ s2))
        | E_Cat (E_Str s1, e2) -> cat_e2 (E_Str s1) e2 delta
        | E_Cat (e1, e2) -> cat_e1 e1 e2 delta
        | E_Len (E_Str s) -> Ok (E_Num (String.length s))
        | E_Len e -> len_e e delta
        | E_Let (e1, x, e2) -> let_e e1 x e2 delta

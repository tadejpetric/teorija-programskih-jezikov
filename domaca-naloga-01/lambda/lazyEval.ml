module S = Syntax


let rec is_value = function
  | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _  |  S.Nil -> true
  | S.Var _ | S.Plus _ | S.Minus _ | S.Times _ | S.Equal _ | S.Less _ | S.Greater _
  | S.IfThenElse _ | S.Apply _ | S.Fst _ | S.Snd _ | S.Match _ -> false
  | S.Cons (car, cdr) -> true
  | S.Pair (x, y) -> true


let rec eval_exp = function
  | S.Var x -> failwith "Expected a closed term"
  | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ as e -> e
  (*| S.Pair (e1, e2) when not (is_value e1 && is_value e2) ->
    let xe1 = eval_exp e1
    and xe2 = eval_exp e2
    in S.Pair(xe1, xe2)
  | S.Cons (e1, e2) when not (is_value e1 && is_value e2) ->
    let xe1 = eval_exp e1
    and xe2 = eval_exp e2
    in S.Cons(xe1, xe2)*)
  | S.Pair _ as e -> e
  | S.Cons _ as e -> e
  | S.Plus (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Int (n1 + n2)
  | S.Minus (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Int (n1 - n2)
  | S.Times (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Int (n1 * n2)
  | S.Equal (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Bool (n1 = n2)
  | S.Less (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Bool (n1 < n2)
  | S.Greater (e1, e2) ->
      let n1 = eval_int e1
      and n2 = eval_int e2
      in S.Bool (n1 > n2)
  | S.IfThenElse (e, e1, e2) ->
      begin match eval_exp e with
      | S.Bool true -> eval_exp e1
      | S.Bool false -> eval_exp e2
      | _ -> failwith "Boolean expected"
      end
  | S.Apply (e1, e2) ->
      let f = eval_exp e1
      and v = e2
      in
      begin match f with
        | S.Lambda (x, e) -> eval_exp (S.subst [(x, v)] e)
        | S.RecLambda (f, x, e) as rec_f -> eval_exp (S.subst [(f, rec_f); (x, v)] e)
        | _ -> failwith "Function expected"
      end
  | S.Nil -> S.Nil
  | S.Fst (S.Cons (x, y)) -> eval_exp x (* We get head and tail on lists *)
  | S.Fst (S.Pair (x, y)) -> eval_exp x
  | S.Snd (S.Cons (x, y)) -> eval_exp y
  | S.Snd (S.Pair (x, y)) -> eval_exp y
  (*if it's not a pair, we try to simplify until we get one, then continue execution *)
  | S.Fst x -> eval_exp (S.Fst (eval_exp x)) 
  | S.Snd x -> eval_exp (S.Snd (eval_exp x))
  | S.Match (e, e1, x, xs, e2) ->
    begin match eval_exp e with
      | S.Nil -> e1
      | S.Cons (car, cdr) ->
        let newcont = S.subst [(x, car)] e2
        in eval_exp (S.subst [(xs, cdr)] newcont)
      | a when is_value a -> failwith "List expected in match clause"
      | a -> S.Match(eval_exp a, e1, x, xs, e2)
    end


and eval_int e =
  match eval_exp e with
  | S.Int n -> n
  | _ -> failwith "Integer expected"


let rec step = function
  | S.Var _ | S.Int _ | S.Bool _ | S.Lambda _ | S.RecLambda _ -> failwith "Expected a non-terminal expression got whatever"
  | S.Nil -> S.Nil
  | S.Cons _ as x -> x (*failwith "Expected non-terminal expression got list"*)
  | S.Pair _ as x -> x (*failwith "Expected non-terminal expression got pair"*)
  | S.Plus (S.Int n1, S.Int n2) -> S.Int (n1 + n2)
  | S.Plus (S.Int n1, e2) -> S.Plus (S.Int n1, step e2)
  | S.Plus (e1, e2) -> S.Plus (step e1, e2)
  | S.Minus (S.Int n1, S.Int n2) -> S.Int (n1 - n2)
  | S.Minus (S.Int n1, e2) -> S.Minus (S.Int n1, step e2)
  | S.Minus (e1, e2) -> S.Minus (step e1, e2)
  | S.Times (S.Int n1, S.Int n2) -> S.Int (n1 * n2)
  | S.Times (S.Int n1, e2) -> S.Times (S.Int n1, step e2)
  | S.Times (e1, e2) -> S.Times (step e1, e2)
  | S.Equal (S.Int n1, S.Int n2) -> S.Bool (n1 = n2)
  | S.Equal (S.Int n1, e2) -> S.Equal (S.Int n1, step e2)
  | S.Equal (e1, e2) -> S.Equal (step e1, e2)
  | S.Less (S.Int n1, S.Int n2) -> S.Bool (n1 < n2)
  | S.Less (S.Int n1, e2) -> S.Less (S.Int n1, step e2)
  | S.Less (e1, e2) -> S.Less (step e1, e2)
  | S.Greater (S.Int n1, S.Int n2) -> S.Bool (n1 > n2)
  | S.Greater (S.Int n1, e2) -> S.Greater (S.Int n1, step e2)
  | S.Greater (e1, e2) -> S.Greater (step e1, e2)
  | S.IfThenElse (S.Bool b, e1, e2) -> if b then e1 else e2
  | S.IfThenElse (e, e1, e2) -> S.IfThenElse (step e, e1, e2)
  | S.Apply (S.Lambda (x, e), v) -> S.subst [(x, v)] e
  | S.Apply (S.RecLambda (f, x, e) as rec_f, v) -> S.subst [(f, rec_f); (x, v)] e
  | S.Apply (e1, e2) -> S.Apply (step e1, e2)
  | S.Fst (S.Pair (x, y)) -> x
  | S.Fst (S.Cons (car, cdr)) -> car (* acts as head *)
  | S.Fst x when is_value x -> failwith "Expected list or pair for Fst, got some other value"
  | S.Fst x -> S.Fst (step x)
  | S.Snd (S.Pair (x, y)) -> y
  | S.Snd (S.Cons (car, cdr)) -> cdr (* acts as tail *)
  | S.Snd x when is_value x -> failwith "Expected list or pair for Snd, got some other value"
  | S.Snd x -> S.Snd (step x)
  | S.Match (e, e1, x, xs, e2) ->
    begin match e with
      | S.Nil -> e1
      | S.Cons (car, cdr) ->
        let newcont = S.subst [(x, car)] e2
        in (S.subst [(xs, cdr)] newcont)
      | a when is_value a -> failwith "List expected in match clause"
      | a -> S.Match(step a, e1, x, xs, e2)
    end


let big_step e =
  let v = eval_exp e in
  print_endline (S.string_of_exp v)

let rec small_step e =
  print_endline (S.string_of_exp e);
  if not (is_value e) then
    (print_endline "  ~>";
    small_step (step e))

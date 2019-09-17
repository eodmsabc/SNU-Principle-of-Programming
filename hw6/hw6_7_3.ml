exception TODO

type pgm = cmd
and cmd =
	| ASSIGN of exp
	| SEQUENCE of cmd * cmd
	| REPEAT of cmd
	| CHOICE of cmd * cmd
	| EQ of exp * cmd
	| NEQ of exp * cmd
and exp =
	| NUM of int
	| ADD of exp * exp
	| SUB of exp * exp
	| VAR

type state = int

let aso = (fun a b -> a - b)

(*let rec lstr = function
	| [] -> ""
	| h :: t -> (string_of_int h) ^ " " ^ (lstr t)*)

let rec merge l1 l2 =
	match l1 with
	| [] -> l2
	| h :: t ->
		if List.mem h l2 then merge t l2
		else merge t (h :: l2)

let exeval (p:pgm) (st:state): state list =
	let sort l = List.sort_uniq (fun a b -> a - b) l in

(*	let rec lstr = function (* for debugging *)
		| [] -> ""
		| h :: t -> (string_of_int h) ^ " " ^ (lstr t)
	in*)

	let rec evalone e st =
		match e with
		| NUM n -> n
		| ADD (e1, e2) -> (evalone e1 st) + (evalone e2 st)
		| SUB (e1, e2) -> (evalone e1 st) - (evalone e2 st)
		| VAR -> st
	in
	let eval e stl = List.map (evalone e) stl in

	let checkend r = if (r > 5) || (r < (-5)) then true else false in
	let insertf stl res l =
		let rec insaux stl res l =
			match (stl, res) with
			| (h1 :: t1, h2 :: t2) ->
				if (checkend h2) then insaux t1 t2 (h1 :: l)
				else
					let (a, b) = insaux t1 t2 l in
					(h2 :: a, b)
			| _ -> (res, l)
		in
		let (a, b) = insaux stl res l in
		(sort a, sort b)
	in

	let rec exv p stl l =
		if stl = [] then ([], l) else
		match p with
		| ASSIGN e ->
(*			let res = eval e stl in
			insert stl res l *)
			insertf stl (eval e stl) l
		| SEQUENCE (c1, c2) ->
			let (nr, nl) = exv c1 stl l in
			let (nnr, nnl) = insertf stl nr l in
			exv c2 nnr nnl
		| REPEAT c ->
			let (a, b) = exv c stl l in
			let nxtl = sort (stl @ a) in
			if (sort stl) = nxtl then (nxtl, b)
			else exv (REPEAT c) nxtl b
		| CHOICE (c1, c2) ->
			let (r1, l1) = exv c1 stl l in
			let (r2, l2) = exv c2 stl l in
			(sort (r1 @ r2), sort (l1 @ l2))
		| EQ (e, c) ->
			let rec eqv e lst =
				match lst with
				| [] -> []
				| h :: t -> if (evalone e h) = h then h :: (eqv e t) else eqv e t
			in
			let nl = eqv e stl in
			exv c nl l
		| NEQ (e, c) ->
			let rec neqv e lst =
				match lst with
				| [] -> []
				| h :: t -> if (evalone e h) = h then neqv e t else h :: (neqv e t)
			in
			let nl = neqv e stl in
			exv c nl l
	in
	let (r, l) = exv p [st] [] in
	sort (r @ l)
(*
let p1 =
  SEQUENCE (ASSIGN (NUM 1),
            REPEAT
              (CHOICE (EQ (NUM 1, ASSIGN (ADD (VAR,NUM 1))),
                       NEQ (NUM 1, ASSIGN (SUB (VAR,NUM 1))))
           ))

let p2 = REPEAT (ASSIGN (ADD (VAR,NUM 1)))

let p3 = SEQUENCE (ASSIGN (NUM 1), ASSIGN (NUM 2))

let p4 = SEQUENCE (ASSIGN (NUM 1), CHOICE (ASSIGN (ADD (VAR, NUM 2)), ASSIGN (ADD (VAR, NUM 3))))

let p5 = SEQUENCE (ASSIGN (NUM 1),
SEQUENCE (
CHOICE (ASSIGN (ADD (VAR, NUM 2)), ASSIGN (ADD (VAR, NUM 3))),
CHOICE (ASSIGN (ADD (VAR, NUM 7)), ASSIGN (SUB (VAR, NUM 6)))))

let p6 = (REPEAT (SEQUENCE ((CHOICE ((ASSIGN (ADD (VAR, NUM 1))), (ASSIGN (SUB (VAR, NUM 1))))), (ASSIGN (ADD (VAR, VAR))))))

let p7 = SEQUENCE (REPEAT (CHOICE (
EQ (NUM 1, ASSIGN (ADD (VAR,NUM 1))),
NEQ (NUM 1, ASSIGN (SUB (VAR,NUM 1))))), EQ (NUM 1, ASSIGN (ADD (VAR, NUM 1))))*)



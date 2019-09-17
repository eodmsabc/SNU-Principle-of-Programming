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

let rec lstr = function
	| [] -> ""
	| h :: t -> (string_of_int h) ^ " " ^ (lstr t)

let rec merge l1 l2 =
	match l1 with
	| [] -> l2
	| h :: t ->
		if List.mem h l2 then merge t l2
		else merge t (h :: l2)

let rec exeval (p:pgm) (st:state): state list =
	let rec evalone e st =
		match e with
		| NUM n -> n
		| ADD (e1, e2) -> evalone e1 st + evalone e2 st
		| SUB (e1, e2) -> evalone e1 st - evalone e2 st
		| VAR -> st
	in
	let rec eval e stl =
		List.map (evalone e) stl
	in
	let checkend t = if (t > 5) || (t < (-5)) then true else false in
	let rec check r =
		match r with
		| [] -> []
		| h :: t -> if (checkend h) then (check t) else h :: (check t)
	in
	let rec insert r l =
		match r with
		| [] -> l
		| h :: t -> if List.mem h l then insert t l else insert t (h :: l)
	in
	let rec eqv e stl =
		match stl with
		| [] -> []
		| h :: t ->
			if (evalone e h) = h then h :: (eqv e t)
			else (eqv e t)
	in
	let rec neqv e stl =
		match stl with
		| [] -> []
		| h :: t ->
			if (evalone e h) = h then (eqv e t)
			else h :: (eqv e t)
	in
	let rec elst p stl l =
		(*let _ = print_endline "stl" in
		let _ = print_endline (lstr stl) in
		let _ = print_endline "l" in
		let _ = print_endline (lstr l) in
		*)
		match p with
		| ASSIGN e ->
(*			let _ = print_endline "assign" in*)
			let res = eval e stl in
(*			let _ = print_endline (lstr res) in
			let _ = print_endline "assign end" in*)
(*			let rr = check res in*)
		(*	let newl = insert rr l in*)
(*			(rr, newl)*)
		| SEQUENCE (c1, c2) ->
			let _ = print_endline "sequence" in
			let (newr, newl) = elst c1 stl l in
			let rr = check newr in
			elst c2 rr newl
		| REPEAT c ->
			let _ = print_endline "repeat" in
		(*	if stl = [] then ([], l)
			else*)
			let (newr, newl) = elst c stl l in
			(*let _ = print_endline "repeat stl" in
			let _ = print_endline (lstr stl) in
			let _ = print_endline "repeat newr" in
			let _ = print_endline (lstr newr) in*)
			let ml = merge stl newr in
			let s1 = List.sort_uniq aso stl in
			let s2 = List.sort_uniq aso ml in
			if s1 = s2 then (s2, newl)
			else elst (REPEAT c) ml newl
		| CHOICE (c1, c2) ->
			let _ = print_endline "choice" in
			let (a, newl) = elst c1 stl l in
			let (b, nnwl) = elst c2 stl newl in
			(a @ b, nnwl)
		| EQ (e, c) ->
			let _ = print_endline "eq" in
			let eql = eqv e stl in
			elst c eql l
		| NEQ (e, c) ->
			let _ = print_endline "neq" in
			let neql = neqv e stl in
			elst c neql l
	in
	let (_, l) = elst p [st] [] in
	List.sort (fun a b -> a - b) l

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

exception TODO

type ae =
	| CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec diff (e: ae) (x: string): ae =
	match e with
	| CONST c -> CONST 0
	| VAR y -> if x = y then CONST 1 else CONST 0
	| POWER (y, i) -> if x = y then TIMES [CONST i; POWER (y, i-1)] else CONST 0
	| TIMES [] -> CONST 0
	| TIMES (h :: []) -> diff h x
	| TIMES (h :: t) -> SUM [TIMES ((diff h x) :: t); TIMES [h; diff (TIMES t) x]]
	| SUM [] -> CONST 0
	| SUM (h :: []) -> diff h x
	| SUM (h :: t) -> SUM [diff h x; diff (SUM t) x]

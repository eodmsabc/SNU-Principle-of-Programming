module type SKI = sig
  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)
  val react: liquid -> liquid
  val pprint: liquid -> string
end

module SkiLiquid : SKI = struct
	exception ETODO

	type liquid =
		| S
		| K
		| I
		| V of string (* varible *)
		| M of liquid * liquid (* mix of two liquids *)

	let rec react: liquid -> liquid = fun l ->
		match l with
		| S -> S
		| K -> K
		| I -> I
		| V x -> V x
		| M (I, e) -> react e
		| M (M (K, e), e') -> react e
		| M (M (M (S, e), e'), e'') -> react (M (M (e, e''), M (e', e'')))
		| M (s1, s2) ->
			let after = M (react s1, react s2) in
			if l = after then l else react after

	let rec pprint: liquid -> string = fun l ->
		match l with
		| S -> "S"
		| K -> "K"
		| I -> "I"
		| V x -> x
		| M (s1, s2) -> "(" ^ (pprint s1) ^ " " ^ (pprint s2) ^ ")"
end

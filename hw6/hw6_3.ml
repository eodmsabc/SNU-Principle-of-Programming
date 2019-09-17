exception TODO

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let rec drop (t: tourna) (d: team): string =
	let rec paren t =
		match t with
	 	| LEAF Korea -> "Korea"
	    | LEAF France -> "France"
	    | LEAF Usa -> "Usa"
	    | LEAF Brazil -> "Brazil"
	    | LEAF Japan -> "Japan"
	    | LEAF Nigeria -> "Nigeria"
	    | LEAF Cameroon -> "Cameroon"
	    | LEAF Poland -> "Poland"
	    | LEAF Portugal -> "Portugal"
	    | LEAF Italy -> "Italy"
	    | LEAF Germany -> "Germany"
	    | LEAF Norway -> "Norway"
	    | LEAF Sweden -> "Sweden"
	    | LEAF England -> "England"
	    | LEAF Argentina -> "Argentina"
	    | NODE (a, b) -> "(" ^ (paren a) ^ " " ^ (paren b) ^ ")"
	in
	let rec del t d =
		match t with
		| LEAF l -> LEAF l
		| NODE (a, b) ->
			if a = LEAF d then del b d
			else if b = LEAF d then del a d
			else NODE (del a d, del b d)
	in
	match t with
	| LEAF l -> if l = d then "" else paren t
	| _ -> paren (del t d)
		


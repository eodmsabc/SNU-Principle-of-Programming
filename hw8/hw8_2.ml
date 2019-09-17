module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module StringSetQ : Queue with type element = string = 
struct 
	type element = string
	type queue = element list * element list
	exception EMPTY_Q
	let emptyq = ([], [])
	let enq ((l, r), e) =
		if (List.mem e l) || (List.mem e r) then (l, r)
		else (e::l, r)
	let rec deq = function
		| ([], []) -> raise (EMPTY_Q)
		| (l, []) -> deq ([], List.rev l)
		| (l, e::r) -> (e, (l, r))
end

module StringSetQQ : Queue with type element = StringSetQ.queue = 
struct
	type element = StringSetQ.queue
	type queue = element list * element list
	exception EMPTY_Q
	let emptyq = ([], [])

	let enq ((l, r), e) =
        let rec sameQ q1 q2 =
            let rec qlist q =
                try
                    let (e, left) = StringSetQ.deq q in
                    (e :: (qlist left))
                with StringSetQ.EMPTY_Q -> []
            in
           (qlist q1) = (qlist q2)
        in
        let rec qmem q l =
            match l with
            | [] -> false
            | h :: t -> if (sameQ q h) then true else qmem q t
        in
		if (qmem e l) || (qmem e r) then (l, r)
		else (e::l, r)
	let rec deq = function
		| ([], []) -> raise (EMPTY_Q)
		| (l, []) -> deq ([], List.rev l)
		| (l, e::r) -> (e, (l, r))
end

module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module StringQ : Queue with type element = string = 
struct
	type element = string
	type queue = element list * element list
	exception EMPTY_Q
	let emptyq = ([], [])
	let enq ((l, r), e) = (e::l, r)
	let rec deq = function
		| ([], []) -> raise (EMPTY_Q)
		| (l, []) -> deq ([], List.rev l)
		| (l, e::r) -> (e, (l, r))
end

module StringQQ : Queue with type element = StringQ.queue = 
struct
	type element = StringQ.queue
	type queue = element list * element list
	exception EMPTY_Q
	let emptyq = ([], [])
	let enq ((l, r), e) = (e::l, r)
	let rec deq = function
		| ([], []) -> raise (EMPTY_Q)
		| (l, []) -> deq ([], List.rev l)
		| (l, e::r) -> (e, (l, r))
end

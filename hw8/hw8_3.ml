module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module type ArgTy = 
sig
  type t
  val is_eq : t -> t -> bool
end

module QueueMake (Arg: ArgTy) 
  : Queue with type element = Arg.t =
struct
    type element = Arg.t
    type queue = Arg.t list * Arg.t list
    exception EMPTY_Q
    let emptyq = ([], [])
    let rec enq ((l, r), e) = (e::l, r)
    let rec deq = function
        | ([], []) -> raise (EMPTY_Q)
        | (l, []) -> deq ([], List.rev l)
        | (l, e::r) -> (e, (l, r))
end

type design = TURTLE | WAVE | DRAGON    (* three design patterns *)
type orientation = NW | NE | SE | SW
type box = BOX of orientation * design | GLUED of box * box * box * box

module type FRAME =
sig
  val box: box
  val rotate: box -> box                (* roatate box M to 3 to W to E *)
  val pp: box -> int * int -> unit      (* pretty printer *)
  val size: int
end

module BasicFrame (Design: sig val design: design end): FRAME = 
struct 
  exception NON_BASIC_BOX
  let box = BOX (NW, Design.design)     (* a box is defined *)
  let rec rotate (b:box) : box =
    match b with
(*    | BOX (dir, des) -> Rotate.rotate (BOX (dir, des))*)
    | BOX (NW, k) -> BOX (NE, k)
    | BOX (NE, k) -> BOX (SE, k)
    | BOX (SE, k) -> BOX (SW, k)
    | BOX (SW, k) -> BOX (NW, k)
    | _ -> raise NON_BASIC_BOX
  let pp b center = 
    match b with
    | BOX (NW,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (NE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SW,x) -> ()                  (* dummy, fill it if you want *)
    | _ -> raise NON_BASIC_BOX
  let size = 1
end

module Rotate (Box:FRAME) : FRAME =
struct 
  let box =
    let rec rotate (b:box) : box =
      match b with
      | BOX (NW, k) -> BOX (NE, k)
      | BOX (NE, k) -> BOX (SE, k)
      | BOX (SE, k) -> BOX (SW, k)
      | BOX (SW, k) -> BOX (NW, k)
      | GLUED (a, b, c, d) -> GLUED (rotate d,
                                     rotate a,
                                     rotate b,
                                     rotate c)
    in
    rotate Box.box
  let rec rotate (b:box) : box =
    match b with
    | BOX (NW, k) -> BOX (NE, k)
    | BOX (NE, k) -> BOX (SE, k)
    | BOX (SE, k) -> BOX (SW, k)
    | BOX (SW, k) -> BOX (NW, k)
    | GLUED (a, b, c, d) -> GLUED (rotate d,
                                   rotate a,
                                   rotate b,
                                   rotate c)
  let pp b center = ()                  (* dummy, fill it if you want *)
  let size = Box.size
end

module Glue (Nw:FRAME) (Ne:FRAME) (Se:FRAME) (Sw:FRAME) : FRAME =
struct
  exception DIFFERENT_SIZED_BOXES
  let box =
    if (Nw.size = Ne.size) && (Ne.size = Se.size) && (Se.size = Sw.size)
    then GLUED (Nw.box, Ne.box, Se.box, Sw.box)
    else raise DIFFERENT_SIZED_BOXES
  let rec rotate (b:box) : box =
    match b with
    | BOX (NW, k) -> BOX (NE, k)
    | BOX (NE, k) -> BOX (SE, k)
    | BOX (SE, k) -> BOX (SW, k)
    | BOX (SW, k) -> BOX (NW, k)
    | GLUED (a, b, c, d) -> GLUED (rotate d,
                                   rotate a,
                                   rotate b,
                                   rotate c)

  let pp b center = ()
  let size = Nw.size + 1
end

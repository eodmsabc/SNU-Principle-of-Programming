open CommonGrade
open Hw6_7

let same_list a b =
    (List.for_all (fun x -> List.mem x b) a) && (List.for_all (fun x -> List.mem x a) b)

let p1 = 
    SEQUENCE (ASSIGN (NUM 1),
        REPEAT
          (CHOICE (EQ (NUM 1, ASSIGN (ADD (VAR,NUM 1))),
               NEQ (NUM 1, ASSIGN (SUB (VAR,NUM 1))))
          ))

let p2 = REPEAT (ASSIGN (ADD (VAR,NUM 1)))

let p3 = SEQUENCE(ASSIGN(NUM(1)),REPEAT(CHOICE(EQ(NUM(1),ASSIGN(ADD(VAR,NUM(1)))),NEQ(NUM(1),ASSIGN(SUB(VAR,NUM(1)))))))

let p4 = SEQUENCE(ASSIGN(NUM(1)),REPEAT(ASSIGN(ADD(VAR,NUM(1)))))

let p5 = REPEAT(CHOICE(ASSIGN(ADD(VAR,NUM(1))),ASSIGN(SUB(VAR,NUM(1)))))

let p6 = REPEAT(CHOICE(EQ(NUM(1),ASSIGN(ADD(VAR,NUM(1)))),NEQ(NUM(1),ASSIGN(SUB(VAR,NUM(1))))))

let p7 = SEQUENCE(REPEAT(CHOICE(ASSIGN(ADD(VAR,NUM(1))),EQ(NUM(5),ASSIGN(SUB(VAR,NUM(1)))))),REPEAT(CHOICE(ASSIGN(SUB(VAR,NUM(1))),EQ(NUM(-5),ASSIGN(ADD(VAR,NUM(1)))))))

let _ = output (fun () -> same_list (exeval p1 (-1)) [1; 2])
let _ = output (fun () -> same_list (exeval p2 1) [1; 2; 3; 4; 5])
let _ = output (fun () -> same_list (exeval p3 1) [1; 2])
let _ = output (fun () -> same_list (exeval p4 1) [1; 2; 3; 4; 5])
let _ = output (fun () -> same_list (exeval p5 0) [-5; -4; -3; -2; -1; 0; 1; 2; 3; 4; 5])
let _ = output (fun () -> same_list (exeval p6 5) [1; 2; 3; 4; 5])
let _ = output (fun () -> same_list (exeval p7 (-1)) [-5; -4; -3; -2; -1; 0; 1; 2; 3; 4; 5])

let prg1 = SEQUENCE (ASSIGN (NUM 1), REPEAT ( CHOICE (EQ (NUM 1, ASSIGN (ADD (VAR, NUM 1) )), (NEQ (NUM 1, ASSIGN (SUB (VAR, NUM 1) ) ) ))))
let _ = output (fun () -> same_list (exeval prg1 0) [1; 2])

let prg2 = SEQUENCE (ASSIGN (NUM 1), REPEAT (ASSIGN (ADD (VAR, NUM 1))) )     (* x=1; (x=x+1); *) 
let _ = output (fun () -> same_list (exeval prg2 0) [1;2;3;4;5]) (* [1 2 3 4 5] *) 

let prg3 = SEQUENCE ( ASSIGN (NUM 1),   REPEAT (SEQUENCE( EQ ((NUM (-5)), ASSIGN (NUM 5)),      ASSIGN (SUB (VAR, NUM 1)) )))
let _ = output (fun () -> same_list (exeval prg3 0) [-5; -4; -3; -2; -1; 0;1;2;3;4]) (* [-5 -4 -3 -2 -1 0 1 2 3 4] *) 

let prg4 = SEQUENCE( ASSIGN (NUM 6), REPEAT( ASSIGN (ADD (VAR, NUM 1))) )
let _ = output (fun () -> same_list (exeval prg4 1) [1]) (* [1] *) 

let prg5 = REPEAT( ASSIGN (NUM 3) )
let _ = output (fun () -> same_list (exeval prg5 2) [2; 3]) (* [2 3] *) 

let prg6 = CHOICE( CHOICE( REPEAT (ASSIGN (ADD (VAR, VAR))), ASSIGN (NUM (-2))), ASSIGN (NUM (-5)) )
let _ = output (fun () -> same_list (exeval prg6 1) [-5; -2 ;1 ;2 ;4])    (* [-5 -2 1 2 4] *) 

let prg7 =  REPEAT( REPEAT( REPEAT( REPEAT (REPEAT (CHOICE( ASSIGN ( ADD (VAR, NUM 1)), ASSIGN ( SUB (VAR, NUM 1) ) ))) ) ));; 
(*let prg7 = REPEAT (CHOICE( ASSIGN ( ADD (VAR, NUM 1)), ASSIGN ( SUB (VAR, NUM 1) ) ));; 
  same as above, but if too slow then try me instead :) *) 
let _ = output (fun () -> same_list (exeval prg7 4) [-5; -4; -3; -2; -1; 0;1;2;3;4;5])    (* [-5 -4 -3 -2 -1 0 1 2 3 4 5] *) 

let prg8 = SEQUENCE(EQ( NUM 3, prg5), prg6)
let _ = output (fun () -> same_list (exeval prg8 0) [-5; -2; 0])    (* [-5 -2 0] *) 

let prg9 = SEQUENCE( CHOICE( 
        CHOICE(ASSIGN (NUM 1), ASSIGN(ADD (NUM 3, VAR))), 
        CHOICE(ASSIGN (NUM (-1)), ASSIGN(NUM (-4)) ) 
), ASSIGN (NUM (-4)))
let _ = output (fun () -> same_list (exeval prg9 5) [-4;5])    (* [-4 5] *) 
let _ = output (fun () -> same_list (exeval prg9 2) [-4])    (* [-4] *) 

let prg10 = SEQUENCE( ASSIGN (NUM 2), CHOICE( REPEAT ( NEQ (ADD (VAR, NUM 1), prg5) ), ASSIGN (NUM 4)) )
let _ = output (fun () -> same_list (exeval prg10 0) [2; 3; 4])   (* [2 3 4] *) 

let prg11 = CHOICE( CHOICE(prg4, prg1), REPEAT( CHOICE(prg6, prg10) ) );; 
let _ = output (fun () -> same_list (exeval prg11 0) [-5;-4;-2;0;1;2;3;4])   (* [-5 -4 -2 0 1 2 3 4] *) 


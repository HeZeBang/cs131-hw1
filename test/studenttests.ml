open Util.Assert
open Hellocaml

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

(* Problem 1-1 *)
let cube_test1 : int = cube 2 (* should evaluate to 8 *)
let cube_test2 : int = cube 3 (* should evaluate to 27 *)

(* Problem 1-2 *)
let cents_of_test2 : int = cents_of 0 0 0 0 (* should evaluate to 0 *)

(* Problem 1-3 *)
let prob3_case2_test : int = prob3_case2 10 (* should evaluate to 32 *)

(* PART 2: Tuples, Generics, Pattern Matching *)
(* Example pattern matching against tuples *)
let first_of_three_test : int = first_of_three (1, true, "hello")

(* should evaluate to 1 *)
let second_of_three_test : bool = second_of_three (1, true, "hello")
(* should evaluate to true *)

(* Problem 2-1 *)
let third_of_three_test : string = third_of_three (1, true, "hello")
(* should evaluate to "hello" *)

(* Problem 2-2 *)
let compose_pair_test : int =
  compose_pair ((fun x -> x + 1), fun x -> x * 2) 3 (* should evaluate to 7 *)

(* PART 3: Lists and Recursion *)
(* Problem 3-1 *)
let vars_of_test1 : string list = vars_of e1 (* should evaluate to [] *)
let vars_of_test2 : string list = vars_of e2 (* should evaluate to ["x"] *)
let vars_of_test3 : string list = vars_of e3 (* should evaluate to ["x"; "y"] *)

(* Problem 3-2 *)
let list_to_mylist_test1 : int mylist = list_to_mylist [ 1; 2; 3 ]

(* should evaluate to Cons (1, Cons (2, Cons (3, Nil))) *)
let list_to_mylist_test2 : string mylist = list_to_mylist [ "a"; "b"; "c" ]
(* should evaluate to Cons ("a", Cons ("b", Cons ("c", Nil))) *)

(* Problem 3-3 *)
let append_test1 : int list = append [ 1; 2; 3 ] [ 4; 5 ]

(* should evaluate to [1; 2; 3; 4; 5] *)
let append_test2 : string list = append [ "a"; "b"; "c" ] [ "d"; "e" ]
(* should evaluate to ["a"; "b"; "c"; "d"; "e"] *)

(* Problem 3-4 *)
let rev_test1 : int list = rev [ 1; 2; 3 ] (* should evaluate to [3; 2; 1] *)
let rev_test2 : string list = rev [ "a"; "b"; "c" ]
(* should evaluate to ["c"; "b"; "a"] *)

(* Problem 3-5 *)
let insert_test1 : int list = insert 3 [ 1; 2; 4; 5 ]

(* should evaluate to [1; 2; 3; 4; 5] *)
let insert_test2 : string list = insert "c" [ "a"; "b"; "d"; "e" ]
(* should evaluate to ["a"; "b"; "c"; "d"; "e"] *)

(* Problem 3-6 *)
let union_test1 : int list = union [ 1; 2; 3 ] [ 2; 3; 4 ]

(* should evaluate to [1; 2; 3; 4] *)
let union_test2 : string list = union [ "a"; "b"; "c" ] [ "b"; "c"; "d" ]
(* should evaluate to ["a"; "b"; "c"; "d"] *)

(* PART 4: Compiling the Expression Language to a Stack-Based Language *)
(* Example program *)
let program1 : program = [ IPushC 2L; IPushC 3L; IMul ]

(* should evaluate to [6L] *)
let program2 : program = [ IPushC 3L; IPushC 4L; IAdd ]
(* should evaluate to [7L] *)

(* Problem 4-1 *)
let optimize_test1 : exp = optimize (Add (Const 3L, Const 4L))

(* should evaluate to Const 7L *)
let optimize_test2 : exp = optimize (Mult (Const 0L, Var "x"))
(* should evaluate to Const 0L *)

(* Problem 4-3 *)
let interpret_test1 : int64 = interpret ctxt1 e1 (* should evaluate to 6L *)
let interpret_test2 : int64 = interpret ctxt1 e2 (* should evaluate to 4L *)

(* Problem 4-4 *)
let optimize_test3 : exp = optimize (Add (Const 3L, Mult (Const 0L, Var "x")))
(* should evaluate to Const 3L *)

let optimize_test4 : exp =
  optimize (Add (Var "x", Neg (Add (Var "x", Neg (Const 1L)))))
(* should evaluate to Add (Var "x", Neg (Add (Var "x", Const (-1L))) *)

(* Example program *)
let program3 : program = [ IPushC 2L; IPushC 3L; IMul ]

(* should evaluate to [6L] *)
let program4 : program = [ IPushC 3L; IPushC 4L; IAdd ]
(* should evaluate to [7L] *)

(* Problem 4-2 *)
let lookup_test1 : int64 = lookup "x" ctxt1 (* should evaluate to 3L *)
let lookup_test2 : int64 = lookup "x" ctxt2 (* should evaluate to 2L *)
let lookup_test3 : int64 = lookup "y" ctxt2 (* should evaluate to 7L *)

(* Example program *)
let program5 : program = [ IPushV "x"; IPushC 1L; IAdd ]

(* should evaluate to [4L] *)
let program6 : program = [ IPushV "x"; IPushC 1L; IMul ]
(* should evaluate to [6L] *)

(* Example program *)
let program7 : program = [ IPushV "x"; IPushC 1L; IAdd; IPushC 2L; IMul ]

(* should evaluate to [8L] *)
let program8 : program = [ IPushV "x"; IPushC 1L; IAdd; IPushC 2L; IAdd ]
(* should evaluate to [6L] *)

(* Example program *)
let program9 : program = [ IPushV "x"; IPushC 1L; IAdd; IPushC 2L; IMul; INeg ]

(* should evaluate to [-8L] *)
let program10 : program = [ IPushV "x"; IPushC 1L; IAdd; IPushC 2L; IAdd; INeg ]
(* should evaluate to [-6L] *)

let student_tests : suite =
  [
    Test
      ( "Student-Provided Tests",
        [
          ("Prob 1-1", assert_eqf (fun () -> cube_test1) 8);
          ("Prob 1-2", assert_eqf (fun () -> cents_of_test2) 0);
          ("Prob 1-3", assert_eqf (fun () -> prob3_case2_test) 32);
          ("Prob 2-1", assert_eqf (fun () -> third_of_three_test) "hello");
          ("Prob 2-2", assert_eqf (fun () -> compose_pair_test) 7);
          ("Prob 3-1", assert_eqf (fun () -> vars_of_test1) []);
          ("Prob 3-1", assert_eqf (fun () -> vars_of_test2) [ "x" ]);
          ("Prob 3-1", assert_eqf (fun () -> vars_of_test3) [ "x"; "y" ]);
          ( "Prob 3-2",
            assert_eqf
              (fun () -> list_to_mylist_test1)
              (Cons (1, Cons (2, Cons (3, Nil)))) );
          ( "Prob 3-2",
            assert_eqf
              (fun () -> list_to_mylist_test2)
              (Cons ("a", Cons ("b", Cons ("c", Nil)))) );
          ("Prob 3-3", assert_eqf (fun () -> append_test1) [ 1; 2; 3; 4; 5 ]);
          ( "Prob 3-3",
            assert_eqf (fun () -> append_test2) [ "a"; "b"; "c"; "d"; "e" ] );
          ("Prob 3-4", assert_eqf (fun () -> rev_test1) [ 3; 2; 1 ]);
          ("Prob 3-4", assert_eqf (fun () -> rev_test2) [ "c"; "b"; "a" ]);
          ("Prob 3-5", assert_eqf (fun () -> insert_test1) [ 1; 2; 3; 4; 5 ]);
          ( "Prob 3-5",
            assert_eqf (fun () -> insert_test2) [ "a"; "b"; "c"; "d"; "e" ] );
          ("Prob 3-6", assert_eqf (fun () -> union_test1) [ 1; 2; 3; 4 ]);
          ("Prob 3-6", assert_eqf (fun () -> union_test2) [ "a"; "b"; "c"; "d" ]);
          ("Prob 4-1", assert_eqf (fun () -> optimize_test1) (Const 7L));
          ("Prob 4-1", assert_eqf (fun () -> optimize_test2) (Const 0L));
          ("Prob 4-3", assert_eqf (fun () -> interpret_test1) 6L);
          ("Prob 4-3", assert_eqf (fun () -> interpret_test2) 4L);
          ("Prob 4-4", assert_eqf (fun () -> optimize_test3) (Const 3L));
          ( "Prob 4-4",
            assert_eqf
              (fun () -> optimize_test4)
              (Add (Var "x", Neg (Add (Var "x", Const (-1L))))) );
          ("Prob 4-2", assert_eqf (fun () -> lookup_test1) 3L);
          ("Prob 4-2", assert_eqf (fun () -> lookup_test2) 2L);
          ("Prob 4-2", assert_eqf (fun () -> lookup_test3) 7L);
        ] );
  ]

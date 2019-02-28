(* $Id: interp.mli,v 1.5 2019-01-24 17:08:37-08 - - $ *)
(*
  Joseph Nguyen (jnguy243)
  Joshua Tai (jitai)
*)
(*
* Interpreter for Silly Basic
*)

val want_dump : bool ref

val interpret_program : Absyn.program -> unit

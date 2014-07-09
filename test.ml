(*
	progetto PR2
	test interprete espressioni
*)

#use "interprete.ml" (*carica l'interprete*)

let amb = emptyenv NoValue

let testCase (e:exp) (v:valori) (s:string) = 
	if (sem e amb) = v then
		print_endline ("test superato: " ^ s)		
	else
		print_endline ("test fallito: " ^ s )
		
let tt = Value( Bool true )
let ff = Value( Bool false )

let zero = Value( Int(0) )
let uno = Value( Int(1) )
let due = Value( Int(2) )

let l1 = Value( List( Cons(1,Nil) ) )
let l2 = Value( List ( Cons(1,Cons(2,Nil)) ))
let l3 = Value( List(  Cons(3,Cons(0,Nil)) ))
let l4 = Value( List( Cons(3,Cons(0,Nil)) ))
let l5 = Value( List( Nil ))

let f1 = Value( FunVal( Fun("x", Ope("+", Ide("x"), uno)), amb ) )

		
let t1 = And( tt,tt )
;; testCase t1 (Bool true) "And"

let t2 = Or( ff,tt )
;; testCase t2 (Bool true) "Or"

let t3 = Not( tt )
;; testCase t3 (Bool false) "Not"

let t4 = LessEq( l1, l2 )
;; testCase t4 (Bool true) "LessEq"

let t5 = Equal( l3, l4 )
;; testCase t5 (Bool true) "Equal"

let t6 = IsEmpty( l5 )
;; testCase t6 (Bool true) "IsEmpty"

let res = List( Cons(1,Cons(2,Cons(3,Cons(0,Nil)))) )
let t7 = Append( l2, l3 )
;; testCase t7 res "Append"

let t8 = IfThenElse( tt , Value(Int 0), Value(Int 1) )
;; testCase t8 (Int 0) "IfThenElse"

let t9 = Let( "x", tt , And(Ide("x"), tt ) )
;; testCase t9 (Bool true) "Let"

let t10 = Ope( "*", uno, due )
;; testCase t10 (Int 2) "Ope"

let t11 = Apply( f1, uno )
;; testCase t11 (Int 2) "Apply"

let res = List ( Cons(2,Cons(3,Nil)) )
let t12 = Map( f1, l2 )
;; testCase t12 res "Map"

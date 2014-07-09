(*
	progetto PR2
	interprete di espressioni
	
	TODO
	- implementazione lessEq
*)

(* AMBIENTE *)
type 't env = string -> 't 
let emptyenv x = function (y:string) -> x
let applyenv x (y:string) = x y
let bind (r: 'a env) (l:string) (e:'a) = 
    function lu -> if lu = l then e else applyenv r lu
    
type intList =
		| Nil 
		| Cons of int*intList		

type ide = string		
type operator = string
type efun =  exp * valori env
		
(* definizione valori esprimibili, coincidono con quelli denotabili *)
and valori = 
		| Int of int
		| List of intList
		| Bool of bool
		| FunVal of efun
		
(* sintassi astratta espressioni *)		
and exp = 	
		| Ide of ide
		| Value of valori
		| Fun of ide*exp 	
		| And of exp*exp 
		| Or of exp*exp
		| Not of exp
		| Ope of operator*exp*exp
		| LessEq of exp*exp
		| Equal of exp*exp
		| IsEmpty of exp
		| Append of exp*exp
		| IfThenElse of exp*exp*exp
		| Let of ide*exp*exp
		| Apply of exp*exp
		| Map of exp*exp

(* funzioni ausiliarie per la sem. definiscono la semantica di exp *)		
		
let semAnd x y = 
	match x,y with
		| Bool(a),Bool(b)	-> Bool( a && b )
		| _					-> failwith "And: errore di tipo" 

let semOr x y = 
	match x,y with
		| Bool(x),Bool(y)	-> Bool( x || y )
		| _					-> failwith "Or: errore di tipo"

let semNot x =
	match x with
		|Bool(x) 			-> Bool( not x )
		| _					-> failwith "Not: errore di tipo"

let semOpe op x y = 
	match x,y with 
		| Int(x), Int(y) 	->( match op with 
								| "+"	-> Int(x+y)
								| "-"	-> Int(x-y)
								| "*" 	-> Int(x*y)
								| "="	-> Bool(x=y)
								| "<="	-> Bool(x<=y)
								| _		-> failwith "Ope: operatore sconosciuto" )
		| _		-> failwith "Ope: errore di tipo"
		
let semLessEq x y =	
	let rec listLessEq x y = 
		match x,y with
			| Nil, Nil 					-> Bool(true) 
			| Nil, l					-> Bool(true)
			| Cons(a,al), Cons(b,bl)	-> if a=b then (listLessEq al bl) else Bool(false)
			| _							-> Bool(false)
	in match x,y with
		| List(x), List(y) -> listLessEq x y
		| _				   -> failwith "LessEq: errore di tipo"

let semEqual x y =
	match x,y with
		| List(x), List(y)  -> Bool( x=y )
		| _					-> failwith "Equal: errore di tipo"
		
let semEmpty x =
	match x with
		|List(x) 			-> Bool( x=Nil )
		| _					-> failwith "Empty: errore di tipo" 

let semAppend x y = 
	let rec append x y =
		match x with
			| Nil 			-> y
			| Cons(el,tl)   -> Cons(el, (append tl y) )			
	in match x,y with 
		| List(x), List(y)	-> List( append x y )
		| _					-> failwith "Append: errore di tipo"
		
(* interprete del linguaggio *)		
let rec sem (e:exp) (r:valori env) =
	match e with
		| Ide(x)			-> applyenv r x
		| Value(x)			-> x
		| Fun(x,e)			-> FunVal( Fun(x,e), r )
		| And(x,y) 			-> semAnd (sem x r) (sem y r)
		| Or(x,y)			-> semOr (sem x r) (sem y r)
		| Not(x)			-> semNot (sem x r)
		| Ope(op,x,y)		-> semOpe op (sem x r) (sem y r)
		| LessEq(x,y)		-> semLessEq (sem x r) (sem y r)
		| Equal(x,y)		-> semEqual (sem x r) (sem y r)
		| IsEmpty(x)		-> semEmpty (sem x r)
		| Append(x,y)		-> semAppend (sem x r) (sem y r)
		| IfThenElse(x,y,z)	-> (match (sem x r) with 
								| Bool(true)	-> sem y r
								| Bool(false)	-> sem z r
								| _				-> failwith "IfThenElse: guardia non booleana")
		| Let(x,y,z)		-> sem z (bind r x (sem y r)) 
		| Apply(x,y)		-> (match (sem x r) with
								|FunVal(Fun(s,e), amb) 	-> sem e (bind amb s (sem y r))
								| _					 	-> failwith "Apply: errore di tipo")
		| Map(f,y)			-> (match (sem y r) with 
								| List(Nil) 		-> List(Nil) 
								| List(Cons(x,xs))	->  let a = Value(Int(x)) in
														let hd= sem (Apply(f,a)) r in 
														let b = Value( List(xs) ) in
														let tl = sem (Map(f,b)) r 
														in (match hd, tl with
															| Int(x),List(y)	-> List( Cons(x, y) )
															| _					-> failwith "Map: errore di tipo")
								| _					-> failwith "Map: errore di tipo" )
	
	
	
	
	
	
	
	
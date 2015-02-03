(* FoCS Spring 2015

   Homework 1 code


   Name:

   Email:

   Comments:

 *)



(* 
 *  Question 1
 *)

let rec append (xs,ys) = match (xs,ys) with 
    (xs, []) -> xs  
  | ([], y::ys) -> y::append([], ys)
  | (x::xs, ys) -> x::append(xs,ys);;

let rec flatten x = match x with
    [] -> []
  | [[]] -> []
  | xs::xss -> append(xs, flatten(xss));;


let rec double (xs) = match (xs) with 
  [] -> []
  | x::xs -> (x*2)::double(xs);;

let rec last (xs) = match (xs) with
  [] -> None
  | x::[] -> Some x
  | x::xs -> last(xs);;



(*
 *  Question 2 
 *)

let rec setIn (elt,set) = match (elt, set) with
  (elt, []) -> false
  |(elt, s::set) -> if elt=s then true else setIn(elt, set);;


let rec setSub (set1,set2) = match (set1,set2) with
  ([],set2) -> true
  | (set1, []) -> false
  | (s1::set1, set2) -> if setIn(s1, set2) then setSub(set1, set2) else false;;

let setEqual (set1,set2) = match (set1, set2) with 
  (set1,set2) -> if setSub(set1, set2) && setSub(set2, set1) then true else false;;

let rec setUnion (set1,set2) = match (set1, set2) with
  ([],[]) -> []
  | ([], s2::set2) -> s2::setUnion([], set2) 
  | (s1::set1,set2) -> if setIn(s1, set2) || setIn(s1, set1) then setUnion(set1, set2) else s1::setUnion(set1, set2);;

let rec setInter (set1,set2) = match (set1, set2) with
  ([], set2) -> []
  | (s1::set1, set2) -> if setIn(s1, set2) && (setIn(s1, set1) != true) then s1::setInter(set1, set2) else setInter(set1, set2);;

let rec setSize (set) = match (set) with
  ([]) -> 0
  |  (s::set) -> if setIn(s, set) then setSize(set) else 1+setSize(set);;


(* 
 *  Question 3
 *)

type rat = {num: int; den: int}

let half = {num=1; den=2}
let third = {num=1; den=3}
let fourth = {num=1; den=4}

let floatR (r) = float(r.num) /. float(r.den)

let rec gcd (x,y) = match (x,y) with
  (0,y) -> abs(y)
  | (x,0) -> abs(x)
  | (x,y) -> if abs(x) < abs(y) then gcd (x,(y mod x)) else gcd(y,(x mod y));;



let simplify (r) = match (r) with
  r -> {num = r.num/gcd(r.num, r.den); den = r.den/gcd(r.num, r.den)};;

let addR (r1,r2) = 
  failwith "Not implemented"

let multR (r1,r2) = 
  failwith "Not implemented"

type number = I of int
            | R of rat
            | F of float

let add (n1,n2) = 
  failwith "Not implemented"



(* 
 *  Optional question
 *)
 

type bConst = True | False

type bExpr = Constant of bConst
           | Variable of string
           | And of bExpr * bExpr
           | Or of bExpr * bExpr
           | Not of bExpr

let sample1 = And(Not(Variable "a"),Not(Variable "b"))

let sample2 = Or(Not(Variable "a"),And(Variable "b",Constant(True)))

let sample3 = And(Variable "a", Not(Variable "a"))

let vars (bexpr) = 
  failwith "Not implemented"

let subst (bexpr,var,sub) = 
  failwith "Not implemented"

let eval (bexpr) = 
  failwith "Not implemented"

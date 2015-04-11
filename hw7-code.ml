(* FoCS Spring 2015

   Homework 7 code


   Name: Jazmin GOonzalez-Rivero

   Email: Jazmin.gonzalez-Rivero@students.olin.edu

   Comments: writing out Turning machines makes me sad :(

 *)



(* 
 *   explode_str : string -> string list
 *      returns the list of characters making up a string
 *      (where each character gets returned as a string)
 *
 *)

let explode_str (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.sub str index 1)::result)
  in
    acc(String.length(str)-1, [])




(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type direction = Left | Right

type symbol = string

type 'a tm = { tm_states : 'a list;
               tm_input_alph : symbol list;
	       tm_tape_alph : symbol list;
	       tm_leftmost : symbol;
	       tm_blank : symbol;
	       tm_delta : ('a * symbol * 'a * symbol * direction) list;
	       tm_start : 'a;
	       tm_accept : 'a;
	       tm_reject : 'a }


(*
 * Print a configuration (including newline) to standard output
 *  and return a value
 * 
 *)

let print_config m (u,q,v) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_newline ()  in
    value




(* QUESTION 1 *)
(* types seem a bit off for the first part *)
let starting_config m w = ([],m.tm_start, m.tm_leftmost::(explode_str w));;

let accepting_config m (tape,state,position) = (state = m.tm_accept);; 

let rejecting_config m (tape,state,position) = (state = m.tm_reject);; 

let halting_config m (tape,state,position) = (state = m.tm_accept || state = m.tm_reject);; 

let rec last xs = match xs with
	| x::[] -> x
	| x::xs -> (last xs)
	| [] -> failwith "has no last value";;

let rec all_but_last xs = match xs with
	[] -> []
	| x::[] -> []
	| x::xs -> x::(all_but_last xs);;


(* Do i need to check for the null left? *)
let step_config m (tape,state,position) =
	let make_Move (p, a, q, b, direc) position = match (direc,position) with
		(Right,[]) -> ((tape@[b]), q, [])
		| (Right,pos::position) -> ((tape@[b]), q, position)
		| (Left, []) -> ((all_but_last tape), q, (last tape)::b::[]) 
		| (Left, pos::position) -> ((all_but_last tape), q, (last tape)::b::position) 

	in 
	let rec find_delta delta pos = match delta with
		[] -> failwith "no Delta found"
		| (p, a, q, b, direc)::delta -> if (p = state && a = pos) 
			then (make_Move (p, a, q, b, direc) (position))
			else (find_delta delta pos)
	in
	match position with
		[] -> find_delta m.tm_delta "_"
		|pos::position -> find_delta m.tm_delta pos;;  	

let run m w = 
	let feed_config food =
		print_config m food food
	in
	let rec keepGoing c = 
		if accepting_config m c
			then print_config m c true
			else 
				if rejecting_config m c 
					then print_config m c false
					else (keepGoing (feed_config (step_config m c)))
	in
		keepGoing (starting_config m w);;

let run2  m w = 
	let rec keepGoing c = 
		if accepting_config m c
			then true
			else 
				if rejecting_config m c 
					then false
					else (keepGoing (step_config m c))
	in
		keepGoing (starting_config m w);;

(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the context-free language {a^n b^n | n >= 0}
 * anbncn is the non-context-free language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { tm_states = ["start"; "q1"; "acc"; "rej"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [("start", "a", "start", "a", Right);
     	                ("start", "b", "q1", "b", Right);
		        ("start", ">", "start", ">", Right);
		        ("start", "_", "acc", "_", Right);
		        ("q1", "a", "rej", "a", Right);
		        ("q1", "b", "q1", "b", Right);
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "_", "acc", "_", Right);
		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "_", "rej", "_", Right)] }

let anbn = { tm_states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b";"X";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [ ("start", "a", "start", "a", Right);
     	                ("start", "b", "q1", "b", Right);
		        ("start", ">", "start", ">", Right);
		        ("start", "_", "q2", "_", Right);
		        ("start", "X", "rej", "X", Right);
		        ("q1", "b", "q1", "b", Right);
		        ("q1", "_", "q2", "_", Right);
		        ("q1", "a", "rej", "a", Right);
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "X", "rej", "X", Right);
		        ("q2", ">", "q3", ">", Right);
		        ("q2", "a", "q2", "a", Left);
		        ("q2", "b", "q2", "b", Left);
		        ("q2", "X", "q2", "X", Left);
		        ("q2", "_", "q2", "_", Left);
		        ("q3", "X", "q3", "X", Right);
		        ("q3", "_", "acc", "_", Right);
		        ("q3", "a", "q4", "X", Right);
		        ("q3", "b", "rej", "b", Right);
		        ("q3", ">", "rej", ">", Right);
		        ("q4", "a", "q4", "a", Right);
		        ("q4", "X", "q4", "X", Right);
		        ("q4", "b", "q2", "X", Right);
		        ("q4", "a", "rej", "a", Right);
		        ("q4", ">", "rej", ">", Right);
		        ("q4", "_", "rej", "_", Right);
		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "X", "acc", "X", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "X", "rej", "X", Right);
		        ("rej", "_", "rej", "_", Right)] }

let anbncn = { tm_states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       tm_input_alph = ["a";"b";"c"];
	       tm_tape_alph = ["a";"b";"c";"X";"_";">"];
	       tm_blank = "_";
	       tm_leftmost = ">";
	       tm_start = "start";
	       tm_accept = "acc";
	       tm_reject = "rej";
	       tm_delta = [ ("start", "a", "start", "a", Right);
     	          ("start", "b", "q1", "b", Right);
		  ("start", "c", "q6", "c", Right);
		  ("start", ">", "start", ">", Right);
		  ("start", "_", "q2", "_", Right);
		  ("start", "X", "rej", "X", Right);
		  ("q1", "b", "q1", "b", Right);
		  ("q1", "c", "q6", "c", Right);
		  ("q1", "_", "q2", "_", Right);
		  ("q1", "a", "rej", "a", Right);
		  ("q1", ">", "rej", ">", Right);
		  ("q1", "X", "rej", "X", Right);
		  ("q2", ">", "q3", ">", Right);
		  ("q2", "a", "q2", "a", Left);
		  ("q2", "b", "q2", "b", Left);
		  ("q2", "c", "q2", "c", Left);
		  ("q2", "_", "q2", "_", Left);
		  ("q2", "X", "q2", "X", Left);
		  ("q3", "X", "q3", "X", Right);
		  ("q3", "_", "acc", "_", Right);
		  ("q3", "a", "q4", "X", Right);
		  ("q3", "b", "rej", "b", Right);
		  ("q3", "c", "rej", "c", Right);
		  ("q3", ">", "rej", ">", Right);
		  ("q4", "a", "q4", "a", Right);
		  ("q4", "X", "q4", "X", Right);
		  ("q4", "b", "q5", "X", Right);
		  ("q4", "c", "rej", "c", Right);
		  ("q4", "_", "rej", "_", Right);
		  ("q4", ">", "rej", ">", Right);
		  ("q5", "b", "q5", "b", Right);
		  ("q5", "X", "q5", "X", Right);
		  ("q5", "c", "q2", "X", Right);
		  ("q5", "a", "rej", "a", Right);
		  ("q5", "_", "rej", "_", Right);
		  ("q5", ">", "rej", ">", Right);
		  ("q6", "c", "q6", "c", Right);
		  ("q6", "_", "q2", "_", Right);
		  ("q6", "a", "rej", "a", Right);
		  ("q6", "b", "rej", "b", Right);
		  ("q6", ">", "rej", ">", Right);
		  ("q6", "X", "rej", "X", Right);
		  ("acc", "a", "acc", "a", Right);
		  ("acc", "b", "acc", "b", Right);
		  ("acc", "c", "acc", "c", Right);
		  ("acc", ">", "acc", ">", Right);
		  ("acc", "X", "acc", "X", Right);
		  ("acc", "_", "acc", "_", Right);
		  ("rej", "a", "rej", "a", Right);
		  ("rej", "b", "rej", "b", Right);
		  ("rej", "c", "rej", "c", Right);
		  ("rej", ">", "rej", ">", Right);
		  ("rej", "X", "rej", "X", Right);
		  ("rej", "_", "rej", "_", Right)] }




(* QUESTION 2 *)


let question2a = { tm_states = ["start"; "acc"; "rej";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"q8"; "rewind"; "oops"];
		   tm_input_alph = ["a";"b"];
	       tm_tape_alph = ["a";"b";"X";"Y";"_";">"];
		   tm_leftmost = ">";
		   tm_blank = "_";
		   tm_delta = [("start", "a", "start", "a", Right);
		  ("start", ">", "start", ">", Right);
		  ("start", "X", "rej", "X", Right);
		  ("start", "Y", "rej", "Y", Right);
		  ("start", "b", "q1", "b", Right);
		  ("start", "_", "rewind", "_", Right);
		  ("q1", "X", "rej", "X", Right);
		  ("q1", "Y", "rej", "Y", Right);
		  ("q1", ">", "rej", ">", Right);	  
		  ("q1", "b", "q1", "b", Right);	
		  ("q1", "a", "q2", "a", Right);
		  ("q1", "_", "rej", "_", Right);
		  ("q2", "X", "rej", "X", Right);
		  ("q2", "Y", "rej", "Y", Right);
		  ("q2", ">", "q3", ">", Right);	  
		  ("q2", "a", "q2", "a", Left);
		  ("q2", "b", "q2", "b", Left);	
		  ("q2", "_", "q2", "_", Left);
		  ("q3", "X", "rej", "X", Right);
		  ("q3", "Y", "rej", "Y", Right);
		  ("q3", ">", "rej", ">", Right);	  
		  ("q3", "a", "rewind", "a", Right);
		  ("q3", "b", "rej", "b", Right);	
		  ("q3", "_", "rewind", "_", Right);
		  ("rewind", "X", "rewind", "X", Left);
		  ("rewind", "Y", "rewind", "Y", Left);
		  ("rewind", "a", "rewind", "a", Left);
		  ("rewind", "b", "rewind", "b", Left);	
		  ("rewind", "_", "rewind", "_", Left); 
		  ("rewind", ">", "q4", ">", Right);
		  ("q4", "X", "q4", "X", Right);
		  ("q4", "Y", "q4", "Y", Right);
		  ("q4", ">", "rej", ">", Right);	  
		  ("q4", "a", "q5", "X", Right);
		  ("q4", "b", "q7", "Y", Right);	
		  ("q4", "_", "acc", "_", Right);
		  ("q5", "X", "q5", "X", Right);
		  ("q5", "Y", "q5", "Y", Right);
		  ("q5", ">", "rej", ">", Right);	  
		  ("q5", "a", "q5", "a", Right);
		  ("q5", "b", "q5", "b", Right);	
		  ("q5", "_", "oops", "_", Left);
		  ("oops", "X", "oops", "X", Left);
		  ("oops", "Y", "oops", "Y", Left);
		  ("oops", ">", "rej", ">", Right);	  
		  ("oops", "a", "q6", "X", Left);
		  ("oops", "b", "oops", "b", Left);	
		  ("oops", "_", "oops", "_", Left);
		  ("q6", "X", "q6", "X", Left);
		  ("q6", "Y", "q6", "Y", Left);
		  ("q6", ">", "rewind", ">", Right);	  
		  ("q6", "a", "q6", "a", Left);
		  ("q6", "b", "q7", "Y", Right);	
		  ("q6", "_", "rej", "_", Right);
		  ("q7", "X", "q7", "X", Right);
		  ("q7", "Y", "q7", "Y", Right);
		  ("q7", ">", "rej", ">", Right);	  
		  ("q7", "a", "q7", "a", Right);
		  ("q7", "b", "q7", "b", Right);	
		  ("q7", "_", "q8", "_", Left);
		  ("q8", "X", "q8", "X", Left);
		  ("q8", "Y", "q8", "Y", Left);
		  ("q8", ">", "rej", ">", Right);	  
		  ("q8", "a", "rej", "a", Right);
		  ("q8", "b", "rewind", "Y", Left);	
		  ("q8", "_", "q8", "_", Left);		  ];  
		   tm_start = "start";
		   tm_accept = "acc";
		   tm_reject = "rej" }


let question2b = { tm_states = ["start"; "acc"; "rej";"q1";"q2"; "q3"; "q4"; "q5"; "q6"; "q7";"rewind"];
		   tm_input_alph = ["a";"b"];
		   tm_tape_alph = ["a";"b"; "A"; "B";"_";">"];
		   tm_leftmost = ">";
		   tm_blank = "_";
		   tm_delta = [
		   ("start", "a", "q1", "a", Right);
		   ("start", "b", "q2", "b", Right);
		   ("start", "A", "rej", "A", Right);
		   ("start", "B", "rej", "B", Right);
		   ("start", ">", "start", ">", Right);
		   ("start", "_", "acc", "_", Right);
		   ("q1", "a", "q1", "a", Right);
		   ("q1", "b", "rewind", "b", Left);
		   ("q1", "A", "rej", "A", Right);
		   ("q1", "B", "rej", "B", Right);
		   ("q1", ">", "rej", ">", Right);
		   ("q1", "_", "acc", "_", Right);
		   ("q2", "a", "rej", "a", Right);
		   ("q2", "b", "q2", "b", Right);
		   ("q2", "A", "rej", "A", Right);
		   ("q2", "B", "rej", "B", Right);
		   ("q2", ">", "rej", ">", Right);
		   ("q2", "_", "acc", "_", Right);
		   ("rewind", "a", "q3", "A", Right);
		   ("rewind", "b", "rewind", "b", Left);
		   ("rewind", "A", "rewind", "A", Left);
		   ("rewind", "B", "rewind", "b", Left);
		   ("rewind", ">", "q6", ">", Right);
		   ("rewind", "_", "acc", "_", Right);
		   ("q3", "a", "q3", "a", Right);
		   ("q3", "b", "q4", "B", Right);
		   ("q3", "A", "q3", "A", Right);
		   ("q3", "B", "rej", "B", Right);
		   ("q3", ">", "rej", ">", Right);
		   ("q3", "_", "rej", "_", Right);
		   ("q4", "a", "q5", "A", Right);
		   ("q4", "b", "q4", "b", Right);
		   ("q4", "A", "q4", "A", Right);
		   ("q4", "B", "q4", "B", Right);
		   ("q4", ">", "rej", ">", Right);
		   ("q4", "_", "rej", "_", Right);
		   ("q5", "a", "q5", "a", Left);
		   ("q5", "b", "q4", "B", Right);
		   ("q5", "A", "q5", "A", Left);
		   ("q5", "B", "q5", "B", Left);
		   ("q5", ">", "rewind", ">", Right);
		   ("q5", "_", "q5", "_", Left);
		   ("q6", "a", "rej", "a", Right);
		   ("q6", "b", "q6", "b", Right);
		   ("q6", "A", "q6", "A", Right);
		   ("q6", "B", "q6", "B", Right);
		   ("q6", ">", "rej", ">", Right);
		   ("q6", "_", "acc", "_", Right);];
		   tm_start = "start";
		   tm_accept = "acc";
		   tm_reject = "rej" }



(* QUESTION 3 *)


let binary_sum = { tm_states = [];
		   tm_input_alph = ["0"; "1"; "#"];
		   tm_tape_alph = ["0";"1";"#";"%"; "0'"; "1'"; ">"; "_"];
		   tm_leftmost = ">";
		   tm_blank = "_";
		   tm_delta = [
		   ("start", "0", "inter1", "0", Right);
		   ("start", "1", "inter1", "1", Right);
		   ("start", "#", "rej", "#", Right);
		   ("start", ">", "start", ">", Right);
		   ("start", "%", "rej", "%", Right);
		   ("start", "0'", "rej", "0'", Right);
		   ("start", "1'", "rej", "1'", Right);
		   ("start", "_", "rej", "_", Right);
		   ("inter1", "0", "inter1", "0", Right);
		   ("inter1", "1", "inter1", "1", Right);
		   ("inter1", "#", "q1", "#", Right);
		   ("inter1", ">", "inter1", ">", Right);
		   ("inter1", "%", "rej", "%", Right);
		   ("inter1", "0'", "rej", "0'", Right);
		   ("inter1", "1'", "rej", "1'", Right);
		   ("inter1", "_", "rej", "_", Right);		   
		   ("q1", "0", "q1", "0", Right);
		   ("q1", "1", "q1", "1", Right);
		   ("q1", "#", "rewind", "%", Right);
		   ("q1", ">", "rej", ">", Right);
		   ("q1", "%", "rej", "%", Right);
		   ("q1", "0'", "rej", "0'", Right);
		   ("q1", "1'", "rej", "1'", Right);
		   ("q1", "_", "rej", "_", Right);
		   ("rewind", "0", "rewind", "0", Right);
		   ("rewind", "1", "rewind", "1", Right);
		   ("rewind", "#", "rewind", "#", Right);
		   ("rewind", ">", "rewind", ">", Right);
		   ("rewind", "%", "rewind", "%", Right);
		   ("rewind", "0'", "rewind", "0'", Right);
		   ("rewind", "1'", "rewind", "1'", Right);
		   ("rewind", "_", "q2", "_", Left);
		   ("q2", "0", "q4", "0'", Left);
		   ("q2", "1", "q3", "1'",Left);
		   ("q2", "#", "rej", "#", Left);
		   ("q2", ">", "rej", ">", Right);
		   ("q2", "%", "checker", "%", Left);   (* Need to add a step hear for checking length *)
		   ("q2", "0'", "q2", "0'", Left);
		   ("q2", "1'", "q2", "1'", Left);
		   ("q2", "_", "q2", "_", Left);		   		   
		   ("q3", "0", "q3", "0", Left);
		   ("q3", "1", "q3", "1", Left);
		   ("q3", "#", "rej", "#", Left);
		   ("q3", ">", "rej", ">", Right);
		   ("q3", "%", "q5", "%", Left);
		   ("q3", "0'", "q3", "0'", Left);
		   ("q3", "1'", "q3", "1'", Left);
		   ("q3", "_", "q3", "_", Left);
		   ("q4", "0", "q4", "0", Left);
		   ("q4", "1", "q4", "1", Left);
		   ("q4", "#", "rej", "#", Left);
		   ("q4", ">", "rej", ">", Right);
		   ("q4", "%", "q7", "%", Left);
		   ("q4", "0'", "q4", "0'", Left);
		   ("q4", "1'", "q4", "1'", Left);
		   ("q4", "_", "q4", "_", Left);
		   ("q5", "0", "rewind", "1'", Right);
		   ("q5", "1", "q6", "0'", Left);
		   ("q5", "#", "rej", "#", Left);
		   ("q5", ">", "rej", ">", Right);
		   ("q5", "%", "rej", "%", Right);
		   ("q5", "0'", "q5", "0'", Left);
		   ("q5", "1'", "q5", "1'", Left);
		   ("q5", "_", "rej", "_", Right);
		   ("q6", "0", "rewind", "1", Right);
		   ("q6", "1", "q6", "0", Left);
		   ("q6", "#", "rej", "#", Left);
		   ("q6", ">", "rej", ">", Right);
		   ("q6", "%", "rej", "%", Right);
		   ("q6", "0'", "rej", "0'", Left);
		   ("q6", "1'", "rej", "1'", Left);
		   ("q6", "_", "rej", "_", Right);	
		   ("q7", "0", "rewind", "0'", Left);
		   ("q7", "1", "rewind", "1'", Left);
		   ("q7", "#", "rej", "#", Left);
		   ("q7", ">", "rej", ">", Right);
		   ("q7", "%", "rej", "%", Left);
		   ("q7", "0'", "q7", "0'", Left);
		   ("q7", "1'", "q7", "1'", Left);
		   ("q7", "_", "rej", "_", Left);	   
		   ("checker", "0", "checker", "0", Left);
		   ("checker", "1", "checker", "1", Left);
		   ("checker", "#", "remaining", "#", Left);
		   ("checker", ">", "rej", ">", Right);
		   ("checker", "%", "rej", "%", Left);
		   ("checker", "0'", "c0", "0", Left);
		   ("checker", "1'", "c1", "1", Left);
		   ("checker", "_", "rej", "_", Left);	
		   ("c0", "0", "rej", "0", Left);
		   ("c0", "1", "rej", "1", Left);
		   ("c0", "#", "c0#", "#", Left);
		   ("c0", ">", "rej", ">", Right);
		   ("c0", "%", "rej", "%", Left);
		   ("c0", "0'", "c0", "0'", Left);
		   ("c0", "1'", "c0", "1'", Left);
		   ("c0", "_", "rej", "_", Left);	
		   ("c1", "0", "rej", "0", Left);
		   ("c1", "1", "rej", "1", Left);
		   ("c1", "#", "c1#", "#", Left);
		   ("c1", ">", "rej", ">", Right);
		   ("c1", "%", "rej", "%", Left);
		   ("c1", "0'", "c1", "0'", Left);
		   ("c1", "1'", "c1", "1'", Left);
		   ("c1", "_", "rej", "_", Left);
		   ("c0#", "0", "find#", "0'", Left);
		   ("c0#", "1", "rej", "1'", Left);
		   ("c0#", "#", "c0#", "#", Left);
		   ("c0#", ">", "rej", ">", Right);
		   ("c0#", "%", "rej", "%", Left);
		   ("c0#", "0'", "c0#", "0'", Left);
		   ("c0#", "1'", "c0#", "1'", Left);
		   ("c0#", "_", "rej", "_", Left);		
		   ("c1#", "0", "rej", "0'", Left);
		   ("c1#", "1", "find#", "1'", Left);
		   ("c1#", "#", "c1#", "#", Left);
		   ("c1#", ">", "rej", ">", Right);
		   ("c1#", "%", "rej", "%", Left);
		   ("c1#", "0'", "c1#", "0'", Left);
		   ("c1#", "1'", "c1#", "1'", Left);
		   ("c1#", "_", "rej", "_", Left);
		   ("find#", "0", "find#", "0", Right);
		   ("find#", "1", "find#", "1", Right);
		   ("find#", "#", "find#", "#",Right);
		   ("find#", ">", "find#", ">", Right);
		   ("find#", "%", "checker", "%", Left);
		   ("find#", "0'", "find#", "0'", Right);
		   ("find#", "1'", "find#", "1'", Right);
		   ("find#", "_", "rej", "_", Right);	
		   ("remaining", "0", "rej", "0", Left);
		   ("remaining", "1", "rej", "1", Left);
		   ("remaining", "#", "remaining", "#",Left);
		   ("remaining", ">", "acc", ">", Right);
		   ("remaining", "%", "rej", "%", Left);
		   ("remaining", "0'", "remaining", "0'", Left);
		   ("remaining", "1'", "remaining", "1'", Left);
		   ("remaining", "_", "rej", "_", Left);		   
		   ];
		   tm_start = "start";
		   tm_accept = "acc";
		   tm_reject = "rej" };;


(* test  *)

run2 question2b "";;
run2 question2b "bb";;
run2 question2b "abbaa";;
run2 question2b "aba";;
run2 question2b "a";;
run2 question2b "aa";;
run2 question2b "aabbaa";;
run2 question2b "abbbaa";;
run2 question2b "abbaaaaaaa";;
run2 question2b "abb";;


(* run2 binary_sum "";;
run2 binary_sum "##";;
run2 binary_sum "1##";; 
run2 binary_sum "#1#";; 
run2 binary_sum "##1";;
run2 binary_sum "1#1#";; 
run2 binary_sum "#1#1";;
run2 binary_sum "1##1";;
run2 binary_sum "000";;
run2 binary_sum "000#000";;
run2 binary_sum "000##000";;
run2 binary_sum "001#000#000";;
run2 binary_sum "011#001#000";;
run2 binary_sum "011#001#001";;
run2 binary_sum "110#001#001";;
run2 binary_sum "111#101#10";;
run2 binary_sum "11110#00111#10101";;
run2 binary_sum "010#001#001";;
run2 binary_sum "001#001#000";; 
run2 binary_sum "110#101#001";;
run2 binary_sum "111#101#010";; 
run2 binary_sum "000#000#000";;
run2 binary_sum "001#000#001";; 
run2 binary_sum "11100#00111#10101";; 
 *)
(* FoCS Spring 2015

   Homework 2 code


   Name:

   Email:

   Comments:

 *)




(* 
 * The type for a DFA, parameterized by the type for the states 
 *
 *)

type 'a dfa = {states :   'a list;
    	       alphabet : char list;
	       start :    'a;
   	       delta :    ('a * char * 'a) list;
	       final :    'a list}


(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = String.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (String.set str index c; loop(cs,index+1))
  in
    loop(cs,0)


(*
 * Some error code
 * Call "transitionError" to report an error while looking for a 
 *   transition in the delta of a DFA
 *
 *)

let transitionError (input) = 
  failwith ("DFA Error: Cannot transition on input "^(implode [input]))


(*
 * Some sample DFAs
 *
 *)


let isolatedBs =                (* language: all strings where every b *)
  {alphabet = ['a'; 'b'];       (* is bracketed by a's                 *)   
   states = ["start"; "readb"; "sink"];
   start = "start";
   delta = [("start", 'a', "start");
            ("start", 'b', "readb");
            ("readb", 'a', "start");
            ("readb", 'b', "sink"); 
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["start";"readb"]}


let ambn =                 (* language: strings of a's followed by b's *)
    {states = ["eata"; "eatb"; "sink"];
     alphabet = ['a'; 'b'];
     start = "eata";
     delta = [("eata", 'a', "eata");
              ("eata", 'b', "eatb");
              ("eatb", 'a', "sink");
              ("eatb", 'b', "eatb");
              ("sink", 'a', "sink");
              ("sink", 'b', "sink")];
     final = ["eata"; "eatb"]}


let aStar =                    (* language: all strings of a's *)
  {alphabet= ['a'; 'b'];
   states= ["ok"; "sink"];
   start= "ok";
   delta = [("ok",   'a', "ok");
            ("ok",   'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["ok"]}


let bPlus =                     (* language: all nonempty strings of b's *)
  {alphabet= ['a'; 'b'];
   states= ["start"; "ok"; "sink"];
   start= "start";
   delta = [("start", 'b', "ok");
            ("start", 'a', "sink");
            ("ok",    'b', "ok");
            ("ok",    'a', "sink");
            ("sink",  'b', "sink");
            ("sink",  'a', "sink")];
   final = ["ok"]}


let abaStar =              (* language: any number of ab's followed by a's *)
  {alphabet= ['a'; 'b'];
   states= ["astate"; "bstate"; "aonly"; "sink"];
   start= "astate";
   delta = [("astate", 'a', "bstate");
            ("astate", 'b', "sink");
            ("bstate", 'a', "aonly");
            ("bstate", 'b', "astate");
            ("aonly",  'a', "aonly");
            ("aonly",  'b', "sink");
            ("sink",   'a', "sink");
            ("sink",   'b', "sink")];
   final = ["astate"; "bstate"; "aonly"]}



(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings (alphabet, n) = 
  let rec mapCons (c, ls) = 
    match ls with
      [] -> []
    | l::ls' -> (c::l)::mapCons(c,ls')  in
  let rec mapConsSet (alphabet, l) = 
    match alphabet with
      [] -> []
    | c::cs -> mapCons(c,l) @ mapConsSet(cs,l)  in
  let rec mapImplode (css) = 
    match css with
      [] -> []
    | (cs::css) -> (implode cs)::mapImplode(css)  in
  let rec strings' (n) = 
    if (n<=0) then
      [[]]
    else let s = strings'(n-1) in
      [] :: mapConsSet(alphabet,s)
  in 
    mapImplode(strings'(n))




(*
 *  isFinal : 'a dfa * 'a -> bool
 *
 *    isFinal(dfa,q) should return true if and only if 'q' is a final state
 *    in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let isFinal (dfa,state) = 
  failwith "isFinal not implemented"



(* 
 *  transition : 'a dfa * 'a * char -> 'a
 *
 *    transition(dfa,q,a) should return the state obtained by reading input
 *    symbol 'a' in state 'q' in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let transition (dfa,state,input) = 
   failwith "transition not implemented"



(*
 *  extendedTransition : 'a dfa * 'a * char list -> 'a
 *
 *    extendedTransition(dfa,q,cs) should return the state obtained by
 *    reading the list of input symbols in 'cs' from state 'q' in the DFA
 *    'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let extendedTransition (dfa, state, cs) = 
  failwith "extendedTransition not implemented"


(*
 *  accept : 'a dfa * string -> bool
 *
 *    accept(dfa,input) should return true if and only the input string
 *    'input' is accepted by the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let accept (dfa, input) = 
  failwith "accept not implemented"



(*
 * PLACE YOUR ANSWERS TO QUESTION 3 HERE
 *
 * Each of these should be a function of no argument
 * returning the DFA that is a solution to the question
 *
 *)

let dfaQuestion1a () = 
  failwith "dfaQuestion1a not implemented"

let dfaQuestion1b () = 
  failwith "dfaQuestion1b not implemented"

let dfaQuestion1c () = 
  failwith "dfaQuestion1c not implemented"

let dfaQuestion1d () = 
  failwith "dfaQuestion1d not implemented"

let dfaQuestion1e () = 
  failwith "dfaQuestion1e not implemented"




(* 
 *  Compute the language of a DFA, restricted to inputs of length <= n
 *   language(dfa,n) returns a list of strings accepted by dfa
 *   printLanguage(dfa,n) prints the strings accepted by dfa
 *
 *)

let language (dfa, n) = 
  let candidates = strings(dfa.alphabet, n) in
  let rec tryAll (l) = 
    match l with
      [] -> []
    | s::ss -> if (accept(dfa,s)) then
                 s::(tryAll ss)
               else
                 tryAll ss
  in
    tryAll(candidates)


let printLanguage (dfa,n) = 
  let rec printList (l) = 
    match l with 
      [] -> ()
    | s::ss -> (print_string "   ";
                if (s="") then
                  print_string "<empty>"
                else
                  print_string s; 
                print_newline(); 
                printList ss)
  in
    printList(language(dfa,n))


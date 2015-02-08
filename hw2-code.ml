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

let rec inList (lookList, value) = match (lookList, value) with
  (l::look, value) -> if value = l then true else inList(look, value)
  | ([], value) -> false

let isFinal (dfa,state) = inList(dfa.final, state) 



(* 
 *  transition : 'a dfa * 'a * char -> 'a
 *
 *    transition(dfa,q,a) should return the state obtained by reading input
 *    symbol 'a' in state 'q' in the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)

let rec gettrans (delta, state, input) = match (delta, state, input) with
((now,tran,ends)::delta, state, input) -> if state = now && input = tran then ends else gettrans(delta, state, input)
| ([], state, input) -> "no end state"

let rec transition (dfa,state,input) = gettrans(dfa.delta, state, input)


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

let rec extendedTransition (dfa, state, cs) = match (dfa, state, cs) with
(dfa, state, c::cs) -> extendedTransition(dfa, transition(dfa, state,c), cs)
| (dfa, state, []) -> state

(*
 *  accept : 'a dfa * string -> bool
 *
 *    accept(dfa,input) should return true if and only the input string
 *    'input' is accepted by the DFA 'dfa'
 *
 *  PROVIDE CODE FOR THIS FUNCTION FOR QUESTION (2) 
 *
 *)


let accept (dfa, input) = isFinal(dfa, extendedTransition(dfa, dfa.start, explode (input)))



(*
 * PLACE YOUR ANSWERS TO QUESTION 3 HERE
 *
 * Each of these should be a function of no argument
 * returning the DFA that is a solution to the question
 *
 *)



let dfaQuestion1a () = 
  {alphabet= ['a'; 'b'];
   states= ["start"; "mid1"; "mid2"; "win"; "sink"];
   start= "start";
   delta = [("start", 'a', "mid1");
            ("start", 'b', "mid1");
            ("mid1", 'a', "mid2");
            ("mid1", 'b', "mid2");
            ("mid2",  'a', "win");
            ("mid2",  'b', "win");
            ("win",   'a', "sin  k");
            ("win",   'b', "sink");
            ("sink",   'a', "sink");
            ("sink",   'b', "sink")];
   final = ["win"]}


let dfaQuestion1b () = 
  {alphabet= ['a'; 'b'];
   states= ["start"; "mid1"; "mid2"; "win"];
   start= "start";
   delta = [("start", 'a', "mid1");
            ("start", 'b', "mid1");
            ("mid1", 'a', "mid2");
            ("mid1", 'b', "mid2");
            ("mid2",  'a', "win");
            ("mid2",  'b', "win");
            ("win",   'a', "mid1");
            ("win",   'b', "mid1")];
   final = ["win"]}

let dfaQuestion1c () = 
  {alphabet= ['a'; 'b'];
   states= ["start"; "mid1"; "mid2"; "win"];
   start= "start";
   delta = [("start", 'a', "win");
            ("start", 'b', "mid1");
            ("mid1", 'a', "win");
            ("mid1", 'b', "mid1");
            ("win",  'a', "mid1");
            ("win",  'b', "mid2");
            ("mid2",   'a', "mid1");
            ("mid2",   'b', "mid2")];
   final = ["win"]}

let dfaQuestion1d () = 
  {alphabet= ['a'; 'b'];
   states= ["start"; "mid1"; "win"; "mid2"];
   start= "start";
   delta = [("start", 'a', "win");
            ("start", 'b', "mid1");
            ("mid1", 'a', "win");
            ("win", 'a', "win");
            ("win", 'b', "mid1");
            ("mid1", 'b', "mid2");
            ("mid2",  'a', "mid2");
            ("mid2",  'b', "mid2")];
   final = ["win"]}

let dfaQuestion1e () = 
  {alphabet= ['a'; 'b'];
   states= ["start"; "mid1"; "win1"; "win2"; "win3"; "mid2"; "win4"];
   start= "start";
   delta = [("start", 'a', "mid1");
            ("start", 'b', "mid1");
            ("mid1", 'a', "win1");
            ("mid1", 'b', "win1");
            ("win1",  'a', "win2");
            ("win1",  'b', "win2");
            ("win2", 'b', "win3");
            ("win2",  'a', "win3");
            ("win3", 'b', "mid2");
            ("win3",  'a', "mid2");
            ("mid2", 'b', "win4");
            ("mid2",  'a', "win4");
            ("win4", 'b', "mid1");
            ("win4",  'a', "mid1")];
   final = ["win1"; "win2"; "win3"; "win4"]}




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


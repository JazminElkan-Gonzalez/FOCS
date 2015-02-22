(* FoCS Spring 2015

   Homework 2 code

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
 * String to Characters utility functions:
 *   explode : string -> char list
 *      returns the list of characters making up a string
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
 * Some error code call 
 * Call "transitionError" to report an error while looking for a 
 *   transition in the delta of a DFA
 *
 *)

let transitionError (input) = 
  failwith ("DFA Error: Cannot transition on input "^(implode [input]))



(*
 * Some sample DFAs to try out your code
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





(* QUESTION 2 *)

let isFinal (dfa,state) = 
  let rec check (finalStates) = 
    match finalStates with
      [] -> false
    | s::ss -> s=state || check(ss)  in
  check(dfa.final)


let transition (dfa,state,input) = 
  let rec transition' (delta) = 
    match delta with
      [] -> transitionError(input)
    | ((q1,c,q2)::delta') -> if (q1=state && c=input) then
                               q2
                             else
                              transition'(delta')  in
  transition'(dfa.delta)


let rec extendedTransition (dfa, state, cs) = 
  match cs with
    [] -> state
  | c::cs' -> extendedTransition(dfa,transition(dfa,state,c),cs')


let accept (dfa, input) = 
  isFinal(dfa,extendedTransition(dfa, dfa.start, explode(input)))



(* QUESTION 3 *)

let dfaQuestion1a () = 
  {alphabet = ['a';'b'];
    states = ["start";"one";"two";"three";"sink"];
    start="start";
    delta = [ ("start",'a',"one");
	      ("start",'b',"one");
	      ("one",'a',"two");
	      ("one",'b',"two");
	      ("two",'a',"three");
	      ("two",'b',"three");
	      ("three",'a',"sink");
	      ("three",'b',"sink");
	      ("sink",'a',"sink");
	      ("sink",'b',"sink")];
    final = ["three"]}


let dfaQuestion1b () = 
  {alphabet = ['a';'b'];
    states = ["start";"one";"two"];
    start="start";
    delta = [ ("start",'a',"one");
	      ("start",'b',"one");
	      ("one",'a',"two");
	      ("one",'b',"two");
	      ("two",'a',"start");
	      ("two",'b',"start")];
    final = ["start"]}
    

let dfaQuestion1c () = 
  {alphabet = ['a';'b'];
    states = ["start";"odd"];
    start="start";
    delta = [ ("start",'a',"odd");
	      ("start",'b',"start");
	      ("odd",'a',"start");
	      ("odd",'b',"odd")];
    final = ["odd"]}


let dfaQuestion1d () = 
  {alphabet = ['a';'b'];
    states = ["start";"b";"sink"];
    start="start";
    delta = [ ("start",'a',"start");
	      ("start",'b',"b");
	      ("b",'a',"start");
	      ("b",'b',"sink");
	      ("sink",'a',"sink");
	      ("sink",'b',"sink")];
    final = ["start"]}


let dfaQuestion1e () = 
  {alphabet = ['a';'b'];
    states = ["start";"1";"2";"3";"4";"5"];
    start="start";
    delta = [ ("start",'a',"1");
	      ("start",'b',"1");
	      ("1",'a',"2");
	      ("1",'b',"2");
	      ("2",'a',"3");
	      ("2",'b',"3");
	      ("3",'a',"4");
	      ("3",'b',"4");
	      ("4",'a',"5");
	      ("4",'b',"5");
	      ("5",'a',"start");
	      ("5",'b',"start")];
    final = ["start";"2";"3";"4"]}





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

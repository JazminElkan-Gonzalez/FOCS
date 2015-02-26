(* FoCS Spring 2015

   Homework 3 code


   Name:  Jazmin Gonzalez-Rivero

   Email: Jazmin.gonzalez-Rivero@students.olin.edu

   Comments:
   I was very sick with a fever during this homework assignment. I tried to make things as clear as possible. sorry if they arent!!!

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


let aStar =                    
  (* language: all strings of a's *)
  {alphabet= ['a'; 'b'];
   states= ["ok"; "sink"];
   start= "ok";
   delta = [("ok",   'a', "ok");
            ("ok",   'b', "sink");
            ("sink", 'a', "sink");
            ("sink", 'b', "sink")];
   final = ["ok"]}


let bPlus =                     
  (* language: all nonempty strings of b's *)
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

(*
 * Exercise 2 
 *
 *)


let complement (dfa) =
    let rec removeValue(l, value) = match (l, value) with
      ([], value) -> []
      | (l::ls, v) -> 
        if l = v 
          then ls 
          else l::removeValue(ls, v) in
    let rec removeAllValues(l, values) =  match (l,values) with
      (l, []) -> l
      | (ls, v::vs) -> removeAllValues(removeValue(ls,v), vs) in
    {states = dfa.states; alphabet = dfa.alphabet ; start = dfa.start ; delta = dfa.delta; final = removeAllValues(dfa.states, dfa.final)};;



let rec cross (xs, ys) =  
  let rec matchYs (x, ys) = match (x,ys) with
    (x, []) -> []
    | (x, y::ys) -> (x,y)::matchYs(x,ys) in
  match (xs,ys) with
    ([], ys) -> []
    | (x::xs, ys) -> matchYs(x,ys)@cross(xs,ys);;

let rec findFinal (crossList, dfa1, dfa2) = 
  let rec inList (x, ys) = match (x,ys) with
    (val1, []) -> false
    | (val1, s::lit) -> 
      if val1 = s 
       then true 
       else inList(val1,lit)
  in
  match (crossList, dfa1,dfa2) with
    ([], dfa1, dfa2) -> []
    |((s1,s2)::crossList,  dfa1, dfa2) -> 
      if inList(s1, dfa1.final) || inList(s2, dfa2.final) 
        then (s1,s2)::findFinal(crossList,dfa1, dfa2) 
        else findFinal(crossList, dfa1, dfa2)  ;;


let union (dfa1, dfa2) =
  let rec findEndpoint(s, t, delta) = match (s,t,delta) with
    (s,t,[]) -> "fail"
    | (s, t, (s2,t2,e2)::delta) ->
      if (s = s2 && t = t2) 
        then e2
        else findEndpoint(s,t,delta)
  in
  let rec findTrans(crossList, transList) = match (crossList, transList) with
    ((s1,s2)::crossList, []) -> findTrans(crossList, dfa1.alphabet)
    | ([],tL) -> []
    | ((s1,s2)::crossList, t::transList) -> ((s1,s2), t, (findEndpoint(s1, t, dfa1.delta),findEndpoint(s2, t,dfa2.delta)))::findTrans((s1,s2)::crossList, transList) 
  in
    {states = cross(dfa1.states, dfa2.states);
    alphabet = dfa1.alphabet ; 
    start = (dfa1.start, dfa2.start) ;
    delta = findTrans(cross(dfa1.states, dfa2.states),dfa1.alphabet);
    final =  findFinal(cross(dfa1.states, dfa2.states), dfa1, dfa2)};;




(*
 *  Set-based helper functions, mostly from Homework 1
 *
 *  The only addition is 'subsets', which computes the
 *  set of subsets of a set (all taken to be lists, 
 *  of course)
 *
 *  'subsets' should be handy for Exercise 4
 *
 *)


let rec setIn (elt,set) = 
  match set with
    [] -> false
  | x::xs -> (x = elt || setIn(elt,xs))

let rec setSub (set1,set2) = 
  match set1 with
    [] -> true
  | x::xs -> setIn(x,set2) && setSub(xs,set2)

let setEqual (set1,set2) = 
  setSub(set1,set2) && setSub(set2,set1)

let rec setInter (set1,set2) = 
   match set1 with 
     [] -> []
   | x::xs -> if setIn(x,set2) then 
                 x::(setInter(xs,set2))
              else 
                 setInter(xs,set2)

let rec subsets xs = 
  match xs with
    [] -> [[]]
  | x::xs' -> subsets(xs') @ (List.map (fun ys -> x::ys) (subsets xs'))




(* 
 * The type for an NFA, parameterized by the type for the states 
 *
 *)

type 'a nfa = {nfa_states :   'a list;
               nfa_alphabet : char list;
               nfa_start :    'a;
   	       nfa_delta :    ('a * char * 'a list) list;
	       nfa_final :    'a list}


(*
 * Some sample NFAs
 *
 *)


(* language: (ab+aab)* *)
let abaabStar = {nfa_states = ["s";"a11";"a21";"a22"];
                 nfa_alphabet = ['a';'b'];
                 nfa_start = "s";
                 nfa_delta = [("s", 'a', ["a11"; "a21"]);
                               ("a21", 'a', ["a22"]);
                               ("a11", 'b', ["s"]);
                               ("a22", 'b', ["s"])];
                 nfa_final = ["s"]}

(* language: ab+aab *)
let abaab = {nfa_states = ["q1"; "q2"; "q3"; "q4"; "q5"];
             nfa_alphabet = ['a'; 'b'];
             nfa_start = "q1";
             nfa_delta = [("q1", 'a', ["q2"; "q3"]);
                        ("q2", 'b', ["q5"]);
                        ("q3", 'a', ["q4"]);
                        ("q4", 'b', ["q5"])];
             nfa_final = ["q5"]}





(*
 * In common for exercises 3 and 4
 *
 *)


let rec nfa_hasFinal (nfa,states) = 
  let rec inList (nfaStates, s) = match (nfaStates, s) with
    ([],s) -> false
    |(s1::nfaStates, s) -> 
      if s = s1 
        then true 
        else inList(nfaStates,s)
  in
  match (nfa,states) with
    (nfa, []) -> false
    | (nfa, s::states) -> 
      if inList(nfa.nfa_final, s) 
        then true
        else nfa_hasFinal(nfa,states);;

let rec nfa_transition (nfa,states,input) = 
  let rec findOutputs(state, input, delta) = match (state, input, delta)  with
    (state, input, []) -> []
    | (state, input, ((s,t,e)::delta)) ->
      if state = s && input = t 
        then e 
        else findOutputs(state, input, delta) in
  match (nfa,states,input) with 
    (nfa,[],input) -> []
    | (nfa,s::states,input) -> findOutputs(s, input, nfa.nfa_delta)@nfa_transition(nfa, states, input);;


(*
 * Exercise 3
 *
 *)

let rec nfa_extendedTransition (nfa, states, cs) = 
    match (nfa, states, cs) with 
      (nfa, states, []) -> states
      | (nfa, [], cs) -> []
      | (nfa, states, cs1::cs) -> nfa_extendedTransition(nfa, nfa_transition(nfa,states,cs1), cs);;


let nfa_accept (nfa, input) = nfa_hasFinal(nfa,nfa_extendedTransition(nfa, [nfa.nfa_start], explode(input)));;




(*
 * Exercise 4
 *
 *)


let subsetConstruction nfa =
  failwith "subsetConstruction not implemented"




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


(* 
 *  Compute the language of an NFA, restricted to inputs of length <= n
 *   nfa_language(nfa,n) returns a list of strings accepted by nfa
 *   nfa_printLanguage(nfa,n) prints the strings accepted by nfa
 *
 *)

let nfa_language (nfa, n) = 
  let candidates = strings(nfa.nfa_alphabet, n) in
  let rec tryAll (l) = 
    match l with
      [] -> []
    | s::ss -> if (nfa_accept(nfa,s)) then
                 s::(tryAll ss)
               else
                 tryAll ss
  in
    tryAll(candidates)

let nfa_printLanguage (nfa,n) = 
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
    printList(nfa_language(nfa,n))

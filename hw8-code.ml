(*FoCS Spring 2015

   Homework 8 code


  Name: Jazmin GOonzalez-Rivero

   Email: Jazmin.gonzalez-Rivero@students.olin.edu

   Comments: was sick...again...so i didnt get to finish question 3 :( So sad

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
 *   Type for deterministic Turing machines
 *
 *   Parameterized by type for states
 *)

type direction = Left | Right | Stay

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
 *   Run a Turing machine on an input word
 *)

let run m w = 

  let print_config m (u,q,v) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_newline ()  in
    value  in

  let starting_config = 
    ([], m.tm_start, m.tm_leftmost::(explode_str w))  in

  let accepting_config m (u,q,v) = (q=m.tm_accept)  in

  let rejecting_config m (u,q,v) = (q=m.tm_reject)  in

  let halting_config m c = (accepting_config m c) || (rejecting_config m c)  in

  let step_config m (u,q,v) = 

    let rec find_match (q,a) delta = 
      match delta with
        [] -> failwith a
      | ((q_,sym_,p_,rewrite_,dir_)::_) when q=q_ && a=sym_ -> (p_,rewrite_,dir_)
      | _::delta' -> find_match (q,a) delta'  in

    let rec last u = 
      match u with
        [] -> failwith "Moving Left from leftmost tape position"
      | [a] -> ([],a)
      | x::xs -> let (u',r) = last xs  in (x::u',r)   in

    if (halting_config m (u,q,v)) 
      then (u,q,v)
    else let (a,v') = match v with
                      | [] -> (m.tm_blank,[])
                      | a::v' -> (a,v')  in
         let (q',b,dir) = find_match (q,a) m.tm_delta  in
         match dir with
         | Left -> let (u',c) = last u in (u',q',c::b::v')
         | Right -> (u@[b],q',v')
         | Stay -> (u,q',b::v')  in

  let rec loop c = 
    let _ = print_config m c c in
    if  (accepting_config m c) then true
    else if (rejecting_config m c) then false
    else loop (step_config m c)  in

  loop starting_config



(* 
 *
 * QUESTION 1
 *
 *)


let rec make_delta states alph f = 
  let get_result s a (x,y,z) = (s,a,x,y,z)
 in
  let rec alpha_step s alph = match alph with
  [] -> []
  | a::alphs -> (get_result s a (f (s,a)))::(alpha_step s alphs)
  in 
  match states with 
  [] -> []
  | s::ss ->  (alpha_step s alph)@(make_delta ss alph f);;

  
let rec transform_states t states = match states with
[] -> []
| s::ss -> (t s)::(transform_states t ss);;

let rec transform_delta t delta = match delta with
[] -> []
| (s1, a1, s2, a2, d)::deltas -> ((t s1), a1, (t s2), a2, d)::(transform_delta t deltas);;

let transform t tm =
 {tm_states = (transform_states t tm.tm_states);
tm_input_alph = tm.tm_input_alph;
tm_tape_alph = tm.tm_tape_alph;
tm_blank = tm.tm_blank;
tm_leftmost = tm.tm_leftmost;
tm_delta = (transform_delta t tm.tm_delta);
tm_start = (t tm.tm_start);
tm_accept = (t tm.tm_accept);
tm_reject = (t tm.tm_reject)};;




(* 
 * Some sample deterministic Turing machines
 *
 * anbn is the context-free language {a^n b^n | n >= 0}
 * anbncn is the non-context-free language {a^n b^n c^n | n >= 0}
 *
 *)


let anbn = 
  let states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"]  in
  let tape_alph = ["a";"b";"X";"_";">"]  in
  { tm_states = states;
    tm_input_alph = ["a";"b"];
    tm_tape_alph = tape_alph;
    tm_blank = "_";
    tm_leftmost = ">";
    tm_start = "start";
    tm_accept = "acc";
    tm_reject = "rej";
    tm_delta = make_delta states tape_alph
      (fun (q,a) -> 
        match (q,a) with
        | ("start", "a") -> ("start", "a", Right)
        | ("start", "b") -> ("q1", "b", Right)
        | ("start", ">") -> ("start", ">", Right)
        | ("start", "_") -> ("q2", "_", Right)
        | ("q1", "b") -> ("q1", "b", Right)
        | ("q1", "_") -> ("q2", "_", Right)
        | ("q2", ">") -> ("q3", ">", Right)
        | ("q2", sym) -> ("q2", sym, Left)
        | ("q3", "X") -> ("q3", "X", Right)
        | ("q3", "_") -> ("acc", "_", Right)
        | ("q3", "a") -> ("q4", "X", Right)
        | ("q4", "a") -> ("q4", "a", Right)
        | ("q4", "X") -> ("q4", "X", Right)
        | ("q4", "b") -> ("q2", "X", Right)
        | ("acc", sym) -> ("acc", sym, Right)
        | (_, sym) -> ("rej", sym, Right)) }


let anbncn = 
  let states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"] in
  let tape_alph = ["a";"b";"c";"X";"_";">"] in
  { tm_states = states;
    tm_input_alph = ["a";"b";"c"];
    tm_tape_alph = tape_alph;
    tm_blank = "_";
    tm_leftmost = ">";
    tm_start = "start";
    tm_accept = "acc";
    tm_reject = "rej";
    tm_delta = make_delta states tape_alph
      (fun (q,a) -> 
        match (q,a) with
        | ("start", "a") -> ("start", "a", Right)
        | ("start", "b") -> ("q1", "b", Right)
        | ("start", "c") -> ("q6", "c", Right)
        | ("start", ">") -> ("start", ">", Right)
        | ("start", "_") -> ("q2", "_", Right)
        | ("q1", "b") -> ("q1", "b", Right)
        | ("q1", "c") -> ("q6", "c", Right)
        | ("q1", "_") -> ("q2", "_", Right)
        | ("q2", ">") -> ("q3", ">", Right)
        | ("q2", sym) -> ("q2", sym, Left)
        | ("q3", "X") -> ("q3", "X", Right)
        | ("q3", "_") -> ("acc", "_", Right)
        | ("q3", "a") -> ("q4", "X", Right)
        | ("q4", "a") -> ("q4", "a", Right)
        | ("q4", "X") -> ("q4", "X", Right)
        | ("q4", "b") -> ("q5", "X", Right)
        | ("q5", "b") -> ("q5", "b", Right)
        | ("q5", "X") -> ("q5", "X", Right)
        | ("q5", "c") -> ("q2", "X", Right)
        | ("q6", "c") -> ("q6", "c", Right)
        | ("q6", "_") -> ("q2", "_", Right)
        | ("acc", sym) -> ("acc", sym, Right)
        | (_, sym) -> ("rej", sym, Right) )}


(* see write up *)

let evenOddSequence = 
  let delta (p,a) = 
    match p,a with
    | "even", ">" -> ("even", ">", Right)
    | "even", "0" -> ("odd", "0", Right)
    | "even", "1" -> ("even", "1", Right)
    | "even", "_" -> ("q1/0", "_", Left)
          
    | "odd", "0" -> ("even", "0", Right)
    | "odd", "1" -> ("odd", "1", Right)
    | "odd", "_" -> ("q1/1", "_", Left)
          
    | "q1/0", ">" -> ("acc", ">", Right)
    | "q1/0", "0" -> ("q2/0", "0", Left)
    | "q1/0", "1" -> ("q1/0", "1", Left)
          
    | "q2/0", ">" -> ("acc", ">", Right)
    | "q2/0", "0" -> ("q3/0", "0", Left)
    | "q2/0", "1" -> ("q1/0", "1", Left)
          
    | "q3/0", ">" -> ("acc", ">", Right)
    | "q3/0", "1" -> ("q1/0", "1", Left)
          
    | "q1/1", ">" -> ("acc", ">", Right)
    | "q1/1", "0" -> ("q1/1", "0", Left)
    | "q1/1", "1" -> ("q2/1", "1", Left)
          
    | "q2/1", ">" -> ("acc", ">", Right)
    | "q2/1", "0" -> ("q1/1", "0", Left)
    | "q2/1", "1" -> ("q3/1", "1", Left)
          
    | "q3/1", ">" -> ("acc", ">", Right)
    | "q3/1", "0" -> ("q1/1", "0", Left)
          
    | "acc", sym -> ("acc", sym, Right)
    | _, sym -> ("rej", sym, Right)  in
  let states = [ "even";"odd";"q1/0";"q2/0";"q3/0";"q1/1";"q2/1";"q3/1";
                 "acc";"rej"]  in
  let alph = ["0";"1";"_";">"]  in
  { tm_states = states ;
    tm_input_alph = ["0";"1"];
    tm_tape_alph = alph;
    tm_leftmost = ">";
    tm_blank = "_";
    tm_delta = make_delta states alph delta;
    tm_start = "even";
    tm_accept = "acc";
    tm_reject = "rej" }



(* A version of the same Turing machine but with
   structured states 
 *)

type even_odd_sequence_state = 
  | Simple of string
  | BitTag of string * string

let evenOddSequence_struct = 
  let delta (p,a) = 
    match p,a with
    | Simple("even"), ">" -> (Simple("even"), ">", Right)
    | Simple("even"), "0" -> (Simple("odd"), "0", Right)
    | Simple("even"), "1" -> (Simple("even"), "1", Right)
    | Simple("even"), "_" -> (BitTag("q1", "0"), "_", Left)
          
    | Simple("odd"), "0" -> (Simple("even"), "0", Right)
    | Simple("odd"), "1" -> (Simple("odd"), "1", Right)
    | Simple("odd"), "_" -> (BitTag("q1", "1"), "_", Left)
          
    | BitTag("q1", t), ">" -> (Simple("acc"), ">", Right)
    | BitTag("q1", t), sym when t = sym -> (BitTag("q2", t), sym, Left)
    | BitTag("q1", t), sym -> (BitTag("q1", t), sym, Left)
          
    | BitTag("q2", t), ">" -> (Simple("acc"), ">", Right)
    | BitTag("q2", t), sym when t = sym -> (BitTag("q3", t), sym, Left)
    | BitTag("q2", t), sym -> (BitTag("q1", t), sym, Left)
          
    | BitTag("q3", t), ">" -> (Simple("acc"), ">", Right)
    | BitTag("q3", t), sym when t <> sym -> (BitTag("q1", t), sym, Left)
          
    | Simple("acc"), sym -> (Simple("acc"), sym, Right)
    | _, sym -> (Simple("rej"), sym, Right)  in
  let states = [ Simple("even"); Simple("odd"); Simple("acc"); Simple("rej");
                 BitTag("q1", "0"); BitTag("q2", "0"); BitTag("q3", "0"); 
                 BitTag("q1", "1"); BitTag("q2", "1"); BitTag("q3", "1") ]  in
  let alph = ["0"; "1"; ">"; "_"]  in
  let string_of st = 
    match st with
    | Simple(s) -> s
    | BitTag(s,b) -> s^"|"^b  in
  transform string_of
    { tm_states = states;
      tm_input_alph = ["0"; "1"];
      tm_tape_alph = alph;
      tm_leftmost = ">";
      tm_blank = "_";
      tm_delta = make_delta states alph delta;
      tm_start = Simple("even");
      tm_accept = Simple("acc");
      tm_reject = Simple("rej") }




(* 
 *
 * QUESTION 2
 *
 *)


(* some helper definitions *)

let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let isDigit c = List.mem c digits


let triple =
let delta (p,a) = match p,a with
 | Simple("start"), ">" -> (Simple("start"), ">", Right)
 | Simple("start"), "0" -> (Simple("q1"), "0", Right)
 | Simple("start"), "1" -> (Simple("q1"), "1", Right)
 | Simple("start"), "2" -> (Simple("q1"), "2", Right)
 | Simple("start"), "3" -> (Simple("q1"), "3", Right)
 | Simple("start"), "4" -> (Simple("q1"), "4", Right)
 | Simple("start"), "5" -> (Simple("q1"), "5", Right)
 | Simple("start"), "6" -> (Simple("q1"), "6", Right)
 | Simple("start"), "7" -> (Simple("q1"), "7", Right)
 | Simple("start"), "8" -> (Simple("q1"), "8", Right)
 | Simple("start"), "9" -> (Simple("q1"), "9", Right)
 | Simple("q1"), "0" -> (Simple("q1"), "0", Right)
 | Simple("q1"), "1" -> (Simple("q1"), "1", Right)
 | Simple("q1"), "2" -> (Simple("q1"), "2", Right)
 | Simple("q1"), "3" -> (Simple("q1"), "3", Right)
 | Simple("q1"), "4" -> (Simple("q1"), "4", Right)
 | Simple("q1"), "5" -> (Simple("q1"), "5", Right)
 | Simple("q1"), "6" -> (Simple("q1"), "6", Right)
 | Simple("q1"), "7" -> (Simple("q1"), "7", Right)
 | Simple("q1"), "8" -> (Simple("q1"), "8", Right)
 | Simple("q1"), "9" -> (Simple("q1"), "9", Right)
 | Simple("q1"), "#" -> (Simple("q2"), "#", Right)
 | Simple("q2"), "0" -> (Simple("q3"), "0", Right)
 | Simple("q2"), "1" -> (Simple("q3"), "1", Right)
 | Simple("q2"), "2" -> (Simple("q3"), "2", Right)
 | Simple("q2"), "3" -> (Simple("q3"), "3", Right)
 | Simple("q2"), "4" -> (Simple("q3"), "4", Right)
 | Simple("q2"), "5" -> (Simple("q3"), "5", Right)
 | Simple("q2"), "6" -> (Simple("q3"), "6", Right)
 | Simple("q2"), "7" -> (Simple("q3"), "7", Right)
 | Simple("q2"), "8" -> (Simple("q3"), "8", Right)
 | Simple("q2"), "9" -> (Simple("q3"), "9", Right)
 | Simple("q3"), "0" -> (Simple("q3"), "0", Right)
 | Simple("q3"), "1" -> (Simple("q3"), "1", Right)
 | Simple("q3"), "2" -> (Simple("q3"), "2", Right)
 | Simple("q3"), "3" -> (Simple("q3"), "3", Right)
 | Simple("q3"), "4" -> (Simple("q3"), "4", Right)
 | Simple("q3"), "5" -> (Simple("q3"), "5", Right)
 | Simple("q3"), "6" -> (Simple("q3"), "6", Right)
 | Simple("q3"), "7" -> (Simple("q3"), "7", Right)
 | Simple("q3"), "8" -> (Simple("q3"), "8", Right)
 | Simple("q3"), "9" -> (Simple("q3"), "9", Right)
 | Simple("q3"), "#" -> (Simple("q4"), "#", Right)
 | Simple("q4"), "0" -> (Simple("q5"), "0", Right)
 | Simple("q4"), "1" -> (Simple("q5"), "1", Right)
 | Simple("q4"), "2" -> (Simple("q5"), "2", Right)
 | Simple("q4"), "3" -> (Simple("q5"), "3", Right)
 | Simple("q4"), "4" -> (Simple("q5"), "4", Right)
 | Simple("q4"), "5" -> (Simple("q5"), "5", Right)
 | Simple("q4"), "6" -> (Simple("q5"), "6", Right)
 | Simple("q4"), "7" -> (Simple("q5"), "7", Right)
 | Simple("q4"), "8" -> (Simple("q5"), "8", Right)
 | Simple("q4"), "9" -> (Simple("q5"), "9", Right)
 | Simple("q5"), "0" -> (Simple("q5"), "0", Right)
 | Simple("q5"), "1" -> (Simple("q5"), "1", Right)
 | Simple("q5"), "2" -> (Simple("q5"), "2", Right)
 | Simple("q5"), "3" -> (Simple("q5"), "3", Right)
 | Simple("q5"), "4" -> (Simple("q5"), "4", Right)
 | Simple("q5"), "5" -> (Simple("q5"), "5", Right)
 | Simple("q5"), "6" -> (Simple("q5"), "6", Right)
 | Simple("q5"), "7" -> (Simple("q5"), "7", Right)
 | Simple("q5"), "8" -> (Simple("q5"), "8", Right)
 | Simple("q5"), "9" -> (Simple("q5"), "9", Right)
 | Simple("q5"), "_" -> (Simple("rewind"), "_", Right)
 | Simple("rewind"), ">" -> (Simple("q6"), ">", Right)
 | Simple("rewind"), "0" -> (Simple("rewind"), "0", Left)
 | Simple("rewind"), "1" -> (Simple("rewind"), "1", Left)
 | Simple("rewind"), "2" -> (Simple("rewind"), "2", Left)
 | Simple("rewind"), "3" -> (Simple("rewind"), "3", Left)
 | Simple("rewind"), "4" -> (Simple("rewind"), "4", Left)
 | Simple("rewind"), "5" -> (Simple("rewind"), "5", Left)
 | Simple("rewind"), "6" -> (Simple("rewind"), "6", Left)
 | Simple("rewind"), "7" -> (Simple("rewind"), "7", Left)
 | Simple("rewind"), "8" -> (Simple("rewind"), "8", Left)
 | Simple("rewind"), "9" -> (Simple("rewind"), "9", Left)
 | Simple("rewind"), "#" -> (Simple("rewind"), "#", Left)
 | Simple("rewind"), "_" -> (Simple("rewind"), "_", Left)
 | Simple("rewind"), "X" -> (Simple("rewind"), "X", Left)
 | Simple("q6"), "0" -> (BitTag("q7", "0"), "X", Right)
 | Simple("q6"), "1" -> (BitTag("q7", "1"), "X", Right)
 | Simple("q6"), "2" -> (BitTag("q7", "2"), "X", Right)
 | Simple("q6"), "3" -> (BitTag("q7", "3"), "X", Right)
 | Simple("q6"), "4" -> (BitTag("q7", "4"), "X", Right)
 | Simple("q6"), "5" -> (BitTag("q7", "5"), "X", Right)
 | Simple("q6"), "6" -> (BitTag("q7", "6"), "X", Right)
 | Simple("q6"), "7" -> (BitTag("q7", "7"), "X", Right)
 | Simple("q6"), "8" -> (BitTag("q7", "8"), "X", Right)
 | Simple("q6"), "9" -> (BitTag("q7", "9"), "X", Right)
 | Simple("q6"), "#" -> (Simple("checker"), "#", Right)
 | Simple("q6"), "X" -> (Simple("q6"), "X", Right)
 | BitTag("q7", t), "X" -> (BitTag("q7",t), "X", Right)
 | BitTag("q7", t), "#" -> (BitTag("q8",t), "#", Right)
 | BitTag("q7", t), sym -> (BitTag("q7", t), sym, Right)
 | BitTag("q8", t), "X" -> (BitTag("q8",t), "X", Right)
 | BitTag("q8", t), sym when t = sym  -> (BitTag("q9", t), "X", Right)
 | BitTag("q9", t), "X" -> (BitTag("q9",t), "X", Right)
 | BitTag("q9", t), "#" -> (BitTag("q10",t), "#", Right)
 | BitTag("q9", t), sym -> (BitTag("q9", t), sym, Right)
 | BitTag("q10", t), "X" -> (BitTag("q10",t), "X", Right)
 | BitTag("q10", t), sym when t = sym  -> (Simple("rewind"), "X", Right)
 | Simple("checker"), "#" -> (Simple("checker"), "#", Right)
 | Simple("checker"), "_" -> (Simple("acc"), "_", Right)
 | Simple("checker"), "X" -> (Simple("checker"), "X", Right)
 | Simple("acc"), sym -> (Simple("acc"), sym, Right)
 | _, sym -> (Simple("rej"), sym, Right)
in
let states = [ 
Simple("rewind"); Simple("checker"); Simple("acc"); Simple("rej"); Simple("start"); Simple("q1"); Simple("q2"); Simple("q3"); Simple("q4"); Simple("q5"); Simple("q6");
BitTag("q7", "0"); BitTag("q8", "0"); BitTag("q9", "0"); BitTag("q10", "0");
BitTag("q7", "1"); BitTag("q8", "1"); BitTag("q9", "1"); BitTag("q10", "1");
BitTag("q7", "2"); BitTag("q8", "2"); BitTag("q9", "2"); BitTag("q10", "2");
BitTag("q7", "3"); BitTag("q8", "3"); BitTag("q9", "3"); BitTag("q10", "3");
BitTag("q7", "4"); BitTag("q8", "4"); BitTag("q9", "4"); BitTag("q10", "4");
BitTag("q7", "5"); BitTag("q8", "5"); BitTag("q9", "5"); BitTag("q10", "5");
BitTag("q7", "6"); BitTag("q8", "6"); BitTag("q9", "6"); BitTag("q10", "6");
BitTag("q7", "7"); BitTag("q8", "7"); BitTag("q9", "7"); BitTag("q10", "7");
BitTag("q7", "8"); BitTag("q8", "8"); BitTag("q9", "8"); BitTag("q10", "8");
BitTag("q7", "9"); BitTag("q8", "9"); BitTag("q9", "9"); BitTag("q10", "9")]
in
let alph = digits@[">"; "_"; "X"; "#"] in
let string_of st =
  match st with
    | Simple(s) -> s
    | BitTag(s,b) -> s^"|"^b in
transform string_of
  { tm_states = states;
  tm_input_alph = digits;
  tm_tape_alph = alph;
  tm_leftmost = ">";
  tm_blank = "_";
  tm_delta = make_delta states alph delta;
  tm_start = Simple("start");
  tm_accept = Simple("acc");
  tm_reject = Simple("rej") }
   



(* 
 *
 * QUESTION 3
 *
 *)



(*
 * Two-tape Turing machines
 *
 *)

type 'a tm2 = { tm2_states : 'a list;
                tm2_input_alph : symbol list;
                tm2_tape_alph : symbol list;
                tm2_leftmost : symbol;
                tm2_blank : symbol;
                tm2_delta : ('a * symbol * symbol * 'a * symbol * symbol * direction * direction) list;
                tm2_start : 'a;
                tm2_accept : 'a;
                tm2_reject : 'a }


(* 
 *     A sample two-tape TM that decides {u#u | u a string}
 *)

let pair = 
  let foreach l f = List.flatten (List.map f l)  in
  let states = [ "1";"2";"3";"4";"5";"6";"acc";"rej" ]  in
  let alph = ["0";"1";"#";"_";">"]  in
  let default = foreach states 
                  (fun q -> foreach alph 
                              (fun sym1 -> foreach alph
                                             (fun sym2 -> [(q,sym1,sym2,"rej",sym1,sym2,Stay,Stay)])))  in
  { tm2_states = states;
    tm2_input_alph = [ "0";"1";"#" ];
    tm2_tape_alph = alph;
    tm2_leftmost = ">";
    tm2_blank = "_";
    tm2_delta = [ ("1",">",">","2",">",">",Right,Right);
                  ("2","0","_","2","0","_",Right,Stay);
                  ("2","1","_","2","1","_",Right,Stay);
                  ("2","#","_","3","_","_",Right,Stay);
                  ("3","0","_","3","_","0",Right,Right);
                  ("3","1","_","3","_","1",Right,Right);
                  ("3","_","_","4","_","_",Left,Stay);
                  ("4","_","_","4","_","_",Left,Stay);
                  ("4","0","_","4","0","_",Left,Stay);
                  ("4","1","_","4","1","_",Left,Stay);
                  ("4",">","_","5",">","_",Stay,Stay);
                  ("5",">","_","5",">","_",Stay,Left);
                  ("5",">","0","5",">","0",Stay,Left);
                  ("5",">","1","5",">","1",Stay,Left);
                  ("5",">",">","6",">",">",Right,Right);
                  ("6","0","0","6","0","0",Right,Right);
                  ("6","1","1","6","1","1",Right,Right);
                  ("6","_","_","acc","_","_",Stay,Stay)
                ] @ default;
    tm2_start = "1";
    tm2_accept = "acc";
    tm2_reject = "rej" }



(* 
 *   Some code to run a two-tape TM on an input word
 *)

let run2 m w = 

  let print_config m (u,q,v,u2,v2) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_string " | "  in
    let _ = print_syms u2  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v2  in
    let _ = print_newline ()  in
    value   in

  let accepting_config m (u,q,v,_,_) = (q=m.tm2_accept)  in

  let rejecting_config m (u,q,v,_,_) = (q=m.tm2_reject)  in

  let halting_config m c = (accepting_config m c) || (rejecting_config m c)  in

  let step_config m (u,q,v,u2,v2) = 

    let rec find_match (q,a,b) delta = 
      match delta with
        [] -> failwith "No transition defined!"
      | ((q_,sym1_,sym2_,p_,rewrite1_,rewrite2_,dir1_,dir2_)::_) 
              when q=q_ && a=sym1_ && b=sym2_ -> (p_,rewrite1_,rewrite2_,dir1_,dir2_)
      | _::delta' -> find_match (q,a,b) delta'  in

    let rec last u = 
      match u with
        [] -> failwith "Moving Left from leftmost tape position"
      | [a] -> ([],a)
      | x::xs -> let (u',r) = last xs  in (x::u',r)   in

    if (halting_config m (u,q,v,u2,v2)) 
      then (u,q,v,u2,v2)
    else let (a,v') = match v with
                      | [] -> (m.tm2_blank,[])
                      | a::v' -> (a,v')  in
         let (a2,v2') = match v2 with
                      | [] -> (m.tm2_blank,[])
                      | a2::v2' -> (a2,v2')  in
         let (q',b,b2,dir,dir2) = find_match (q,a,a2) m.tm2_delta  in
         match dir,dir2 with
         | Left,Left -> let (u',c) = last u in 
                        let (u2',c2) = last u2 in (u',q',c::b::v',u2',c2::b2::v2')
         | Left,Right -> let (u',c) = last u in (u',q',c::b::v',u2@[b2],v2')
         | Left,Stay -> let (u',c) = last u in (u',q',c::b::v',u2,b2::v2')
         | Right,Left -> let (u2',c2) = last u2 in (u@[b],q',v',u2',c2::b2::v2')
         | Right,Right -> (u@[b],q',v',u2@[b2],v2')
         | Right,Stay -> (u@[b],q',v',u2,b2::v2')
         | Stay,Left -> let (u2',c2) = last u2 in (u,q',b::v',u2',c2::b2::v2')
         | Stay,Right -> (u,q',b::v',u2@[b2],v2')
         | Stay,Stay -> (u,q',b::v',u2,b2::v2')   in

  let starting_config = 
    ([], m.tm2_start, m.tm2_leftmost::(explode_str w),[],[m.tm2_leftmost])  in

  let rec loop c = 
    let _ = print_config m c c in
    if  (accepting_config m c) then true
    else if (rejecting_config m c) then false
    else loop (step_config m c)  in

  loop starting_config



type sim_state =
  NoTag of string
  | Tag1 of string * string
  | Tag2 of string * string * string
  | Tag3 of string * string * string * string
  | Tag4 of string * string * string * direction * direction
  | Tag5 of string * string * string * string * direction * direction
  | Push2 of string * string * direction
  | Push3 of string * string * direction * direction

let simulate2 tm2 = 
  let rec find_head alph pos = match alph with
    [] ->  false
    | a::alphs -> if pos = "^*"^a then true else (find_head alphs pos)
  in
  let rec get_a_value alph pos = match alph with
    [] ->  failwith "WTF"
    | a::alphs -> if pos = "^*"^a then "*"^a else ( get_a_value alphs pos)
  in
  let rec find_delta state h1 h2 delta = match delta with
    [] -> failwith "what the actual fuck"
    | (dq,dh1, dh2, dp, dn1, dn2, dd1, dd2)::deltas -> 
        if state = dq && h1 = "^*"^dh1 && h2 = "^*"^dh2 
          then Tag5("p4", dp, "^*"^dn1, "^*"^dn2, dd1, dd2) 
          else (find_delta state h1 h2 deltas)
  in
  let delta (p,a) = match p,a with
  (* 0. Set up the tape to represent two tapes, and set the state to the start state *)
   | NoTag("start"), ">" -> (NoTag("q1"), ">", Right)
   | NoTag ("q1"), "*_" -> (NoTag("q2"), "*_", Left) (* End has been found *)
   | NoTag("q1"), sym -> (NoTag("q1"), sym, Right) (* Keep moving to end *)
   | NoTag("q2"), "*_" -> (NoTag("q2"), "*_", Left) (* ignore blanks *)
   | NoTag("q2"), ">" -> (NoTag("q4"), ">", Right) (* If we have replaced everything then we are ready to add in new start *)
   | NoTag("q2"), sym -> (Tag1("q3", sym), "*_", Right) (* pick up value replace with black *)
   | Tag1("q3", sym), sym2 ->  (NoTag("q2"), sym, Left) (* Put down the pervious value and look for the next one *) 
   | NoTag("q4"), "*_" -> (NoTag("q5"), "^*>", Right) (* Add in new start string *)
   | NoTag("q5"), "*_" -> (NoTag("q6"), "|", Right) (* Add in new mid point string *)
   | NoTag("q5"), sym -> (NoTag("q5"), sym, Right) (* Skip until you get to the end *)
   | NoTag("q6"), "*_" -> (NoTag("rewind"), "^*>", Left) (* Add in second start point *)
   | NoTag("q6"), sym -> (NoTag("q6"),sym, Right) (* Skip until you get to the end *)
   | NoTag("rewind"), "^*>" -> (NoTag("start_sim"), "^*>", Left) (* reached the start of the tape, sim can begin *) 
   | NoTag("rewind"), sym -> (NoTag("rewind"), "*"^sym, Left) (* replace all values with sim values *)
   | NoTag("start_sim"), ">" -> (Tag1("p1", tm2.tm2_start), ">", Right)

(* 1. Say the simulated two-tapes Turing machine is in state q:
2. if q is accepting, accept
3. if q is rejecting, reject *)
   | NoTag("acc"), sym -> (NoTag("acc"), sym, Right)
   | Tag1("rewind2", "acc"), sym -> (NoTag("acc"), sym, Right)
   | Tag1("rewind2", "rej"), sym -> (NoTag("rej"), sym, Right)
   | NoTag("rej"), sym -> (NoTag("rej"), sym, Right)

(*  4. find the symbol s1 under the first tape head*)
   | Tag1("p1", state), sym -> 
    if (find_head (">"::tm2.tm2_tape_alph) sym) 
    then (Tag2("p2", state, sym), sym, Right) 
    else (Tag1("p1", state), sym, Right)

(* 5. find the symbol s2 under the second tape head *)
   | Tag2("p2", state, head1), sym -> 
   if (find_head (tm2.tm2_tape_alph) sym) 
    then (Tag3("p3", state, head1, sym), sym, Right) 
    else (Tag2("p2", state, head1), sym, Right)

(* 6. figure out what (q,s1,s2) should transition to: (p,n1,n2,d1,d2) *)
   | Tag3("p3", state, head1, head2), sym -> ( (find_delta state head1 head2 tm2.tm2_delta), sym, Left) 

(* 7. rewrite the symbol under the second tape head to n2 *)
   | Tag5("p4", p, n1, n2, d1, d2), sym -> if 
      (find_head (tm2.tm2_tape_alph) sym) 
        then (Tag4 ("p5", p, n1, d1, d2), n2, Left) 
        else (Tag5("p4", p, n1, n2, d1, d2), sym, Left)

(* 8. rewrite the symbol under the first tape head to n1 *)
   | Tag4("p5", p, n1, d1, d2), sym -> 
      if (find_head (tm2.tm2_tape_alph) sym) 
      then (Push3 ("p6", p, d1, d2), n1, Left) 
      else (Tag4("p5", p, n1, d1, d2), sym, Left)

(* 9. move the first tape head in direction d1 *)
   | Push3("p6", p, d1, d2), sym -> 
      if (find_head (tm2.tm2_tape_alph) sym) 
      then (Push3 ("p7", p, d1,d2), (get_a_value (tm2.tm2_tape_alph)  sym), d1) 
      else (Push3("p6", p, d1, d2), sym, Right)
   | Push3("p7", p, d1,d2), "*|" -> (Push3("grabB", p , d1,d2), "*|", Right)
   | Push3("p7", p, d1,d2), sym -> (Push2("p8", p , d2), "^"^sym, Right)

   | Push3("grabB",p,d1,d2), "*_" -> (Push3("grab2",p,d1,d2), "*_", Left)
   | Push3("grabB",p,d1,d2), sym -> (Push3("grabB", p , d1,d2), sym, Right)

   | Push3("grab2",p,d1,d2), "*_" -> (Push3("grab2",p,d1,d2), "*_", Left) (* ignore blanks *)
   | Push3("grab2",p,d1,d2), sym -> (Tag4("grab3",p,sym,d1,d2), "*_", Right) (* pick up value replace with black *)

   | Tag4("grab3",p,"*|",d1,d2), sym2 ->  (Push3("p6",p,d1,d2), "*|", Left) 
   | Tag4("grab3",p,sym,d1,d2), sym2 ->  (Push3("grab2",p,d1,d2), sym, Left) (* Put down the pervious value and look for the next one *) 

(* 10. move the second tape head in direction d2 *)
   | Push2("p8", p, d2), sym -> 
      if (find_head (tm2.tm2_tape_alph) sym) 
      then (Tag1 ("p9", p), (get_a_value  (tm2.tm2_tape_alph)  sym), d2) 
      else (Push2("p8", p, d2), sym, Right)
   | Tag1("p9", p), sym -> (Tag1("rewind2", p), "^"^sym, Right)

(*11. go step 1. *)
   | Tag1("rewind2", p), ">" -> (Tag1("p1", p), ">", Right)
   | Tag1("rewind2", p), sym -> (Tag1("rewind2", p), sym, Left)
   | _, sym -> (NoTag("rej"), sym, Right)
  in
  let rec q3 alphabet =  match alphabet with
    [] -> []
    | a::alphs -> (Tag1("q3", a))::(q3 alphs)
  in
  let rec tags t1val t2val t3val t4val t5val p2val p3val delt = 
    let run_t1 t1val s = (List.map (fun t1 -> Tag1(t1,s)) t1val) 
    in
    let run_t2 t2val s h1 = (List.map (fun t2 -> Tag2(t2,s,"^*"^h1)) t2val ) 
    in
    let run_t3 t3val s h1 h2 = (List.map (fun t3 -> Tag3(t3,s,"^*"^h1, "^*"^h2)) t3val ) 
    in
    let run_t4 t4val s h1 d1 d2 = (List.map (fun t4 -> Tag4(t4,s,"^*"^h1,d1,d2)) t4val) 
    in
    let run_t5 t5val s h1 h2 d1 d2 = (List.map (fun t5 -> Tag5(t5,s,"^*"^h1,"^*"^h2,d1,d2)) t5val) 
    in
    let run_p2 p2val s d1 = (List.map (fun p2 -> Push2(p2,s,d1)) p2val) 
    in
    let run_p3 p3val s d1 d2 = (List.map (fun p3 -> Push3(p3,s,d1,d2)) p3val) 
    in
  match delt with
    [] -> []
  | (q,h1, h2, p, n1, n2, d1, d2)::deltas -> (run_t1 t1val q)
                                            @(run_t2 t2val q h1)
                                            @(run_t2 t2val p n1)
                                            @(run_t3 t3val q h1 h2)
                                            @(run_t3 t3val p n1 n2)
                                            @(run_t4 t4val q h1 d1 d2 )
                                            @(run_t4 t4val p n1 d1 d2 )
                                            @(run_t5 t5val q h1 h2 d1 d2)
                                            @(run_t5 t5val p n1 n2 d1 d2)
                                            @(run_p2 p2val q d1)
                                            @(run_p2 p2val p d1)
                                            @(run_p3 p3val q d1 d2)
                                            @(run_p3 p3val p d1 d2)
                                            @(tags t1val t2val t3val t4val t5val p2val p3val deltas )
  in
let rec grab3 delta alphs = 
    let grabD2 a delta = (List.map (fun  (q,h1, h2, p, n1, n2, d1, d2) ->  Tag4("grab3",q,a,d1,d2)) delta)
  in
  match alphs with
  [] -> []
  | a::alp -> (grabD2 a delta)@(grab3 delta alp)
in
let rec make_alph alphs = match alphs with
[] -> [">"; "|"; "*|"]
| a::alphs -> [a; "^*"^a; "*"^a]@(make_alph alphs) in
let states =
 [ NoTag("start");NoTag("q6");NoTag("q1"); NoTag("q2"); NoTag("q4"); NoTag("q5"); NoTag("rewind"); NoTag("start_sim")]@(q3 tm2.tm2_tape_alph)@(tags ["p1"; "p9"; "rewind2"] ["p2"] ["p3"] ["p5"] ["p4"] ["p8"] ["p7";"p6";"grab2";"grabB"] tm2.tm2_delta)@(grab3 tm2.tm2_delta (make_alph tm2.tm2_tape_alph) )
in
let string_rep dir = match dir with
Stay -> "Stay"
| Left -> "Left"
| Right -> "Right"
in
let string_of st =
  match st with
    | NoTag(s) -> s
    | Tag1(s,s1) -> s^"|"^s1
    | Tag2(s,s1,a1)-> s^"|"^s1^"|"^a1
    | Tag3(s,s1,a1,a2)-> s^"|"^s1^"|"^a1^"|"^a2
    | Tag4(s,s1,a1,d1,d2)-> s^"|"^s1^"|"^a1^"|"^(string_rep d1)^"|"^(string_rep d2)
    | Tag5(s,s1,a1,a2,d1,d2)-> s^"|"^s1^"|"^a1^"|"^a2^"|"^(string_rep d1)^"|"^(string_rep d2)
    | Push2(s,s1,d1)-> s^"|"^s1^"|"^(string_rep d1)
    | Push3(s,s1,d1,d2)-> s^"|"^s1^"|"^(string_rep d1)^"|"^(string_rep d2) in
transform string_of
  { tm_states = states;
    tm_input_alph = tm2.tm2_input_alph;
    tm_tape_alph = (make_alph tm2.tm2_tape_alph);
    tm_leftmost = ">";
    tm_blank = "*_";
    tm_delta = (make_delta states (make_alph tm2.tm2_tape_alph) delta);
    tm_start = NoTag("start");
    tm_accept = NoTag("acc");
    tm_reject = NoTag("rej") }
   
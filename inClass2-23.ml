

let rec map (f,l) =
  match l with
  [] -> []
  | x::xs -> (f x)::(map  (f,xs));;

let square  x = x*x;;

map(square,[1;2;3;4;5]);;

map((fun x -> x * x), [1;2;3;4]);;

let add m = (fun n -> m + n);;

let f = add 10;;

f 33;;
(* should give you 43 *)
(* the problem is that the language decides if the m is independent of the function or remember the m that was present when the function was defined  *)

let add m n = m + n;;
(* this is a function that expects an argument and feeds it into a function that accepts the next argument *)

(add 10) 25;;

add 10 25;;

let add' (m,n) = m + n;;


(* implement:
filter: (a' -> bool) -> 'a list -> 'a list
Ex: filter (fun x -> x > 0) [0;1;-2;3;-4;5] = [1;3;5]
*)
let rec filter f xs = match xs with
  [] -> []
  | x::xs -> if f (x) then x::(filter f xs) else  (filter f xs);;

(*
removeNone: 'a option lsit -> 'a option list
Ex: removeNone [Some 1; None; Some 2; None] = [Some 1; Some 2] *)

let rec removeNone l = filter (fun x -> match x with Some y -> true | None -> false) l;;

(* this works for None because it is a special case but can get weird when you do things with Some that could have multiple types *)
let rec removeNone' l = filter (fun x -> x != None) l;;

let rec map_append f l =
  match l with
  [] -> []
  | x::xs -> (f x)@(map_append f xs);;

map_append (fun x -> [x;x+1;x+2]) [10;20;30;40];;

(* implement:

flatten: 'a list list -> 'a list
  Ex: flatten [[1;2];[3;4];[5];[];[6;7]] = [1;2;3;4;5;6;7]  *)

let flatten l = map_append (fun x -> x ) l;;


(* expand: (int * 'a) list -> 'a  list
  Ex: expand [(1, "a"); (3, "b"); (2, "c")] = ["a";"b";"b";"c";"c"]  *)

let rec gen (i,x) = if (i <= 0) then [] else x::(gen(i-1,x));;

let expand l = map_append (fun x -> gen x ) l;;


(* p in this case is a function *)
let new_filter p l = map_append (fun x -> if p(x) then [x] else []) l;; 

let cons a b = a::b;;

let app a b = a@b;;

let rec map_gen comb f l = 
  match l with
  [] -> []
  | x::xs -> comb (f x) (map_gen comb f xs);;

map_gen app (fun x -> x) [[1;2;3];[4]];;

let new_map f l = map_gen cons f l;;

new_map (fun x -> x * x) [1;2;3;4;5];;

(* but you can combine comb  and f together *)
let rec map_gen2 comb l =
  match l with
  [] -> []
  | x::xs -> comb x (map_gen2 comb xs);;

let expand_comb (i,x) l = gen(i,x)@l;;


map_gen2 expand_comb [(1, "a"); (2,"b")]
(*  but not all lists return lists *)

let rec sum l =
  match l  with 
  [] -> 0
  | x::xs -> x + sum xs;;

(* orrrr *)

let rec sum' l =
  match l  with 
  [] -> 0
  | x::xs -> add x sum xs;;

(*  this looks very similar to map_gen the only real difference between them is the result of the base case so... *)

let rec fold_right b comb l = 
  match l  with
  [] -> b
  | x::xs -> comb x (fold_right b comb xs);;

(*  so some now becomes *)
fold_right 0 (fun x sum_rest -> x + sum_rest) [1;2;3;4;5;6];;

(* everything fold_right does is parameterise everything *)
(* fold_right is the essence of recursian over lists *)
(* all the function does is recursian *)
(* why use recursian when you can fold_right *)
(* this prevents us from recusing forever. fold_right will never give you an infinate loop     *)

(* all this stuff is in the List module BUT THE PARAMETERS ARE IN A DIFFERENT ORDER *)

let squares l = List.map (fun x -> x * x) l;;

squares [1;2;3;4];;

let squares = List.map (fun x -> x * x);;

squares [1;2;3;4];;

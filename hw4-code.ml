(* FoCS Spring 2015

   Homework 4 code


   Name:

   Email:

   Comments:

 *)





(*
 *
 *  QUESTION 1 
 * 
 *
 *)


let rec last l = 
  match l with
    [] -> None
  | [x] -> Some(x)
  | x::xs -> last(xs)


let predicate_opt f p = if f p then Some p else None;;

let positive = predicate_opt (fun x -> x >= 0);;

let map_opt f p = match p with
  None -> None
  | (Some p) -> Some (f p);;

let comb_opt f p1 p2  = match p2 with
    None -> None 
    | (Some p2) -> p2;;

let default v i = match i with 
  None -> v
  |Some i -> i;;

let compose_opt f g i = match f i with
  None -> None
  | Some o -> match g o with
    None -> None
    | Some o2 -> Some o2;;

let positive_last = compose_opt last positive;;

(*
 * 
 * QUESTION 2 
 * 
 * 
 *)


let a_least n p xs = failwith "not implemented"

let max_list xs = failwith "not implemented"

let map_funs fs x = failwith "not implemented"

let map_cross fs xs = failwith "not implemented"



(*
 * 
 * QUESTION 3
 * 
 * 
 *)


let suffixes xs = failwith "not implemented"

let prefixes xs = failwith "not implemented"

let inject a xs = failwith "not implemented"

let perms xs = failwith "not implemented"




(*
 * 
 * QUESTION 4
 * 
 * 
 *)


type 'a bintree = 
    EmptyTree 
  | Node of 'a * 'a bintree * 'a bintree

let sample = 
  Node (10, Node (3, Node (7, EmptyTree, EmptyTree),
                     Node (5, EmptyTree, EmptyTree)),
            Node (6, Node (99, EmptyTree, 
                               Node (66, EmptyTree, EmptyTree)),
                     EmptyTree))
                                 
let print_tree t = 
  let print_typ_tree f t = 
    let emptyString n = String.make n ' '  in
    let ljustify n s = s ^ (emptyString (n - (String.length s)))  in
    let height p = List.length p  in
    let width p = List.fold_right (fun s m -> max (String.length s) m) p 0  in
    let rec copy n x = 
      if (n <= 0)
        then []
      else x :: copy (n - 1) x  in
    let empty h w = copy h (emptyString w)  in
    let above p q = 
      let w = max (width p) (width q)
      in (List.map (ljustify w) p) @ (List.map (ljustify w) q)  in
    let beside p q = 
      let h = max (height p) (height q)  in
      let heighten h p = above p (empty (h - List.length p) (width p))
      in List.map2 (^) (heighten h p) (heighten h q)  in
    let string_picture p = (String.concat "\n" p)^"\n"  in
    let print_picture p = Printf.printf "%s" (string_picture p)  in
    let rec picture_tree f t = 
      match t with
        EmptyTree -> [" "]
      | Node (v,EmptyTree,EmptyTree) -> [f v]
      | Node (v,EmptyTree,r) -> above [f v]
            (above ["---|"]
               (beside ["   "] (picture_tree f r)))
      | Node (v,l,EmptyTree) -> above [f v]
            (above ["|"] 
               (picture_tree f l))
      | Node (v,l,r) -> let sub_l = picture_tree f l in
        let sub_r = picture_tree f r
        in above [f v]
          (above ["|"^(String.make (2 + width sub_l) '-')^"|"]
             (beside sub_l (beside ["   "] sub_r)))
    in print_picture (picture_tree f t) in
  print_typ_tree string_of_int t


let rec mapT f t = 
  match t with
    EmptyTree -> EmptyTree
  | Node (v,l,r) -> Node (f v, mapT f l, mapT f r)

let rec foldT f t b = 
  match t with
    EmptyTree -> b
  | Node (v,l,r) -> f v (foldT f l b) (foldT f r b)

let size t = foldT (fun v l r -> 1 + l + r) t 0
let sum t = foldT (fun v l r -> v + l + r) t 0


let height t = failwith "not implemented"

let height' t = failwith "not implemented"

let fringe t = failwith "not implemented"

let fringe' t = failwith "not implemented"


let inorder t = failwith "not implemented"

let preorder t = failwith "not implemented"

let postorder t = failwith "not implemented"


let make_tree xs = failwith "not implemented"



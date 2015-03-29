  if x mod 2 = 0 then Some(x) else None

let predicate_opt f x = 
  if f x then Some(x) else None


let rec last l = 
  match l with
    [] -> None
  | [x] -> Some(x)
  | x::xs -> last(xs)

let square_opt x =
  match x with
    None -> None
  | Some(v) -> Some(v*v)

let add_opt x y = 
  match x,y with
    None,_ -> None
  | _,None -> None
  | Some(i),Some(j) -> Some(i+j)

let map_opt f x =
   match x with
     None -> None
   | Some(v) -> Some(f v)

let comb_opt f x y = 
   match x,y with
     None,_ -> None
   | _,None -> None
   | Some(v),Some(w) -> Some(f v w)

let default d x = 
  match x with
    None -> d
  | Some (v) -> v

let flatten_opt x = 
  match x with
    None -> None
  | Some (None) -> None
  | Some (Some v) -> Some (v)

let compose_opt f g = 
  (fun x -> 
    (match f x with
      None -> None
    | Some v -> g v))



(* Q2 *)



let at_least n p xs = 
  (List.fold_right (fun x r -> if (p x) then r + 1 else r) xs 0) >= n


let max_list xs = 
  let max' x yo = match yo with None -> Some x
                              | Some y -> Some (max x y)  in
  List.fold_right max' xs None
let map_funs fs x = 
  List.map (fun f -> f x) fs

let map_cross fs xs = 
  List.fold_right (fun x r -> (map_funs fs x) @ r) xs []




(* Q3 *) 

let suffixes xs = 
  List.fold_right (fun x xs' -> (x::List.hd xs')::xs') xs [[]]

let prefixes xs = 
  let prepend x yss = List.map (fun ys -> x :: ys) yss  in
  List.fold_right (fun x yss -> [] :: (prepend x yss)) xs [[]]

(* recursive version *)
let rec inject a xs =
  match xs with
    [] -> [[a]]
  | x :: xs' -> (a::xs) :: List.map (fun ys -> x::ys) (inject a xs')


(* non-recursive version with two folds and a map! *)
(* basically attach the list to every element of the list:
     [a;b;c] -> [ (a,[a;b;c]); (b,[b;c]); (c,[c]) ]
   once you have that information, you have all the data to transform
   the above recursive inject into a fold_right *)

let inject a xs = 
  let attachList xs = 
    List.fold_right (fun x r ->  match r with 
                                   [] -> [(x,[x])] 
                                 | (y,ys)::_ -> (x,x::ys)::r)
                        xs []  in
  let prepend x yss = List.map (fun ys -> x :: ys) yss  in
  List.fold_right 
       (fun (x,xs) r -> (a::xs) :: (prepend x r))
         (attachList xs) [[a]]


let perms xs = 
  let fold1 x xss = List.fold_right (fun ys zss -> (inject x ys)@zss) xss []
  in List.fold_right fold1 xs [[]]



(* Q4  *)


type 'a bintree = 
    EmptyTree 
  | Node of 'a * 'a bintree * 'a bintree

let sample = 
  Node (10, Node (3, Node (7, EmptyTree, EmptyTree),
                     Node (5, EmptyTree, EmptyTree)),
            Node (6, Node (99, EmptyTree, 
                               Node (66, EmptyTree, EmptyTree)),
                     EmptyTree))
                                 

let rec size t =
  match t with
    EmptyTree -> 0
  | Node (_,l,r) -> 1 + (size l) + (size r)

let rec sum t = 
  match t with
    EmptyTree -> 0
  | Node (v,l,r) -> v + (sum l) + (sum r)

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

let rec height t = 
  match t with
    EmptyTree -> 0
  | Node (v,l,r) -> 1 + max (height l) (height r)

let height' t = foldT (fun v l r -> 1 + max l r) t 0

let rec fringe t = 
  match t with
    EmptyTree -> []
  | Node (v,EmptyTree,EmptyTree) -> [v]
  | Node (_,l,r) -> (fringe l)@(fringe r)

let fringe' t = 
  foldT (fun v l r -> match l,r with [],[] -> [v] | _,_ -> l@r) t []


let rec split xs = 
  match xs with
    [] -> ([],[])
  | [a] -> ([a],[])
  | x1 :: x2 :: xs' -> let (ys,zs) = split xs'
                       in (x1::ys, x2::zs)

let rec make_tree xs = 
  match xs with
    [] -> EmptyTree
  | x :: xs' -> let (ys,zs) = split xs'
                in Node (x,make_tree ys, make_tree zs)



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
  in print_picture (picture_tree f t)

let print_tree t = print_typ_tree string_of_int t

let s = Node (1,Node(2,Node(3,EmptyTree,EmptyTree),EmptyTree),
              Node(4,EmptyTree,EmptyTree))
let t = Node(99,s,s)
let u = Node(66,s,t)
let v = Node(33,t,u)

let inorder t = foldT (fun v l r -> l@[v]@r) t []
let preorder t = foldT (fun v l r -> [v]@l@r) t []
let postorder t = foldT (fun v l r -> l@r@[v]) t []


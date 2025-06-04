let processa lst =
  let rec aux last acc = function
   | [] -> List.rev acc
   | hd::tl -> aux hd ((hd,hd-last)::acc) tl
  in aux 0 [] lst

let process l =
  let _,res = List.fold_left(fun (last,acc) e -> e,(e,e-last) :: acc) (0,[]) l in
  List.rev res


let combina l =
  let rec aux acc1 acc2= function
  | [] -> (List.rev acc1,List.rev acc2)
  | (hd,hd2)::tl ->
    let a,b =hd mod 2 = 0 , hd2 mod 2 = 0 in
    if a && b then aux acc1 (hd::hd2::acc2) tl else if
      not a && not b then aux (hd::hd2::acc1) acc2 tl else if 
        not a && b then aux (hd::acc1) (hd2::acc2) tl else
          aux (hd2::acc1) (hd::acc2) tl 
in aux [] [] l


let combine l =
  let lst = List.fold_left (fun acc (a,b) -> a::b::acc) [] l in
  List.partition(fun x -> x mod 2 <>  0 ) (List.rev lst )
 
type 'a tree = 
  | Leaf 
  | Node of 'a tree * 'a * 'a tree


let t = Node(Node(Leaf,3,Leaf),5,Node(Node(Leaf,6,Leaf),7,Node(Leaf,9,Leaf)))
let rec inverse tree = 
  match tree with
  | Leaf -> Leaf
  | Node (l,e,r) -> Node(inverse r,e,inverse l)


let rec same_shape t1 t2 =
  match t1,t2 with
  | Leaf,Leaf -> true
  | Leaf,Node (l,e,r) -> false
  | Node(l,e,r),Leaf -> false
  | Node (l,e,r),Node (l1,e1,r1) -> same_shape l l1 && same_shape r r1
type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree


let t = Node(Node(Leaf,3,Leaf),42,Node(Node(Leaf,2,Leaf),5,Leaf))

let rec count_leafs t=
  match t with
  | Leaf -> 1
  | Node (l,e,r) -> count_leafs l + count_leafs r




let count_leafs_opt t =
  let rec aux acc = function
  | Leaf -> 1 + acc
  | Node (l,e,r) -> aux(aux acc l ) r (*primeiro faz as leafs da esq para depois ir para a direita com o acc da esq*)
in aux 0 t


let escreve t =
  let rec aux  = function
  | Leaf -> ()
  | Node (l,e,r) -> aux l; print_int e; print_string " ";aux r
in aux t; print_newline ()



let nodes_leaf t =
  let rec aux acc = function
    | Leaf -> acc
    | Node (l,e,r) -> aux (if l = Leaf && r = Leaf then aux (acc @ [e]) l else aux acc l) r
    in aux [] t  


let nodes_leaf2 t =
  let rec aux acc = function
  | Leaf -> acc
  | Node(Leaf,e,Leaf) -> e::acc
  | Node(l,e,r) -> aux( aux acc l ) r
in aux [] t 

  let depth t =
    let rec aux acc = function 
    | Leaf -> acc
    | Node (l,e,r) -> max (aux (acc+1) r) (aux (acc+1 ) l);
  in aux 0 t

let p1 = [3;0;5;0;1]

let horner p x =
  List.fold_left(fun acc coef -> acc*.x +. (float_of_int coef)) (float_of_int (List.hd p)) (List.tl p)
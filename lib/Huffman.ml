open Utils

type t = 
| Leaf of char*int
| Node of t*t*int
[@@deriving show]

let get_priority = function
| Leaf (_ , i) -> i
| Node (_, _, i) -> i

let compare t1 t2 = 
  compare (get_priority t1) (get_priority t2)

let rec insert_ordered tree = function
| [] -> [tree]
| h :: tail as l ->
  if (get_priority tree) <= (get_priority h) then tree :: l
  else h :: (insert_ordered tree tail)

let rec of_leaf_list = function
  | [] -> failwith "Empty list"
  | [t] -> t
  | t1 :: t2 :: tail ->
    let f1 = get_priority t1 in
    let f2 = get_priority t2 in
    let node = Node (t1, t2, f1 + f2) in
    of_leaf_list (insert_ordered node tail)

let to_leaf_list tree = 
  let rec aux acc = function
  | Leaf _ as leaf -> leaf :: acc
  | Node (left, right, _) -> aux (aux acc right) left
  in
aux [] tree |> List.sort compare


let init s = 
  s
  |> frequency
  |> List.map (fun (c, i) -> Leaf (c, i))
  |> of_leaf_list

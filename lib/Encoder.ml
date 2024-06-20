open Huffman

let rec encode_char ch = function
| Leaf (c, _) -> if c == ch then "" else failwith "Not found"
| Node (left, right, _) -> 
  try "0" ^ encode_char ch left
  with Failure _ -> "1" ^ encode_char ch right


let encode' s tree = 
  s
  |> String.to_seq
  |> Seq.map (fun c -> encode_char c tree)
  |> List.of_seq
  |> String.concat ""

let encode s =
  let tree = init s in
  encode' s tree, tree

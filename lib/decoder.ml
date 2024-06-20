open Huffman

let rec decode' (tree: t) (acc: t) = function
| [] -> 
  (match acc with
  | Leaf (c, _) -> String.make 1 c
  | _ -> "")

| h::tail as l ->
  (match acc with
  | Leaf(c, _) -> 
    (String.make 1 c) ^ decode' tree tree l
  | Node(left, right, _) ->
    if h == '0' then decode' tree left tail
    else decode' tree right tail)

  
let decode msg tree = 
  msg
  |> String.to_seq
  |> List.of_seq
  |> decode' tree tree
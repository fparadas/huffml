let rec count_repetitions c = function
| [] -> 0
| h :: tail -> 
 let offset = if c == h then 1 else 0 in
 offset + count_repetitions c tail 


let frequency s = 
  let l = s
    |> String.to_seq
    |> List.of_seq 
  in
  
  l
  |> List.sort_uniq Char.compare
  |> List.map (fun c -> (c, count_repetitions c l))



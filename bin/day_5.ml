let day_5_file = "data/day_5.txt"

let parse lines =
  let rec split_sections ?(is_available = false) lines ranges available =
    match lines with
    | [] -> (ranges, available)
    | "" :: tail -> split_sections ~is_available:true tail ranges available
    | head :: tail when is_available ->
        split_sections ~is_available:true tail ranges (head :: available)
    | head :: tail -> split_sections tail (head :: ranges) available
  in

  let parse_range range_str =
    match String.split_on_char '-' range_str with
    | [ start_str; end_str ] ->
        let start_num = int_of_string start_str in
        let end_num = int_of_string end_str in
        (start_num, end_num)
    | _ -> failwith ("Invalid range format: " ^ range_str)
  in

  let ranges, available = split_sections lines [] [] in

  let ranges = List.map parse_range ranges in
  let available = List.map int_of_string available in

  (List.rev ranges, List.rev available)

let merge_ranges ranges =
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) ranges in
  let rec merge acc ranges =
    match ranges with
    | [] -> List.rev acc
    | (low, high) :: tail -> (
        match acc with
        | [] -> merge [ (low, high) ] tail
        | (acc_low, acc_high) :: acc_tail when low <= acc_high + 1 ->
            merge ((acc_low, max acc_high high) :: acc_tail) tail
        | _ -> merge ((low, high) :: acc) tail)
  in
  merge [] sorted

let is_fresh ranges item =
  List.exists
    (fun (start_num, end_num) -> item >= start_num && item <= end_num)
    ranges

let run () =
  let ranges, _ =
    In_channel.with_open_text day_5_file In_channel.input_lines |> parse
  in

  ranges |> merge_ranges
  |> List.fold_left (fun sum (low, high) -> sum + (high - low + 1)) 0
  |> Printf.printf "Day 5 - Final Result: %d\n"

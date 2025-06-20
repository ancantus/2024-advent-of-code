open! Core

type location_entry = {id: int; index: int}

let compare_ids lhs rhs =
  match (phys_equal lhs.id rhs.id, lhs.id > rhs.id) with
  | true, _ ->
      0
  | _, true ->
      1
  | _, false ->
      -1

let load_id_file file =
  let parse_line index l =
    Scanf.sscanf l "%d %d" (fun i1 i2 -> ({id= i1; index}, {id= i2; index}))
  in
  In_channel.with_file file ~f:(fun ic ->
      let ids_1, ids_2 =
        In_channel.input_lines ic |> List.mapi ~f:parse_line |> List.unzip
      in
      (Array.of_list ids_1, Array.of_list ids_2) )

let find_distance (local_ lhs) (local_ rhs) ~(local_ verbose) =
  if verbose then
    printf "(%d: %d) -> (%d: %d) = %d\n" lhs.id lhs.index rhs.id rhs.index
      (Int.abs (lhs.id - rhs.id)) ;
  Int.abs (lhs.id - rhs.id)

let similarity_score ~table ~(local_ verbose) id =
  let match_count =
    Array.fold_until ~init:0
      ~f:(fun acc a ->
        match a.id with
        | x when x < id ->
            Continue acc
        | x when phys_equal x id ->
            Continue (acc + 1)
        | _ ->
            Stop acc )
      ~finish:(fun acc -> acc)
      table
  in
  if verbose then printf "%d * %d = %d\n" id match_count (id * match_count) ;
  id * match_count

let command =
  Command.basic ~summary:"Location Id Un-Muxer (AOC-2024-1)"
    [%map_open.Command
      let file = anon (maybe_with_default "input.txt" ("FILE" %: string))
      and verbose = flag "verbose" no_arg ~doc:"Enable verbose output." in
      fun () ->
        let ids_1, ids_2 = load_id_file file in
        Array.stable_sort ~compare:compare_ids ids_1 ;
        Array.stable_sort ~compare:compare_ids ids_2 ;
        Array.map2_exn
          ~f:(fun lhs rhs -> find_distance ~verbose lhs rhs)
          ids_1 ids_2
        |> Array.fold ~init:0 ~f:(fun acc a -> acc + a)
        |> printf "total distance: %d\n" ;
        let score_sim =
          Memo.general (fun id -> similarity_score ~verbose ~table:ids_2 id)
        in
        Array.fold ~init:0 ~f:(fun acc a -> acc + score_sim a.id) ids_1
        |> printf "similarity score: %d\n"]

let () = Command_unix.run command

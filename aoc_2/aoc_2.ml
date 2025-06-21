open! Core

let load_reports file =
  let parse_report line =
    String.split ~on:' ' line |> List.map ~f:Int.of_string |> List.to_array
  in
  In_channel.with_file file ~f:(fun ic ->
      let rec read_loop acc =
        match In_channel.input_line ic with
        | None ->
            acc
        | Some line ->
            read_loop (line :: acc)
      in
      read_loop [] )
  |> List.map ~f:parse_report

type safety_rating =
  | Unsafe_Increment of int * int
  | Unsafe_Stationary of int
  | Unsafe_Wiggle
  | Safe

let rating_to_string rating =
  match rating with
  | Safe ->
      "safe"
  | Unsafe_Wiggle ->
      "unsafe due to wiggle"
  | Unsafe_Stationary n ->
      "unsafe due to repeated " ^ Int.to_string n
  | Unsafe_Increment (p, n) ->
      "unsafe due to increment of size " ^ Int.to_string (n - p)

type assessment = Init | Unknown of int | Rising of int | Falling of int

let assessment_to_int (local_ a) =
  match a with Init -> None | Unknown p | Rising p | Falling p -> Some p

let elementwise_assessment (local_ prev) next =
  let equality_assessment (local_ prev) n =
    match assessment_to_int prev with
    | None ->
        None
    | Some p ->
        if phys_equal p n then Some (Unsafe_Stationary n) else None
  in
  let increment_assessment ~(local_ max) (local_ prev) n =
    match assessment_to_int prev with
    | None ->
        None
    | Some p ->
        if n - p |> Int.abs > max then Some (Unsafe_Increment (p, n)) else None
  in
  let eq_test = equality_assessment prev next in
  let inc_test = increment_assessment ~max:3 prev next in
  match (eq_test, inc_test, prev) with
  | _, _, Init ->
      Base.Container.Continue_or_stop.Continue (Unknown next)
  | Some e, _, _ ->
      Stop e
  | _, Some e, _ ->
      Stop e
  | _, _, Unknown p ->
      if p < next then Continue (Rising next) else Continue (Falling next)
  | _, _, Rising p ->
      if p < next then Continue (Rising next) else Stop Unsafe_Wiggle
  | _, _, Falling p ->
      if p > next then Continue (Falling next) else Stop Unsafe_Wiggle

let safety_assessment ~(local_ verbose) ~(local_ dampened) report =
  let print_report r =
    Array.fold ~init:"" ~f:(fun acc a -> acc ^ " " ^ Int.to_string a) r
  in
  if verbose then printf "report:%s\n" (print_report report) ;
  let gen_result =
    Array.fold_until ~init:Init ~f:elementwise_assessment ~finish:(fun _ ->
        Safe )
  in
  let sub_report ~index r =
    Array.foldi ~init:[]
      ~f:(fun i acc a -> if phys_equal i index then acc else a :: acc)
      r
    |> List.rev |> List.to_array
  in
  let result =
    match (gen_result report, dampened) with
    | Safe, _ ->
        Safe
    | e, false ->
        e
    | e, true ->
        let has_fault =
          Array.for_alli
            ~f:(fun i _ ->
              let r = sub_report ~index:i report in
              match gen_result r with Safe -> false | _ -> true )
            report
        in
        if has_fault then e else Safe
  in
  if verbose then printf "\tsafety rating: %s\n" (rating_to_string result) ;
  result

let command =
  Command.basic ~summary:"Red Nose Reactor Safety Assessor (AOC-2024-2)"
    [%map_open.Command
      let file = anon (maybe_with_default "input.txt" ("FILE" %: string))
      and verbose = flag "verbose" no_arg ~doc:"Enable verbose output."
      and dampened =
        flag "dampened" no_arg ~doc:"Enable the problem dampener."
      in
      fun () ->
        let reports = load_reports file in
        List.fold ~init:0
          ~f:(fun acc a ->
            match safety_assessment ~verbose ~dampened a with
            | Safe ->
                acc + 1
            | _ ->
                acc )
          reports
        |> printf "safe report count: %d\n"]

let () = Command_unix.run command

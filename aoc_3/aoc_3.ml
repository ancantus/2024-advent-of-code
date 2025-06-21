open! Core

type instructions = Mul of int * int | Do | Dont

let instruction_to_string i =
  match i with
  | Mul (x, y) ->
      sprintf "mul(%d, %d)" x y
  | Do ->
      "do"
  | Dont ->
      "don't"

type parse_state = Invalid | Partial | Complete

let load_instructions file =
  let parse_static_instr ~instruction s =
    let max_s_index = String.length s - 1 in
    String.fold_until ~init:0
      ~f:(fun i a ->
        if i > max_s_index then Stop Partial
        else if not (Char.equal__local (String.get s i) a) then Stop Invalid
        else Continue (i + 1) )
      ~finish:(fun _ -> Complete)
      instruction
  in
  let parse_do = parse_static_instr ~instruction:"do()" in
  let parse_dont = parse_static_instr ~instruction:"don't()" in
  let parse_mul = parse_static_instr ~instruction:"mul(" in
  In_channel.with_file file ~f:(fun ic ->
      let ic = Scanf.Scanning.from_channel ic in
      let rec read_loop buff acc =
        let buff =
          match Scanf.bscanf_opt ic "%c" (fun c -> buff ^ String.make 1 c) with
          | None ->
              ""
          | Some b ->
              b
        in
        if phys_equal (String.length buff) 0 then acc
        else
          match (parse_do buff, parse_dont buff, parse_mul buff) with
          | Invalid, Invalid, Invalid ->
              read_loop "" acc
          | Complete, _, _ ->
              read_loop "" (Do :: acc)
          | _, Complete, _ ->
              read_loop "" (Dont :: acc)
          | _, _, Complete -> (
            match
              Scanf.bscanf_opt ic "%d,%d)" (fun x y ->
                  if x > 999 || y > 999 || x < 0 || y < 0 then None
                  else Some (Mul (x, y)) )
            with
            | None | Some None ->
                read_loop "" acc
            | Some (Some i) ->
                read_loop "" (i :: acc) )
          | Partial, _, _ | _, Partial, _ | _, _, Partial ->
              read_loop buff acc
      in
      read_loop "" [] )
  |> List.rev

let execute instructions =
  let rec loop mult_flag acc i =
    let next =
      match i with
      | [] ->
          Null
      | Do :: tail ->
          This (true, 0 :: acc, tail)
      | Dont :: tail ->
          This (false, 0 :: acc, tail)
      | Mul (x, y) :: tail ->
          This
            (mult_flag, (if mult_flag then (x * y) :: acc else 0 :: acc), tail)
    in
    match next with
    | Null ->
        acc
    | This (mult_flag, acc, i) ->
        loop mult_flag acc i
  in
  loop true [] instructions |> List.rev

let command =
  Command.basic ~summary:"Computer Memory Decorrupter (AOC-2024-3)"
    [%map_open.Command
      let file = anon (maybe_with_default "input.txt" ("FILE" %: string))
      and verbose = flag "verbose" no_arg ~doc:"Enable verbose output." in
      fun () ->
        let instructions = load_instructions file in
        let exec_output = execute instructions in
        if verbose then (
          printf "instructions: \n" ;
          List.iter2_exn
            ~f:(fun i o -> printf "\t%s = %d\n" (instruction_to_string i) o)
            instructions exec_output ) ;
        let sum = List.fold ~init:0 ~f:(fun acc o -> acc + o) exec_output in
        printf "sum of instructions: %d\n" sum]

let () = Command_unix.run command

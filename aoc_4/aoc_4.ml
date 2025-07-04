open! Core

type word_search = {data: string; shape: int * int}

(* Attempt to use SIMD but oxcaml 5.2.0 has a compiler issue with it
type word_search = {data: int8x16# array; shape: int * int}
module Int8x16 = Ocaml_simd_sse.Int8x16

let load_word_search file =
  In_channel.with_file file ~f:(fun ic ->
      let to_vector_array (local_ str) =
        let to_vector i = Int8x16.String.unsafe_get str ~byte:i  in
        let len = String.length str in
        let simd_padding = mod len 16 in
        let str = if simd_padding > 0 then str ^ (String.make simd_padding ' ') else str in
        let simd_length = (len / 16) + (if simd_padding > 0 then 1 else 0) in
        len, [| to_vector x for x = 0 to simd_length |]
      in
      let rec read_loop line_length acc =
        match In_channel.input_line ic with
        | None ->
            (match line_length with This l -> l | Null -> failwith "no lines parsed", List.length acc), [|
            c for l in acc for c in l |]
        | Some s ->
            let size, arr = to_vector_array s in
            let line_length = match line_length with | None -> This size | This l -> if phys_equal size l then This l else failwith "line length mismatch" in
            read_loop line_length (arr :: acc)
      in
      let shape, data in read_loop Null [] in
      {shape, data})
*)
let load_word_search file =
  In_channel.with_file file ~f:(fun ic ->
      let rec read_loop line_length acc =
        match In_channel.input_line ic with
        | None ->
            ( (line_length, List.length acc)
            , List.rev acc |> String.concat ~sep:"" )
        | Some s ->
            let size = String.length s in
            read_loop size (s :: acc)
      in
      let shape, data = read_loop 0 [] in
      {shape; data} )

let to_string word_search =
  let line_length, num_lines = word_search.shape in
  let get_line num =
    let starti = num * line_length in
    String.slice word_search.data starti (starti + line_length)
  in
  [|get_line i for i = 0 to num_lines - 1|]
  |> Array.fold
       ~init:(Buffer.create (((line_length * 2) + 1) * num_lines))
       ~f:(fun acc line ->
         let append = Buffer.add_char acc in
         String.iter line ~f:(fun c -> append ' ' ; append c) ;
         append '\n' ;
         acc )
  |> Buffer.contents

let scan_region ~f rows columns =
  let scan_points = [|(x, y) for x = 0 to columns - 1 and y = 0 to rows - 1|] in
  let search_map =
    Array.map ~f:(fun (x, y) -> if f x y then 1 else 0) scan_points
  in
  ( Array.fold ~init:0 ~f:(fun acc a -> acc + a) search_map
  , Array.fold2_exn ~init:[]
      ~f:(fun acc p found -> if phys_equal found 1 then p :: acc else acc)
      scan_points search_map )

let check_wordsearch ~ws ~f rows columns =
  let line_length, _ = ws.shape in
  f (columns + (rows * line_length))

let scan_horiz ~query word_search =
  let kernel_length = String.length query in
  let rev_query = String.rev query in
  let check_query start =
    String.is_substring_at ~pos:start ~substring:query word_search.data
    || String.is_substring_at ~pos:start ~substring:rev_query word_search.data
  in
  let line_length, num_lines = word_search.shape in
  scan_region
    (line_length - kernel_length + 1)
    num_lines
    ~f:(check_wordsearch ~ws:word_search ~f:check_query)

let scan_vert ~query word_search =
  let kernel_length = String.length query in
  let rev_query = String.rev query in
  let line_length, num_lines = word_search.shape in
  let check_query q start =
    String.findi
      ~f:(fun i c ->
        String.get word_search.data (start + (i * line_length))
        |> Char.equal__local c |> not )
      q
    |> Option.is_none
  in
  let scanner =
    check_wordsearch ~ws:word_search ~f:(fun i ->
        check_query query i || check_query rev_query i )
  in
  scan_region line_length (num_lines - kernel_length + 1) ~f:scanner

let scan_down_diag ~query word_search =
  let kernel_length = String.length query in
  let rev_query = String.rev query in
  let line_length, num_lines = word_search.shape in
  let check_down_diag q start =
    let get_down_diag i =
      String.get word_search.data (start + i + (i * line_length))
    in
    String.findi ~f:(fun i c -> get_down_diag i |> Char.equal__local c |> not) q
    |> Option.is_none
  in
  let scanner =
    check_wordsearch ~ws:word_search ~f:(fun i ->
        check_down_diag query i || check_down_diag rev_query i )
  in
  scan_region
    (line_length - kernel_length + 1)
    (num_lines - kernel_length + 1)
    ~f:scanner

let scan_up_diag ~query word_search =
  let kernel_length = String.length query in
  let rev_query = String.rev query in
  let line_length, num_lines = word_search.shape in
  let check_up_diag q start =
    let get_up_diag rows =
      let cols = kernel_length - rows - 1 in
      String.get word_search.data (start + cols + (rows * line_length))
    in
    String.findi ~f:(fun i c -> get_up_diag i |> Char.equal__local c |> not) q
    |> Option.is_none
  in
  let scanner =
    check_wordsearch ~ws:word_search ~f:(fun i ->
        check_up_diag query i || check_up_diag rev_query i )
  in
  scan_region
    (line_length - kernel_length + 1)
    (num_lines - kernel_length + 1)
    ~f:scanner

let scan_cross ~query word_search =
  let kernel_length = String.length query in
  let rev_query = String.rev query in
  let line_length, num_lines = word_search.shape in
  let check_cross start =
    let get_up_diag rows =
      let cols = kernel_length - rows - 1 in
      String.get word_search.data (start + cols + (rows * line_length))
    in
    let get_down_diag i =
      String.get word_search.data (start + i + (i * line_length))
    in
    let find_match ~get_char q =
      String.findi ~f:(fun i c -> get_char i |> Char.equal__local c |> not) q
      |> Option.is_none
    in
    let up_match_found =
      find_match ~get_char:get_up_diag query
      || find_match ~get_char:get_up_diag rev_query
    in
    let down_match_found =
      find_match ~get_char:get_down_diag query
      || find_match ~get_char:get_down_diag rev_query
    in
    up_match_found && down_match_found
  in
  let scanner = check_wordsearch ~ws:word_search ~f:check_cross in
  scan_region
    (line_length - kernel_length + 1)
    (num_lines - kernel_length + 1)
    ~f:scanner

let xmas_match ?(verbose = false) word_search =
  let horiz_count, horiz_matches = scan_horiz ~query:"XMAS" word_search in
  let vert_count, vert_matches = scan_vert ~query:"XMAS" word_search in
  let diag_up_count, up_diag_matches = scan_up_diag ~query:"XMAS" word_search in
  let diag_down_count, down_diag_matches =
    scan_down_diag ~query:"XMAS" word_search
  in
  if verbose then (
    let print_matches matches =
      List.iter ~f:(fun (a, b) -> printf " (%d, %d)" a b) (List.rev matches)
    in
    printf "horiz match start:" ;
    print_matches horiz_matches ;
    printf "\nvert match start:" ;
    print_matches vert_matches ;
    printf "\nup diag match start:" ;
    print_matches up_diag_matches ;
    printf "\ndown diag match start:" ;
    print_matches down_diag_matches ;
    print_endline "" ) ;
  printf "horiz matches: %d\n" horiz_count ;
  printf "vert matches: %d\n" vert_count ;
  printf "up diag matches: %d\n" diag_up_count ;
  printf "down diag matches: %d\n" diag_down_count ;
  printf "\ntotal count %d\n"
    (horiz_count + vert_count + diag_up_count + diag_down_count)

let cross_mas_match ?(verbose = false) word_search =
  let count, matches = scan_cross ~query:"MAS" word_search in
  if verbose then (
    let print_matches matches =
      List.iter ~f:(fun (a, b) -> printf " (%d, %d)" a b) (List.rev matches)
    in
    printf "cross match start:" ;
    print_matches matches ) ;
  printf "cross matches: %d\n" count

let command =
  Command.basic ~summary:"Word Searcher (AOC-2024-4)"
    [%map_open.Command
      let mode = anon ("MODE" %: string)
      and file = anon ("FILE" %: string)
      and verbose = flag "verbose" no_arg ~doc:"Enable verbose output." in
      fun () ->
        let word_search = load_word_search file in
        if verbose then (
          printf "word search:\n" ;
          printf "%s" (to_string word_search) ;
          printf "\tshape: (%d, %d)\n"
            (Tuple2.get1 word_search.shape)
            (Tuple2.get2 word_search.shape) ) ;
        match String.lowercase__stack mode with
        | "xmas" ->
            xmas_match ~verbose word_search
        | "x-mas" ->
            cross_mas_match ~verbose word_search
        | _ ->
            failwith ("uknown mode " ^ mode)]

let () = Command_unix.run command

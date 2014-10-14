open TomlType

exception Bad_key of string

let maybe_escape_char ch =
    match ch with
    | '"'   -> "\\\""
    | '\\'  -> "\\\\"
    | '\n'  -> "\\n"
    | '\t'  -> "\\t"
    | _     ->
        let code = Char.code ch in
        match code with
            | _ when code <= 31 -> Printf.sprintf "\\u%04x" code
            | _                 -> String.make 1 ch

let sanitize_string value = 
    let buffer = Buffer.create (String.length value) in
    let concat_function ch = 
        maybe_escape_char ch |> Buffer.add_string buffer
    in
    String.iter concat_function value;
    Buffer.contents buffer

let format_string value = Printf.sprintf "\"%s\"" (sanitize_string value)

let format_date d = 
    let open UnixLabels
    in
    Printf.sprintf "%4d-%02d-%02dT%02d:%02d:%02dZ"
        (1900 + d.tm_year) (d.tm_mon + 1) d.tm_mday
        d.tm_hour d.tm_min d.tm_sec

let assert_key_valid key =
    String.iter (fun ch ->
        match ch with
        | ' '   ->
          raise (Bad_key (Printf.sprintf "Space forbidden in key '%s'" key))
        | '#'   ->
          raise (Bad_key (Printf.sprintf "Character '#' forbidden in key '%s'" key))
        | '.'   ->
          raise (Bad_key (Printf.sprintf "Character '.' forbidden in key '%s'" key))
        | '['   ->
          raise (Bad_key (Printf.sprintf "Character '[' forbidden in key '%s'" key))
        | ']'   ->
          raise (Bad_key (Printf.sprintf "Character ']' forbidden in key '%s'" key))
        | _     -> ()
    ) key

let rec serialize_table toml_table sections =
    (*
     * We need to serialize non-table values first, otherwise we risk including
     * top-level values in a section by accident
     *)
    let (non_table_key_values, table_key_values) = Hashtbl.fold (
      fun key value (non_table_key_values, table_key_values) ->
        match value with
        | TTable _  -> (non_table_key_values, [(key, value)] @ table_key_values)
        | _         -> ([(key, value)] @ non_table_key_values, table_key_values))
      toml_table ([], [])
    in
    let format_key_values key_values =
      List.fold_left (fun acc (key, value) ->
        acc ^ (serialize_value key value sections)) "" key_values
    in
    (format_key_values non_table_key_values)
    ^
    (format_key_values table_key_values)
and serialize_value key toml_value sections =
    assert_key_valid key;
    match toml_value with
    | TBool value   -> Printf.sprintf "%s = %B\n" key value
    | TInt value    -> Printf.sprintf "%s = %d\n" key value
    | TFloat value  -> Printf.sprintf "%s = %f\n" key value
    | TString value -> Printf.sprintf "%s = %s\n" key (format_string value)
    | TDate value   -> Printf.sprintf "%s = %s\n" key (format_date value)
    | TArray value  ->
            Printf.sprintf "%s = %s\n" key (serialize_array value)
    | TTable value  ->
        let sections' = sections @ [key] in
        let value_as_string = serialize_table value sections' in
        let value_with_header_as_string =
          (*
           * Don't print the intermediate sections, if all values are tables,
           * print [x.y.z] as appropriate instead of [x][y][z]
           *)
          let all_values_are_table =
            Hashtbl.fold (fun _ v acc ->
              match v with
              | TTable _  -> true
              | _         -> false
            ) value true
          in
          if all_values_are_table
          then
            value_as_string
          else
            let header = Printf.sprintf "[%s]" (String.concat "." sections')
            in
            Printf.sprintf "%s\n%s" header value_as_string
        in
        value_with_header_as_string
and serialize_array toml_array =
    let format_list values ~f:format_item_func =
        List.map format_item_func values
        |> String.concat ", "
        |> (fun formatted_list -> Printf.sprintf "[%s]" formatted_list)
    in
    let array_string = match toml_array with
    | NodeBool values   -> format_list values ~f:string_of_bool
    | NodeInt values    -> format_list values ~f:string_of_int
    | NodeFloat values  -> format_list values ~f:string_of_float
    | NodeString values -> format_list values ~f:format_string
    | NodeDate values   -> format_list values ~f:format_date
    | NodeArray values  -> format_list values ~f:serialize_array
    | NodeEmpty         -> "[]"
    in array_string

let string_of_table toml_table = serialize_table toml_table []

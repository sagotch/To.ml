open OUnit
open TomlType

let print_str str = str

let assert_equal_str x y = assert_equal ~printer:print_str x y

let create_table key value =
    let table = Hashtbl.create 1 in
    Hashtbl.add table key value;
    table

let toml_table key value =
    create_table key value |> Toml.string_of_table

let test = "Writing values" >:::
  [
    "key with space" >:: (fun () ->
    assert_raises
      (Toml.Bad_key "Space forbidden in key 'space key'")
      (fun () -> toml_table "space key" (TString "whatever")));
    "key with #" >:: (fun () ->
    assert_raises
      (Toml.Bad_key "Character '#' forbidden in key 'bad#key'")
      (fun () -> toml_table "bad#key" (TString "whatever")));
    "key with [" >:: (fun () ->
    assert_raises
      (Toml.Bad_key "Character '[' forbidden in key 'bad[key'")
      (fun () -> toml_table "bad[key" (TString "whatever")));
    "key with ]" >:: (fun () ->
    assert_raises
      (Toml.Bad_key "Character ']' forbidden in key 'bad]key'")
      (fun () -> toml_table "bad]key" (TString "whatever")));

    "simple string" >:: (fun () ->
      assert_equal_str
        "simple_string = \"string value\"\n"
        (toml_table "simple_string" (TString "string value")));
    "string with control chars" >:: (fun () ->
      assert_equal_str
        "string_with_control_chars = \"str\\\\ing\\t\\n\\u0002\\\"\"\n"
        (toml_table "string_with_control_chars" (TString "str\\ing\t\n\002\"")));
    "string with accented chars" >:: (fun () ->
      assert_equal_str
        "string_with_accented_chars = \"\195\169\"\n"
        (toml_table "string_with_accented_chars" (TString "\195\169")));

    "boolean true" >:: (fun () ->
      assert_equal_str
        "bool_true = true\n"
        (toml_table "bool_true" (TBool true)));
    "boolean false" >:: (fun () ->
      assert_equal_str
        "bool_false = false\n"
        (toml_table "bool_false" (TBool false)));

    "positive int" >:: (fun () ->
      assert_equal_str
        "positive_int = 42\n"
        (toml_table "positive_int" (TInt 42)));
    "negative int" >:: (fun () ->
      assert_equal_str
        "negative_int = -42\n"
        (toml_table "negative_int" (TInt (-42))));

    "positive float" >:: (fun () ->
      assert_equal_str
        "positive_float = 42.240000\n"
        (toml_table "positive_float" (TFloat 42.24)));
    "negative float" >:: (fun () ->
      assert_equal_str
        "negative_float = -42.240000\n"
        (toml_table "negative_float" (TFloat (-42.24))));

    "date" >:: (fun () ->
      let open UnixLabels
      in
      assert_equal_str
        "date = 1979-05-27T07:32:00Z\n"
        (toml_table "date" (TDate (gmtime 296638320.))));

    "table" >:: (fun () ->
      assert_equal_str
        ((String.concat "\n" [
          "[dog]";
          "type = \"golden retriever\""])^"\n")
      (toml_table "dog" (
        TTable (create_table "type" (TString "golden retriever")))));

    "nested tables" >:: (fun () ->
      assert_equal_str
        ((String.concat "\n" [
          "[dog.tater]";
          "type = \"pug\""])^"\n")
      (toml_table "dog" (
        TTable (create_table "tater" (
          TTable (create_table "type" (TString "pug")))))) );

    "empty array" >:: (fun () ->
      assert_equal_str
        "empty_array = []\n"
        (toml_table "empty_array" (TArray NodeEmpty)));
    "empty bool array" >:: (fun () ->
      assert_equal_str
        "empty_bool_array = []\n"
        (toml_table "empty_bool_array" (TArray (NodeBool []))));
    "bool array" >:: (fun () ->
      assert_equal_str
        "bool_array = [true, false]\n"
        (toml_table "bool_array" (TArray (NodeBool [true; false]))));
    "empty int array" >:: (fun () ->
      assert_equal_str
        "empty_int_array = []\n"
        (toml_table "empty_int_array" (TArray (NodeInt []))));
    "int array" >:: (fun () ->
      assert_equal_str
        "int_array = [4, 5]\n"
        (toml_table "int_array" (TArray (NodeInt [4; 5]))));
    "empty float array" >:: (fun () ->
      assert_equal_str
        "empty_float_array = []\n"
        (toml_table "empty_float_array" (TArray (NodeFloat []))));
    "float array" >:: (fun () ->
      assert_equal_str
        "float_array = [4.2, 3.14]\n"
        (toml_table "float_array" (TArray (NodeFloat [4.2; 3.14]))));
    "empty string array" >:: (fun () ->
      assert_equal_str
        "empty_string_array = []\n"
        (toml_table "empty_string_array" (TArray (NodeString []))));
    "string array" >:: (fun () ->
      assert_equal_str
        "string_array = [\"a\", \"b\"]\n"
        (toml_table "string_array" (TArray (NodeString ["a";"b"]))));
    "empty date array" >:: (fun () ->
      assert_equal_str
        "empty_date_array = []\n"
        (toml_table "empty_date_array" (TArray (NodeDate []))));
    "date array" >:: (fun () ->
      let open UnixLabels
      in
      assert_equal_str
        "date_array = [1979-05-27T07:32:00Z, 1979-05-27T08:38:40Z]\n"
        (toml_table "date_array" (TArray (NodeDate [
          (gmtime 296638320.);(gmtime 296642320.)]))));
    "empty array of arrays" >:: (fun () ->
      assert_equal_str
        "empty_array_of_arrays = []\n"
        (toml_table "empty_array_of_arrays" (TArray (NodeArray []))));
    "array of arrays" >:: (fun () ->
      assert_equal_str
        "array_of_arrays = [[2341, 2242], [[true]]]\n"
        (toml_table "array_of_arrays" (TArray (NodeArray [
          (NodeInt [2341;2242]);
          (NodeArray [NodeBool [true]])]))));

    "mixed example" >:: (fun () ->
      let level3_table = Hashtbl.create 2 in
      Hashtbl.add level3_table "is_deep" (TBool true);
      Hashtbl.add level3_table "location" (TString "basement");

      let level2_table = create_table "level3" (TTable level3_table) in

      let level1_table = create_table "level2" (TTable level2_table) in

      let top_level_table = Hashtbl.create 2 in
      Hashtbl.add top_level_table "toplevel" (TString "ocaml");
      Hashtbl.add top_level_table "level1" (TTable level1_table);

      assert_equal_str
        ((String.concat "\n" [
          "toplevel = \"ocaml\"";
          "[level1.level2.level3]";
          "is_deep = true";
          "location = \"basement\""
        ])^"\n")
        (top_level_table |> Toml.string_of_table));

  ]

let _ = OUnit.run_test_tt_main test

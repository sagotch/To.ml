open TomlType

exception Bad_key of string

val string_of_table : tomlTable -> string

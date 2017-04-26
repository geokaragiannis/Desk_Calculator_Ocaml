#use "bigint.ml";;
open Bigint;;
let foo = bigint_of_string "42";;
let bar = bigint_of_string "_10";;
string_of_bigint (rem foo bar);;

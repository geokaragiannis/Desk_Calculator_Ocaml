(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

   let canon lst = 
      let rec canon' lst' = match lst' with
         | []  -> []
         | [0] -> []
         | car::cdr ->
            let cdr' = canon' cdr
            in match car, cdr' with
               | 0, [] -> []
               | car, cdr' -> car::cdr'
      in canon' lst

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []


    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, canon (to_intlist 1))
                     else Bigint (Pos, canon (to_intlist 0))

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

   let rec compare_helper rev1 rev2 =  
      match (rev1, rev2) with
      | [], [] -> 0
      | car1::cdr1, car2::cdr2 -> if car1 > car2 then 1
                                  else if car2 > car1 then -1
                                  else compare_helper cdr1 cdr2
   
   (*
    * compare_big compares value1 and value2:
    * if value1 > value2, return 1
    * if value1 < value2, return -1
    * if value1 = value2, return 0
    ********* right now it takes two bigints as arguments, this is 
    *******just for testing. IN the future, it should take value1,value2
   *)
   let compare_big (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
      let len1 = List.length value1 in
         let len2 = List.length value2 in
            if len1 > len2 then 1
            else if len1 < len2 then -1
            else let rev1 = reverse value1 in
                 let rev2 = reverse value2 in
                   compare_helper rev1 rev2

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else zero

    let sub = add

    let mul = add

    let div = add

    let rem = add

    let pow = add

end


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


   (*
    * canon canonicalizes the value of a bigint by removing the
    * the higher order zeros
    *)
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

   (*
    * compares the magnitude of two numbers which are 
    * passed with the head as the highest order bit
    *** rev1 and rev2 must be of the same length***
    *)
   let rec compare_helper rev1 rev2 =  
      match (rev1, rev2) with
      | [], [] -> 0
      | [], _  -> raise (Invalid_argument "compare helper")
      | _, []  -> raise (Invalid_argument "compare helper")
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
   let compare_big value1 value2 =
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

   (*
    * value1 must be greater than value2
    *)
   let rec sub' list1 list2 borrow = match (list1, list2, borrow) with
      | list1, [], 0 -> list1
      | [], list2, 0 -> raise (Invalid_argument "sub'")
      | list1, [], borrow -> sub' list1 [borrow] 0
      | [], list2, borrow -> raise (Invalid_argument "sub'")
      | car1::cdr1, car2::cdr2, borrow ->
        let diff = car1 - car2 - borrow
        in if diff >= 0 then diff :: sub' cdr1 cdr2 0
           else (diff + 10) :: sub' cdr1 cdr2 1

    let double number = add' number number 0

    let rec mul' (multiplier, powerof2, multiplicand') = 
        if (compare_big  powerof2 multiplier) = 1 then multiplier, [0]
        else let remainder, product = 
            mul' (multiplier, (double powerof2), (double multiplicand'))
            in if (compare_big remainder powerof2) = -1
               then remainder, product
               else  (canon (sub' remainder powerof2 0)),
                           (add' product multiplicand' 0)

    let rec divrem' (dividend, powerof2, divisor') =
        if (compare_big divisor' dividend) = 1 
            then [0], dividend
        else let quotient, remainder =
                 divrem' (dividend, (double powerof2), 
                          (double divisor'))
             in if (compare_big remainder divisor') = -1
                    then quotient, remainder
                else (add' quotient powerof2 0), 
                           (canon (sub' remainder divisor' 0))

    let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

    let even number = if number = [] 
                      then true else (car number) mod 2 = 0

    let rec power' (base, expt, result) = match expt with
        | []                   -> result
        | expt when even expt  -> power' (snd (mul' (base, [1], base)),
                                       fst (divrem (expt, [2])), result)
        | expt                 -> power' (base, canon (sub' expt [1] 0),
                                       snd (mul' (base, [1], result)))

    let add (Bigint(neg1, value1)) (Bigint(neg2, value2)) =
        if neg1 = neg2
        then Bigint(neg1, add' value1 value2 0)
        else
            let result = compare_big value1 value2 in
               if result = 0 then zero
               (*change later *)
               else if result = 1 
                    then Bigint(neg1, canon (sub' value1 value2 0))
               else Bigint(neg2, canon (sub' value2 value1 0))
            
    let sub (Bigint(neg1, value1)) (Bigint(neg2, value2)) =
      if neg1 <> neg2 then
         if neg2 = Neg then Bigint(Pos, add' value1 value2 0)
         else Bigint(Neg, add' value1 value2 0)
      else
         let result = compare_big value1 value2 in
         if neg1 = Pos then
            if result = 1 then Bigint(Pos, canon(sub' value1 value2 0))
            else if result = -1 then 
               Bigint(Neg, canon(sub' value2 value1 0))
            else zero
         else
            if result = 1 then Bigint(Neg, canon(sub' value1 value2 0))
            else if result = -1 then
               Bigint(Pos, canon(sub' value2 value1 0))
            else zero          

    let mul (Bigint (neg1,value1)) (Bigint(neg2, value2)) = 
        let _, product = mul' (value1, [1], value2)
        in if neg1 == neg2 then Bigint(Pos, product)
           else Bigint(Neg, product)

    let div (Bigint (neg1,value1)) (Bigint(neg2, value2)) = 
        let quotient,_ = divrem (value1, value2)
        in if neg1 == neg2 then Bigint(Pos, quotient)
           else Bigint(Neg, quotient)


    let rem (Bigint (neg1,value1)) (Bigint(neg2, value2)) =
        let _, remainder = divrem (value1, value2)
        in match (neg1, neg2) with
            |Pos, Pos -> Bigint(Pos, remainder)
            |Neg, Neg -> Bigint(Neg, remainder)
            |Neg, Pos -> Bigint(Neg, remainder)
            |Pos, Neg -> Bigint(Pos, remainder)
         

    let pow (Bigint (neg1,value1)) (Bigint(neg2, value2)) =
        if neg2 = Neg then zero
        else let result = power' (value1, value2, [1]) in
            if neg1 = Pos then Bigint(Pos, result)
            else 
                if even value2 then Bigint(Pos, result)
                else Bigint(Neg, result)

end


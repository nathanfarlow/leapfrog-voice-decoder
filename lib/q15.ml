(** Round halves toward positive infinity. *)
let round num den =
  let n = (2 * num) + den in
  let d = 2 * den in
  if n < 0 then (n - d + 1) / d else n / d
;;

let mul a b = round (a * b) 32768
let clamp x = max (-32768) (min 32767 x)
let div num den = round (num * 32768) den

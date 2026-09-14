open! Core

type t =
  { state : int array
  ; mutable f : int
  }

let next ({ state; f } as t) =
  let i = f in
  state.(i) <- (state.(i) + state.((i + 28) mod 31)) land 0xFFFFFFFF;
  t.f <- (i + 1) mod 31;
  state.(i) lsr 1
;;

let create () =
  let state =
    Array.folding_map (Array.create ~len:31 ()) ~init:1 ~f:(fun x () ->
      ((1103515245 * x) + 12345) land 0xFFFFFFFF, x)
  in
  let t = { state; f = 3 } in
  Fn.apply_n_times ~n:310 (fun () -> ignore (next t : int)) ();
  t
;;

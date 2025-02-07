open Codelib;;
open Codecps;;
open Codecps.Let_syntax;;

type 'a t = { rand_gen : size_c:(int code) -> random_c:(Splittable_random.State.t code) -> 'a code Codecps.t }

module C = struct
  type 'a t = 'a code
  let lift x = .< x >.
  let pair x y = .< (.~x,.~y) >.
  let i2f x = .< Float.of_int .~x >.
  (*
  let pred n = .< .~n - 1 >.
  let cons x xs = .< .~x :: .~xs >.
  *)
end

type 'a c = 'a C.t
(* type 'a recgen = (unit -> 'a Core.Quickcheck.Generator.t) code *)

let return x = { rand_gen = fun ~size_c:_ ~random_c:_ -> Codecps.return x }

let bind (r : 'a t) ~(f : 'a code -> 'b t) = { rand_gen = fun ~size_c ~random_c ->
  (* need to figure out how to letinsert here! *)
  let%bind a = r.rand_gen ~size_c ~random_c in
  let%bind a = Codecps.let_insert a in
  (f (v2c a)).rand_gen ~size_c ~random_c
}

let bool : bool t = {
  rand_gen =
    fun ~size_c:_ ~random_c ->
      Codecps.return .< Splittable_random.bool .~random_c >.
}

let int ~(lo : int code) ~(hi : int code) : int t = {
  rand_gen =
    fun ~size_c:_ ~random_c ->
      Codecps.return .< Splittable_random.int ~lo:.~lo ~hi:.~hi .~random_c >.
}

let float ~(lo : float code) ~(hi : float code) : float t = {
  rand_gen =
    fun ~size_c:_ ~random_c ->
      Codecps.return .< Splittable_random.float ~lo:.~lo ~hi:.~hi .~random_c >.
}

let rec genpick n ws =
  match ws with
  | [] -> { rand_gen = fun ~size_c:_ ~random_c:_ -> Codecps.return .< failwith "Fell of the end of pick list" >. }
  | (k,g) :: ws' ->
        { rand_gen = 
          fun ~size_c ~random_c ->
          let%bind leq = Codecps.split_bool .< Float.compare .~(v2c n) .~(v2c k) <= 0 >. in
          if leq then
            g.rand_gen ~size_c ~random_c
          else
            let%bind n' = Codecps.let_insert .< .~(v2c n) -. .~(v2c k) >. in
            (genpick n' ws').rand_gen ~size_c ~random_c
        }

  
let sum (ws : (float val_code * 'a t) list) : (float val_code) Codecps.t =
    let rec go (ws : (float val_code * 'a t) list) (acc : float val_code) : (float val_code) Codecps.t =
      match ws with
      | [] -> Codecps.return acc
      | (cn,_) :: ws' ->
          let%bind acc' = Codecps.let_insert .< .~(v2c acc) +. .~(v2c cn) >. in
          go ws' acc'
      in
    let%bind zero = let_insert .<0.>. in
    go ws zero

let choose (ws : (float code * 'a t) list) : 'a t =
  { rand_gen = fun ~size_c ~random_c ->
      let%bind ws' = Codecps.all @@ List.map (fun (cn,g) -> let%bind cvn = Codecps.let_insert cn in Codecps.return (cvn,g)) ws in
      let%bind sum = sum ws' in
      let%bind n = (float ~lo:.<0.>. ~hi:(v2c sum)).rand_gen ~size_c ~random_c in
      let%bind n = Codecps.let_insert n in
      (genpick n ws').rand_gen ~size_c ~random_c
  }

let with_size f ~size_c =
  { rand_gen = fun ~size_c:_ ~random_c -> f.rand_gen ~size_c:size_c ~random_c }

let size = { rand_gen = fun ~size_c ~random_c:_ -> Codecps.return size_c }

let to_fun sg = 
  let f = sg.rand_gen in
  .< fun ~size ~random ->
      .~(Codecps.code_generate (f ~size_c:.< size >. ~random_c:.< random >.))
  >.

let to_qc sg =
  .<
    Base_quickcheck.Generator.create .~(to_fun sg)
  >.


let print sg = Codelib.print_code Format.std_formatter (to_fun sg)

let run_ocamlfind_query package =
  let cmd = Printf.sprintf "ocamlfind query %s" package in
  let ic = Unix.open_process_in cmd in
  match In_channel.input_line ic with
  | Some path -> 
      ignore (Unix.close_process_in ic); 
      Runnative.add_search_path path
  | None -> 
      ignore (Unix.close_process_in ic); 
      failwith ("Could not find " ^ package)

let () =
  List.iter run_ocamlfind_query
    [ "base_quickcheck"; "core"; "splittable_random"; "base" ]


(* LMFAO I CANNOT BELIEVE THIS WORKS *)
let jit cde = Runnative.run_native (Codelib.close_code (to_qc cde))

type ('a,'r) recgen = 'r code -> 'a t
let recurse f x = f x

let recursive (type a) (type r) (x0 : r code) (step : (a,r) recgen -> r code -> a t) =
  {
    rand_gen = fun ~size_c ~random_c -> 
      let%bind x0 = Codecps.let_insert x0 in
      Codecps.return @@ (.< let rec go x ~size ~random = .~(
          Codecps.code_generate @@
            (step
                (fun xc' -> { rand_gen = fun ~size_c ~random_c -> Codecps.return .< go .~xc' ~size:.~size_c ~random:.~random_c >. })
                .<x>.
            ).rand_gen ~size_c:.<size>. ~random_c:.<random>.
        )
        in
          go .~(v2c x0) ~size:.~size_c ~random:.~random_c
      >.)
  }

(*
open Base;;
open Codelib;;
open Codecps;;
open Codecps.Let_syntax;;
open Stdlib
type 'a t = { rand_gen : size_c:(int code) -> random_c:(Splittable_random.State.t code) -> 'a code Codecps.t }

module C = struct
  type 'a t = 'a code
  let lift x = .< x >.
  let pair x y = .< (.~x,.~y) >.
  let i2f x = .< Float.of_int .~x >.
end

type 'a c = 'a C.t
(* type 'a recgen = (unit -> 'a Core.Quickcheck.Generator.t) code *)

let return x = { rand_gen = fun ~size_c:_ ~random_c:_ -> Codecps.return x }

let bind (r : 'a t) ~(f : 'a code -> 'b t) = { rand_gen = fun ~size_c ~random_c ->
  (* need to figure out how to letinsert here! *)
  let%bind a = r.rand_gen ~size_c ~random_c in
  let%bind a = Codecps.let_insert a in
  (f (v2c a)).rand_gen ~size_c ~random_c
}

let bool : bool t = {
  rand_gen =
    fun ~size_c:_ ~random_c ->
      Codecps.return .< Splittable_random.bool .~random_c >.
}

let int ~(lo : int code) ~(hi : int code) : int t = {
  rand_gen =
    fun ~size_c:_ ~random_c ->
      Codecps.return .< Splittable_random.int ~lo:.~lo ~hi:.~hi .~random_c >.
}

let float ~(lo : float code) ~(hi : float code) : float t = {
  rand_gen =
    fun ~size_c:_ ~random_c ->
      Codecps.return .< Splittable_random.float ~lo:.~lo ~hi:.~hi .~random_c >.
}

let rec genpick n ws =
  match ws with
  | [] -> { rand_gen = fun ~size_c:_ ~random_c:_ -> Codecps.return .< failwith "Fell of the end of pick list" >. }
  | (k,g) :: ws' ->
        { rand_gen = 
          fun ~size_c ~random_c ->
          let%bind leq = Codecps.split_bool .< Float.compare .~(v2c n) .~(v2c k) <= 0 >. in
          if leq then
            g.rand_gen ~size_c ~random_c
          else
            let%bind n' = Codecps.let_insert .< .~(v2c n) -. .~(v2c k) >. in
            (genpick n' ws').rand_gen ~size_c ~random_c
        }

  
let sum (ws : (float val_code * 'a t) list) : (float val_code) Codecps.t =
    let rec go (ws : (float val_code * 'a t) list) (acc : float val_code) : (float val_code) Codecps.t =
      match ws with
      | [] -> Codecps.return acc
      | (cn,_g) :: ws' ->
          let%bind acc' = Codecps.let_insert .< .~(v2c acc) +. .~(v2c cn) >. in
          go ws' acc'
      in
    let%bind zero = let_insert .<0.>. in
    go ws zero

let choose (ws : (float code * 'a t) list) : 'a t =
  { rand_gen = fun ~size_c ~random_c ->
      let%bind ws' = Codecps.all @@ List.map (fun (cn,g) -> let%bind cvn = Codecps.let_insert cn in Codecps.return (cvn,g)) ws in
      let%bind sum = sum ws' in
      let%bind n = (float ~lo:.<0.>. ~hi:(v2c sum)).rand_gen ~size_c ~random_c in
      let%bind n = Codecps.let_insert n in
      (genpick n ws').rand_gen ~size_c ~random_c
  }

let with_size f ~size_c =
  { rand_gen = fun ~size_c:_ ~random_c -> f.rand_gen ~size_c:size_c ~random_c }

let size = { rand_gen = fun ~size_c ~random_c:_ -> Codecps.return size_c }

let to_fun sg = 
  let f = sg.rand_gen in
  .< fun ~size ~random ->
      .~(Codecps.code_generate (f ~size_c:.< size >. ~random_c:.< random >.))
  >.

let to_qc sg =
  .<
    Base_quickcheck.Generator.create .~(to_fun sg)
  >.


let print sg = Codelib.print_code Format.std_formatter (to_fun sg)

let run_ocamlfind_query package =
  let cmd = Printf.sprintf "ocamlfind query %s" package in
  let ic = Unix.open_process_in cmd in
  match In_channel.input_line ic with
  | Some path -> 
      ignore (Unix.close_process_in ic); 
      Runnative.add_search_path path
  | None -> 
      ignore (Unix.close_process_in ic); 
      failwith ("Could not find " ^ package)

let () =
  List.iter run_ocamlfind_query
    [ "base_quickcheck"; "core"; "splittable_random"; "base" ]

(* LMFAO I CANNOT BELIEVE THIS WORKS *)
let jit cde = Runnative.run_native (Codelib.close_code (to_qc cde))

type ('a,'r) recgen = 'r code -> 'a t
let recurse f x = f x

let recursive (type a) (type r) (x0 : r code) (step : (a,r) recgen -> r code -> a t) =
  let rec go x =
    step go x 
  in
  {
    rand_gen = fun ~size_c ~random_c -> (go x0).rand_gen ~size_c ~random_c
  }
*)
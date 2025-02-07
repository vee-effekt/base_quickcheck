open Codelib;;

(* This is stolen from andras kovacs *)
type 'a t = {code_gen : 'z. (('a -> 'z code) -> 'z code)}

let v2c (vc : 'a val_code) = (vc : 'a val_code :> 'a code)

let run_code_gen {code_gen=f} k = f k

let code_generate : ('a code) t -> 'a code =
  fun g -> g.code_gen (fun x -> x)

let return x = { code_gen = fun k -> k x}
let bind ({code_gen = inC } : 'a t) ~(f : 'a -> 'b t) : 'b t = { code_gen = fun k -> inC (fun a -> run_code_gen (f a) k) }

module For_monad = Base.Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let bind = bind
    let map = `Define_using_bind
  end)

module Monad_infix = For_monad.Monad_infix
include Monad_infix
module Let_syntax = For_monad.Let_syntax

let all = For_monad.all

let let_insert (cx : 'a code) : 'a val_code t = {
  code_gen = fun k -> letlv cx k
}

let split_bool (cb : bool code) : bool t = {
  code_gen = fun k ->
    Codelib.letl cb (fun bvc ->
    .<
      if .~bvc then .~(k true) else .~(k false)
    >.
    )
}

let split_pair (cp : ('a * 'b) code) : ('a code *'b code) t = {
  code_gen = fun k ->
    .<
      let (a,b) = .~(Codelib.genlet cp) in .~(k (.<a>.,.<b>.))
    >.
}
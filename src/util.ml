
open Batteries

let pad_left n c s =
  String.make (max 0 (n - String.length s)) c ^ s

let pad_right n c s =
  s ^ String.make (max 0 (n - String.length s)) c

module type Monad = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

end

module MonadOps(M : Monad) = struct
  type 'a t = 'a M.t

  let fold f acc data =
    let f' ma b = M.bind ma (fun a -> f a b) in
    Enum.fold f' (M.return acc) data

end

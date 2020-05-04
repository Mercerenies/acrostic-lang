
open Batteries

val pad_left : int -> char -> string -> string

val pad_right : int -> char -> string -> string

module type Monad = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

end

module MonadOps(M : Monad) : sig
  type 'a t = 'a M.t

  val fold : ('a -> 'b -> 'a t) -> 'a -> 'b Enum.t -> 'a t

end

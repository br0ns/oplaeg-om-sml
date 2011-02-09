signature PERSON =
sig
  type t

  val ny : {navn : string, cpr : int} -> t
  val navn : t -> string
  val cpr : t -> int

  val sammenlign : t * t -> order
end

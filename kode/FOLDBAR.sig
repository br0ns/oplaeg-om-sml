signature FOLDBAR = sig
  type 'a t
  val fold : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
end

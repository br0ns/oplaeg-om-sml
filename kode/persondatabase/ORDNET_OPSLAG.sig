signature ORDNET_OPSLAG =
sig
  type noegle
  type 'a t

  val tom : 'a t
  val indsaet : 'a t -> noegle * 'a -> 'a t
  val find : 'a t -> noegle -> 'a option
  val fjern : 'a t -> noegle -> 'a t

  (* Kan hejse {Empty} *)
  val tagMindste : 'a t -> (noegle * 'a) * 'a t
  val tagStoerste : 'a t -> (noegle * 'a) * 'a t
end

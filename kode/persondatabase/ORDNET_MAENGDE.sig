signature ORDNET_MAENGDE =
sig
  type element
  type t

  val tom : t
  val indsaet : t -> element -> t
  val indeholder : t -> element -> bool
  val faelles : t -> t -> t
  val fjern : t -> element -> t
  val erTom : t -> bool

  (* Refleksiv variant: {delmaengde a b} betyder a \subseteq b *)
  val delmaengde : t -> t -> bool

  (* Kan hejse {Empty} *)
  val tagMindste : t -> element * t
  val tagStoerste : t -> element * t
end

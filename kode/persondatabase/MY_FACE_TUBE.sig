signature MY_FACE_TUBE =
sig
  val tilmeld : Person.t -> unit
  val afmeld : Person.t -> unit
  val medlem : Person.t -> bool
  val medlemmer : unit -> PersonMaengde.t

  (* Hvis ikke medlem returneres den tomme maengde *)
  val venner : Person.t -> PersonMaengde.t

  (* {nyVen a b} tilfoejer b til a's venner *)
  val nyVen : Person.t -> Person.t -> unit
end

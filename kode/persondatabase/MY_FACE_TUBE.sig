signature MY_FACE_TUBE =
sig
  type person = Person.t

  val tilmeld : person -> unit
  val afmeld : person -> unit
  val medlem : person -> bool
  val medlemmer : unit -> PersonMaengde.t

  (* Hvis ikke medlem returneres den tomme maengde *)
  val venner : person -> PersonMaengde.t

  (* {nyVen a b} tilfoejer b til a's venner *)
  val nyVen : person -> person -> unit
end

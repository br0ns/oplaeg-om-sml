signature ORDNING =
sig
  type t

  val sammenlign : t * t -> order
end
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
functor OrdnetMaengdeFn (Element : ORDNING) :>
        ORDNET_MAENGDE where type element = Element.t =
struct
type element = Element.t
datatype t = E
           | T of t * element * t

val tom = E

fun indsaet E x = T (E, x, E)
  | indsaet (T (l, y, r)) x =
    case Element.sammenlign (x, y) of
      LESS    => T (indsaet l x, y, r)
    | EQUAL   => T (l, y, r)
    | GREATER => T (l, y, indsaet r x)

fun indeholder E _ = false
  | indeholder (T (l, y, r)) x =
    case Element.sammenlign (x, y) of
      LESS    => indeholder l x
    | EQUAL   => true
    | GREATER => indeholder r x

fun faelles m E = m
  | faelles m (T (l, x, r)) =
    indsaet (faelles (faelles m l) r) x

fun delmaengde E _ = true
  | delmaengde (T (l, x, r)) m =
    indeholder m x andalso
    delmaengde l m andalso
    delmaengde r m

fun tagStoerste E = raise Empty
  | tagStoerste (T (l, x, E)) =
    (x, l)
  | tagStoerste (T (l, x, r)) =
    let
      val (y, r') = tagStoerste r
    in
      (y, T (l, x, r'))
    end

fun tagMindste E = raise Empty
  | tagMindste (T (E, x, r)) =
    (x, r)
  | tagMindste (T (l, x, r)) =
    let
      val (y, l') = tagMindste l
    in
      (y, T (l', x, r))
    end

fun fjern E _ = E
  | fjern (T (l, y, r)) x =
    case Element.sammenlign (x, y) of
      LESS    => T (fjern l x, y, r)
    | GREATER => T (l, y, fjern r x)
    | EQUAL   =>
      let
        val (y', l') = tagStoerste l
      in
        T (l', y', r)
      end handle Empty => r

fun erTom E = true
  | erTom _ = false
end
functor OrdnetOpslagFn (Noegle : ORDNING) :>
        ORDNET_OPSLAG where type noegle = Noegle.t =
struct
type noegle = Noegle.t
datatype 'a t = E
              | T of 'a t * (noegle * 'a) * 'a t

val tom = E

fun indsaet E x = T (E, x, E)
  | indsaet (T (l, (y, w), r)) (x, v) =
    case Noegle.sammenlign (x, y) of
      LESS    => T (indsaet l (x, v), (y, w), r)
    | EQUAL   => T (l, (x, v), r)
    | GREATER => T (l, (y, w), indsaet r (x, v))

fun find E _ = NONE
  | find (T (l, (y, w), r)) x =
    case Noegle.sammenlign (x, y) of
      LESS    => find l x
    | EQUAL   => SOME w
    | GREATER => find r x

fun tagStoerste E = raise Empty
  | tagStoerste (T (l, x, E)) =
    (x, l)
  | tagStoerste (T (l, x, r)) =
    let
      val (y, r') = tagStoerste r
    in
      (y, T (l, x, r'))
    end

fun tagMindste E = raise Empty
  | tagMindste (T (E, x, r)) =
    (x, r)
  | tagMindste (T (l, x, r)) =
    let
      val (y, l') = tagMindste l
    in
      (y, T (l', x, r))
    end

fun fjern E _ = E
  | fjern (T (l, (y, w), r)) x =
    case Noegle.sammenlign (x, y) of
      LESS    => T (fjern l x, (y, w), r)
    | GREATER => T (l, (y, w), fjern r x)
    | EQUAL   =>
      let
        val ((y', w'), l') = tagStoerste l
      in
        T (l', (y', w'), r)
      end handle Empty => r
end
signature PERSON =
sig
  type t

  val ny : {navn : string, cpr : int} -> t
  val navn : t -> string
  val cpr : t -> int

  val sammenlign : t * t -> order
end
structure Person :> PERSON =
struct
type t = string * int

(* TODO: Indfoer integritetstjek af CPR-nummer *)
fun ny {navn, cpr} = (navn, cpr)

fun navn (navn, _) = navn
fun cpr (_, cpr) = cpr

fun sammenlign ((_, x), (_, y)) = Int.compare (x, y)
end
structure PersonMaengde = OrdnetMaengdeFn (Person)
structure PersonOpslag = OrdnetOpslagFn (Person)
signature MY_FACE_TUBE =
sig
  type person = Person.t
  val tilmeld : person -> unit
  val afmeld : person -> unit
  val medlem : person -> bool
  val medlemmer : unit -> PersonMaengde.t

  (* Hvis ikke medlem returneres den tomme maende *)
  val venner : person -> PersonMaengde.t

  (* {nyVen a b} tilfoejer b til a's venner
   *)
  val nyVen : person -> person -> unit
end
structure MyFaceTube :> MY_FACE_TUBE =
struct
type person = Person.t

structure M = PersonMaengde
structure O = PersonOpslag

val opslag = ref O.tom

fun tilmeld p =
    opslag := O.indsaet (!opslag) (p, M.tom)

fun afmeld p =
    opslag := O.fjern (!opslag) p

fun medlem p =
    case O.find (!opslag) p of
      SOME _ => true
    | NONE   => false

fun medlemmer _ =
    let
      fun loop opslag personer =
          let
            val ((p, _), opslag') = O.tagMindste opslag
          in
            loop opslag' (M.indsaet personer p)
          end handle Empty => personer
    in
      loop (!opslag) M.tom
    end

fun venner p =
    case O.find (!opslag) p of
      SOME v => v
    | NONE   => M.tom

fun nyVen a b =
    let
      val v = M.indsaet (venner a) b
    in
      opslag := O.indsaet (!opslag) (a, v)
    end
end

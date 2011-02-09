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

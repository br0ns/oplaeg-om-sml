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

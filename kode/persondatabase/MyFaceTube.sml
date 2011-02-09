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

functor FlagFn () :> FLAG =
struct
structure M = TekstMaengde
val flag = ref M.tom

fun hejs f =
    flag := M.indsaet (!flag) f

fun saenk f =
    flag := M.fjern (!flag) f

fun hejst f =
    M.indeholder (!flag) f
end

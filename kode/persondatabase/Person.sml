structure Person :> PERSON =
struct
(* Navn * CPR-nummer *)
type t = string * int

(* TODO: Indfoer integritetstjek paa CPR-nummer *)
fun ny {navn, cpr} = (navn, cpr)

fun navn (navn, _) = navn
fun cpr (_, cpr) = cpr

fun sammenlign ((_, x), (_, y)) = Int.compare (x, y)
end

functor FoldbarFn (F : FOLDBAR) :> sig
  type 'a t = 'a F.t
  val tilListe : 'a t -> 'a list
  val alle : 'a t -> ('a -> bool) -> bool
  val nogle : 'a t -> ('a -> bool) -> bool
  val findes : ''a t -> ''a -> bool
  val konkat : 'a list t -> 'a list
  val anvend : 'a t -> ('a -> unit) -> unit
end =
struct
type 'a t = 'a F.t
fun tilListe f = F.fold (fn (a, b) => a :: b) nil f
fun alle f g = F.fold (fn (a, b) => g a andalso b) true f
fun nogle f g = F.fold (fn (a, b) => g a orelse b) false f
fun findes f x = F.fold (fn (a, b) => a = x orelse b) false f
fun konkat f = F.fold (fn (a, b) => a @ b) nil f
fun anvend f g = F.fold (fn (a, _) => g a) () f
end

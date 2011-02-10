;load "mft";

local
  structure M = PersonMaengde
in
fun wannabe p =
    let
      fun loop pr besoegte =
          let
            val (p, pr') = M.tagMindste pr
          in
            if M.indeholder besoegte p then
              loop pr' besoegte
            else
              loop
                (M.faelles (MyFaceTube.venner p) pr')
                (M.indsaet besoegte p)
          end handle Empty => besoegte

      val venner = loop (M.indsaet M.tom p) M.tom
      val medlemmer = MyFaceTube.medlemmer ()
    in
      M.delmaengde venner medlemmer andalso
      M.delmaengde medlemmer venner
    end
end

val morten = Person.ny {navn = "Morten", cpr = 030586}
val mikkel = Person.ny {navn = "Mikkel", cpr = 121089}
val jon    = Person.ny {navn = "Jon",    cpr = 280482}
val jesper = Person.ny {navn = "Jesper", cpr = 070886}
val fritz  = Person.ny {navn = "Fritz",  cpr = 030661}
val robin  = Person.ny {navn = "Robin",  cpr = 130134}

;MyFaceTube.tilmeld morten;
;MyFaceTube.tilmeld mikkel;
;MyFaceTube.tilmeld jon;
;MyFaceTube.tilmeld jesper;
;MyFaceTube.tilmeld fritz;
;MyFaceTube.tilmeld robin;

;MyFaceTube.nyVen morten mikkel;
;MyFaceTube.nyVen morten jon;
;MyFaceTube.nyVen morten fritz;
;MyFaceTube.nyVen jesper morten;
;MyFaceTube.nyVen fritz robin;
;MyFaceTube.nyVen fritz jon;
;MyFaceTube.nyVen fritz jesper;

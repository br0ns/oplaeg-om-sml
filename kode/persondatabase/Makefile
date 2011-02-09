all :
	cat ORDNING.sig \
      ORDNET_MAENGDE.sig \
      ORDNET_OPSLAG.sig \
      OrdnetMaengdeFn.sml \
      OrdnetOpslagFn.sml \
\
      PERSON.sig \
      Person.sml \
\
      PersonMaengde.sml \
      PersonOpslag.sml \
\
      MY_FACE_TUBE.sig \
      MyFaceTube.sml \
\
      > mft.sml

	mosmlc -c -P full -toplevel mft.sml
	rm mft.sml
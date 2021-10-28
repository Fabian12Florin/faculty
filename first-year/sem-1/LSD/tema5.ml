(* Pornind de la lista de județe din fișierul atașat și folosind funcțiile predefinite discutate
astăzi la laborator (List.map, List.filter, etc. ), scrieți următoarele funcții:
1. numar_judete: funcția primește ca parametru o listă de județe (o listă de
perechi, ca în fișier) și returnează numărul de județe din listă
2. modifica_populatie: funcția primește ca parametri numele unui județ, un
număr întreg (x) și o listă de județe (o listă de perechi, ca în fișier) și
returnează o nouă listă de județe, însă adună numărul x la populația județului
al cărui nume e dat ca parametru
3. media_populatie: funcția primește ca parametru o listă de județe (o listă de
perechi, ca în fișier) și returnează media populației tuturor județelor din listă
4. statistica: funcția primește ca parametri numele unui județ și o listă de județe
(o listă de perechi, ca în fișier) și returnează o lista cu numele tuturor județelor
ce au populația mai mare decât cea a județului al cărui nume e dat ca
parametru numărul x *)
let jud_pop = [("Alba", 342376); ("Arad", 430629); ("Arges", 612431); ("Bacau", 616168); ("Bihor", 575398); ("Bistrita-Nasaud", 286225); ("Botosani", 412626); ("Brasov", 549217); ("Braila", 321212); ("Buzau", 451069); ("Caras-Severin", 295579); ("Calarasi", 306691); ("Cluj", 691106); ("Constanta", 684082); ("Covasna", 210177); ("Dambovita", 518745); ("Dolj", 660544); ("Galati", 536167); ("Giurgiu", 281422); ("Gorj", 341594); ("Harghita", 310867); ("Hunedoara", 418565); ("Ialomita", 274148); ("Iasi", 772348); ("Ilfov", 388738); ("Maramures", 478659); ("Mehedinti", 265390); ("Mures", 550846); ("Neamt", 470766); ("Olt", 436400); ("Prahova", 762886); ("Satu Mare", 344360); ("Salaj", 224384); ("Sibiu", 397322); ("Suceava", 634810); ("Teleorman", 380123); ("Timis", 683540); ("Tulcea", 213083); ("Vaslui", 395499); ("Valcea", 371714); ("Vrancea", 340310)];;

(*1*)
let rec numar_judete lista=List.fold_left (fun nr (nume,populatie) -> nr+1) 0 lista
let r1=numar_judete jud_pop;;

(*2*)
let rec modifica_populatie nume_judet x lista=match lista with
|[]->lista
|(judet,populatie)::t ->if judet=nume_judet then (judet,x)::(modifica_populatie nume_judet x t)
                        else (judet,populatie)::(modifica_populatie nume_judet x t)
let r2=modifica_populatie "Gorj" 999999 jud_pop;;

(*3*)
let rec media_populatie lista=(List.fold_left (fun suma_init (judet,populatie) ->suma_init+populatie ) 0 lista) / (numar_judete lista)
let r3=media_populatie jud_pop;;

(*4*)
let rec statistica nume_judet_x lista=
  let rec populatia_judetului_x nume_judet_x lista = match lista with
  |[] -> failwith "Numele judetului este incorect!"
  |(nume,populatie)::t -> if(nume = nume_judet_x) then populatie else populatia_judetului_x nume_judet_x t in 
  let rec judete_peste popjudx lista = match lista with
  |[] -> []
  |(judet,populatie)::t ->if (populatie>popjudx) then judet::judete_peste popjudx t 
                          else judete_peste popjudx t in
  judete_peste (populatia_judetului_x nume_judet_x lista) lista
let r4=statistica "Timis" jud_pop;;

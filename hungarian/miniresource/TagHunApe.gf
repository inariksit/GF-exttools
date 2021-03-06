instance TagHunApe of TagHun = open Prelude, Coordination in {

 oper

	Tag : Type = Str ;
--    Word : Type = { s : Str ; tags : Tag } ;

    ListTag : Type = Str ;
    BaseTag = id Str ;
    ConsTag = glue ;

    emptyTag : Tag = [] ;


    vblex = "<vblex>" ;
    vbser = "<vbser>" ;
    det = "<det>" ;
    n = "<n>" ;
    adj = "<adj>" ;
    adv = "<adv>" ;
    prn = "<prn>" ;
    conj = "<cnjcoo>" ;

    pers : Tag = "<pers>" ;
    p1 : Tag = "<p1>" ;
    p2 : Tag = "<p2>" ;
    p3 : Tag = "<p3>" ;
    mf : Tag = "<mf>" ;
    sg : Tag = "<sg>" ;
    pl : Tag = "<pl>" ;
    acc : Tag = "<acc>" ;
    nom : Tag = "<nom>" ;
    pres : Tag = "<pri>" ;
    past : Tag = "<past>" ;
    defi : Tag = "<def>" ;
    ind : Tag = "<ind>" ;


---

    cons = overload {
      cons : Tag -> Tag -> Tag = 
        \t1,t2 -> t1 + t2 ;
      cons : Tag -> Tag -> Tag -> Tag = 
        \t1,t2,t3 -> t1 + t2 + t3 ;
      cons : Tag -> Tag -> Tag -> Tag -> Tag = 
        \t1,t2,t3,t4 -> t1 + t2 + t3 + t4;
    } ;

---

    startWord = "^" ;
    endWord = "$" ;

    tense : PTense -> Tag = \t ->
      case t of {
       PPres => pres ;
       PPerf => past  
      } ;

    num : Number -> Tag = \n ->
      case n of {
        Sg => sg ;
        Pl => pl 
      } ;

    per : Person -> Tag = \p ->
      case p of {
        P1 => p1 ;
        P2 => p2 ;
        P3 => p3 
      } ;


    agr : Agr -> Tag = \a -> 
      case a of {
        Ag n p => cons (per p) (num n) 
      } ;

    wb = id Str ;
}
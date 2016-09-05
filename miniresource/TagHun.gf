interface TagHun = open Coordination in {

  param
    PTense = PPres | PPerf ;

    Person = P1 | P2 | P3 ;
    Number = Sg | Pl ;
    Agr = Ag Number Person ;


  oper

    Tag : Type ;

    vblex : Tag ;
    vbser : Tag ;
    det : Tag ;
    n : Tag ;
    adj : Tag ;
    adv : Tag ;
    prn : Tag ;
    conj : Tag ;

    pers : Tag ;
    p1 : Tag ;
    p2 : Tag ;
    p3 : Tag ;
    mf : Tag ;
    sg : Tag ;
    pl : Tag ;
    acc : Tag ;
    nom : Tag ;
    pres : Tag ;
    past : Tag ;

---

    tense : PTense -> Tag ;

    agr : Agr -> Tag ;
---

    Word : Type = { s : Str ; tags : Tag } ;
    --ListTag : Type = ListX ;
    --BaseTag : Tag -> Tag -> ListTag ;
    --ConsTag : Tag -> ListTag -> ListTag ;

    emptyTag : Tag ;


    --cons = overload {
    --  cons : Tag -> Tag -> Tag ;
    --  cons : Tag -> Tag -> Tag -> Tag ;
    --  cons : Tag -> Tag -> Tag -> Tag -> Tag ;
    --} ;
    

}
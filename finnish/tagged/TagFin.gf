resource TagFin = open ResFin, Prelude in {

oper
  Tag : Type = Str ;

  mkTag : Str -> Tag = \t -> "+" + t ;

  tagNForm : NForm -> Str = \nf -> case nf of {
    NCase n c    => tagCase c + tagNumber n ;
    NComit       => mkTag "Com" + tagNumber Pl ;
    NInstruct    => mkTag "Ins" + tagNumber Pl ; 
    NPossNom n   => tagCase Nom + tagNumber n  ;
    NPossGen n   => tagCase Gen + tagNumber n ;
    NPossTransl n => tagCase Transl + tagNumber n ;
    NPossIllat n => tagCase Illat + tagNumber n ;
    NCompound => mkTag "Comp"
    } ;

  tagDegree : Degree -> Str = \deg -> case deg of {
    Posit  => mkTag "Pos" ;
    Compar => mkTag "Comp" ;
    Superl => mkTag "Superl" 
    } ;

  tagCase : Case -> Str = \c -> case c of {
    Nom => mkTag "Nom" ;
    Gen => mkTag "Gen" ;
    Part => mkTag "Par" ;
    Transl => mkTag "Tra" ;
    Ess => mkTag "Ess" ;
    Iness => mkTag "Ine" ;
    Elat => mkTag "Ela" ;
    Illat => mkTag "Ill" ;
    Adess => mkTag "Ade" ;
    Ablat => mkTag "Abl" ;
    Allat => mkTag "All" ;
    Abess => mkTag "Abe" 
    } ;
  tagNumber : Number -> Str = \n -> case n of {
    Sg => mkTag "Sg" ;
    Pl => mkTag "Pl"
    } ;

  tagVerb : (x,y,z : Str) -> Str = \inf,vc,cas ->
    mkTag "V" + mkTag inf + mkTag vc + mkTag cas ;

  tagVerb2 : (x,y : Str) -> Str = \inf,cas ->
    mkTag "V" + mkTag inf + mkTag cas ;
}
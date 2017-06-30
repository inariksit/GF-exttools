-- opers implementing the API of stem-based morphology in finnish/tagged for tagged Finnish

resource StemFin = open TagFin, MorphoFin, Prelude, ParamX in {

flags coding = utf8 ;

oper

----------
-- Quants

  SQuantForm : Type = Predef.Ints 1 ;
  SQuant : Type = {s : SQuantForm => Str ; s2 : Str} ;

  mkSQuant : (x1,_,_,_,_,_,_,_,_,x10 : Str) -> SQuant = \tuo,_,_,_,_,nuo,_,_,_,_ -> 
    { s = table { 0 => tuo ; 1 => nuo } ; s2 = []} ;

  noQuant : SQuant = { s = table { 0 => "mikään" ; 1 => "mitkään" } ; s2 = [] } ;

  indefArt : SQuant = { s = \\_ => [] ; s2 = []} ;

  squant2detOrd : SQuant -> SNum -> {s : NForm => Str}  -> SDet = \quant,num,_ -> 
   MorphoFin.mkDet num.n (snoun2det (quant ** {h=Back})) ** {s2 = \\_ => quant.s2};

  squant2det : SQuant -> SNum -> SDet = \quant,num -> 
   MorphoFin.mkDet num.n (snoun2det (quant ** {h=Back})) ** {s2 = \\_ => quant.s2} ;


  possPron : {a : Agr ; poss : Str} -> SQuant = \pron -> 
   {s = \\_ => pron.poss ; s2 = BIND ++ "+" + getPoss pron.a } ;

  genNP :  {s : NPForm => Str} -> SQuant = \np ->
   { s = \\_ => np.s ! NPCase Gen ; s2 = [] } ;


  getPoss : Agr -> Str = \agr -> 
    table Agr ["PxSg1" ; "PxSg2" ; "Px3" ; "PxPl1" ; "PxPl2" ; "Px3" ; "PxPl2"] ! agr ;
    
-----------------
-- Dets
SNum : Type = {s : Number => Case => Str ; isNum : Bool ; n : Number} ;
SDet : Type = {
      s1 : Case => Str ;       -- minun kolme
      s2 : Harmony => Str ;    -- -ni (Front for -nsä, Back for -nsa)
      sp : Case => Str ;       -- se   (substantival form)
      n : Number ;             -- Pl   (agreement feature for verb)
      isNum : Bool ;           -- True (a numeral is present)
      isPoss : Bool ;          -- True (a possessive suffix is present)
      isDef : Bool ;           -- True (verb agrees in Pl, Nom is not Part)
      isNeg : Bool             -- False (only True for "mikään", "kukaan")
      } ;


----------
-- Nouns

  --Need 2 forms, because we need to form adverbs from adjectives in GF
  SNForm : Type = Predef.Ints 1 ;
  SNoun : Type = {s : SNForm => Str ; h : Harmony } ;

  mkSNoun : Str -> SNoun = \str -> {s=\\_=>str ; h = Back} ;

  nforms2snoun : NForms -> SNoun = \nf -> {
    s = table {
      0 => nf ! 0 ;                -- ukko
      1 => Predef.tk 1 (nf ! 1)    -- uko(n)
      } ;
    h = Back ;
    } ;

    snoun2nounBind : SNoun -> Noun = snoun2noun True (mkTag "N") ;
    snoun2nounSep  : SNoun -> Noun = snoun2noun False (mkTag "N") ;

    snoun2numBind : SNoun -> Noun =  snoun2noun True (mkTag "Num") ;
    snoun2detBind : SNoun -> Noun =  snoun2noun True (mkTag "Pron") ;

    snoun2adjBind : SNoun -> Noun = snoun2noun True [] ;
    snoun2adjSep  : SNoun -> Noun = snoun2noun True [] ;

    snoun2noun : Bool -> Str -> SNoun -> Noun = \b,tag,sn -> 
      {s = \\nf => glue (sn.s ! 0)  (tag + tagNForm nf) ;
       h = sn.h} ;


    snoun2det : SNoun -> Noun = \sn -> 
      {s = \\nf => sn.s ! 0 ; h = sn.h} ;


    snoun2np : Number -> SPN -> NPForm => Str = \n,sn ->
      \\c => sn.s ! (npform2case n c) ; 

--    noun2snoun : Noun -> SNoun = \n -> n ; not needed for anything

    aHarmony : Str -> Harmony = \a -> case a of 
       {"a" => Back ; _   => Front} ;

    harmonyA : Harmony -> Str = harmonyV "a" "ä" ;

    harmonyV : Str -> Str -> Harmony -> Str = \u,y,h -> case h of 
       {Back => u ; Front => y} ;


  SPN : Type = {s : Case  => Str} ;

  snoun2spn : SNoun -> SPN = \n -> {s = \\c => n.s ! 0 ++ tagCase c} ;

  exceptNomSNoun : SNoun -> Str -> SNoun = \noun,nom -> {
      s = \\_ => nom ;
      h = noun.h 
      } ;

  mika_NForms : NForm => Str =
    table {
       NCase Sg Nom  => "mikä" ;
       NCase Pl Nom  => "mitkä" ;
       NCase _ Gen   => "minkä" ;
       NCase _ Part  => "mitä" ;
       NCase _ Illat => "mihin" ;
       NCase _ Iness => "missä" ;
       NCase _ Elat  => "mistä" ;
       NCase _ Allat => "mille" ;
       NCase _ Adess => "millä" ;
       NCase _ Allat => "miltä" ;
       NCase _ Transl => "miksi" ;
       NCase _ Ess   => "minä" ;
       NCase _ Abess => "mittä" ;
       NComit    => "mine" ;
       NInstruct => "min" ;
       _ => "mikä???"
       } ;


-- Adjectives --- could be made more compact by pressing comparison forms down to a few


param
  SAForm =  SAN SNForm | SAAdv ;

oper


  SAdj = {s : SAForm => Str ; h : Harmony} ;

   sAN : SNForm -> SAForm = SAN ;  ---- without eta exp gives internal error 6/8/2013
   sAAdv : SAForm = SAAdv ;
   sANGen : (SAForm => Str) -> Str = \a -> a ! SAN 1 ;

   mkAdj : (hyva,parempi,paras : SNoun) -> (hyvin,paremmin,parhaiten : Str) -> 
            {s : Degree => SAForm => Str ; h : Harmony} = \h,p,ps,hn,pn,ph -> { 
     s = table {
        Posit => table {
           SAN i => h.s ! i + mkTag "A" + tagDegree Posit ;
           SAAdv  => hn
           } ; 
        Compar => table {
           SAN i => h.s ! i + mkTag "A" + tagDegree Compar ;
           SAAdv  => pn
           } ; 
        Superl => table {
           SAN i => h.s ! i + mkTag"A" + tagDegree Superl ;
           SAAdv  => ph
           }
        } ;
      h = Back
      } ;

  snoun2compar : SNoun -> Str = \n -> n.s ! 0 + mkTag "A" + mkTag "Comp" ;
  snoun2superl : SNoun -> Str = \n -> n.s ! 0 + mkTag "A" + mkTag "Superl" ;

-- verbs

oper
  --need more stems just because the adverb is stupid 
  SVForm : Type = Predef.Ints 3 ; --1=tulla; 2=tullee(n); 3=tullu(n); 4=tule(va)
  SVerb : Type = {s : SVForm => Str ; h : Harmony} ;

  ollaSVerbForms : SVForm => Str = table SVForm ["olla";"ollee";"ollu";"ole"] ;

  -- used in Cat
  SVerb1 = {s : SVForm => Str ; sc : SubjCase ; h : Harmony ; p : Str} ;

  sverb2verbBind : SVerb -> Verb = sverb2verb True ;
  sverb2verbSep  : SVerb -> Verb = sverb2verb False ;

  vforms2sverb : VForms -> SVerb = \v -> 
    {s = v ; h = case (last (v ! 0)) of {"a" => Back ; _ => Front}} ;

  sverb2verb : Bool -> SVerb -> Verb = \b,sverb -> 
    let
      plus = plusIf b ;
      vh   = sverb.s ;

      a = harmonyA sverb.h ;
      o = harmonyV "o" "ö" sverb.h ;
      u = harmonyV "u" "y" sverb.h ;

      tulla  = vh ! 0 ;
      tullee = vh ! 1 ;
      tullu  = vh ! 2 ;
      tule   = vh ! 3 ;

      tuleva = tule + "v" + a ;
      tulema = "TULEMA" ;
      tultava = "TULTAVA" ;
      


      --TODO making adverbs out of the nouns
      tullutN : Noun = snoun2noun b (mkTag "PrfPrc" + mkTag "Act" + mkTag "Pos") {
        s = table SNForm [
          tulla ;
          tullee --???
        ] ;
        h = sverb.h
        } ;

      tultuN  : Noun = snoun2noun b (mkTag "PrfPrc" + mkTag "Pass" + mkTag "Pos") {
        s = table SNForm [
          tulla ;
          tullu ; --???
        ] ;
        h = sverb.h
        } ;

      tulevaN : Noun = snoun2noun b (mkTag "V" + mkTag "PrsPrc" + mkTag "Act" + mkTag "Pos") {
        s = table SNForm [
          tulla ;
          tulla ; --??? tuleva ;
        ] ;
        h = sverb.h
        } ;

      tultavaN : Noun = snoun2noun b (mkTag "PrsPrc" + mkTag "Pass" + mkTag "Pos") {
        s = table SNForm [
          tulla ;
	  tulla ; ---???
        ] ;
        h = sverb.h
        } ;

      tulemaN : Noun = snoun2noun b (mkTag "AgPrc" + mkTag "Pos") {
        s = table SNForm [
          tulla ;
	  tulla ; ---???
        ] ;
        h = sverb.h
        } ;

    in
    {s = table {
      Inf Inf1 => plus tulla (tagVerb2 "Inf1" "Lat");
      Inf Inf1Long  => plus tulla (tagVerb "Inf1" "Act" "Tra") ;
      Inf Inf2Iness => plus tulla (tagVerb "Inf2" "Act" "Ine") ;
      Inf Inf2Instr => plus tulla (tagVerb "Inf2" "Act" "Ins") ;
      Inf Inf2InessPass => plus tulla (tagVerb "Inf2" "Pass" "Ine") ;
      Inf Inf3Iness => plus tulla (tagVerb2 "Inf3" "Ine") ;
      Inf Inf3Elat  => plus tulla (tagVerb2 "Inf3" "Ela") ;
      Inf Inf3Illat => plus tulla (tagVerb2 "Inf3" "Ill") ;
      Inf Inf3Adess => plus tulla (tagVerb2 "Inf3" "Ade") ;
      Inf Inf3Abess => plus tulla (tagVerb2 "Inf3" "Abe") ;
      Inf Inf3Instr => plus tulla (tagVerb2 "Inf3" "Man") ;
      Inf Inf3InstrPass => plus tulla (tagVerb "Inf" "Pass" "Ins") ;
      Inf Inf4Nom   => plus tulla (tagVerb "N" "Nom" "Sg") ; --omorfi actually has V N Nom Sg
      Inf Inf4Part  => plus tulla (tagVerb "N" "Par" "Sg") ;
      Inf Inf5      => plus tulla (mkTag "Adv" + mkTag "Act") ; --poss.suff. added later
      Inf InfPresPart => plus tulla (tagVerb "PrsPrc" "Act" "Pos") ;
      Inf InfPresPartAgr => plus tulla (tagVerb "PrsPrc" "Act" "Pos") ;

      Presn Sg P1  => plus tulla (tagVerb "Prs" "Act" "Sg1") ;
      Presn Sg P2  => plus tulla (tagVerb "Prs" "Act" "Sg2") ;
      Presn Sg P3  => plus tulla (tagVerb "Prs" "Act" "Sg3") ;
      Presn Pl P1  => plus tulla (tagVerb "Prs" "Act" "Pl1") ;
      Presn Pl P2  => plus tulla (tagVerb "Prs" "Act" "Pl2") ;
      Presn Pl P3  => plus tulla (tagVerb "Prs" "Act" "Pl3") ;
      Impf Sg P1   => plus tulla (tagVerb "Pst" "Act" "Sg1") ;    --# notpresent
      Impf Sg P2   => plus tulla (tagVerb "Pst" "Act" "Sg2") ;    --# notpresent
      Impf Sg P3   => plus tulla (tagVerb "Pst" "Act" "Sg3") ;    --# notpresent
      Impf Pl P1   => plus tulla (tagVerb "Pst" "Act" "Pl1") ;  --# notpresent
      Impf Pl P2   => plus tulla (tagVerb "Pst" "Act" "Pl2") ;  --# notpresent
      Impf Pl P3   => plus tulla (tagVerb "Pst" "Act" "Pl3") ;  --# notpresent
      Condit Sg P1 => plus tulla (tagVerb "Cond" "Act" "Sg1") ;   --# notpresent
      Condit Sg P2 => plus tulla (tagVerb "Cond" "Act" "Sg2") ;   --# notpresent
      Condit Sg P3 => plus tulla (tagVerb "Cond" "Act" "Sg3") ;   --# notpresent
      Condit Pl P1 => plus tulla (tagVerb "Cond" "Act" "Pl1") ; --# notpresent
      Condit Pl P2 => plus tulla (tagVerb "Cond" "Act" "Pl2") ; --# notpresent
      Condit Pl P3 => plus tulla (tagVerb "Cond" "Act" "Pl3") ; --# notpresent
      Potent Sg P1 => plus tulla (tagVerb "Pot" "Act" "Sg1") ;   --# notpresent
      Potent Sg P2 => plus tulla (tagVerb "Pot" "Act" "Sg2") ;   --# notpresent
      Potent Sg P3 => plus tulla (tagVerb "Pot" "Act" "Sg3") ;   --# notpresent
      Potent Pl P1 => plus tulla (tagVerb "Pot" "Act" "Pl1") ; --# notpresent
      Potent Pl P2 => plus tulla (tagVerb "Pot" "Act" "Pl2") ; --# notpresent
      Potent Pl P3 => plus tulla (tagVerb "Pot" "Act" "Pl3") ; --# notpresent
      PotentNeg    => plus tulla (tagVerb "Pot" "Act" "ConNeg") ; --# notpresent
      Imper Sg     => plus tulla (tagVerb "Impv" "Act" "Sg2") ;
      Imper Pl     => plus tulla (tagVerb "Impv" "Act" "Pl2") ;
      ImperP3 Sg   => plus tulla (tagVerb "Impv" "Act" "Sg3") ;
      ImperP3 Pl   => plus tulla (tagVerb "Impv" "Act" "Pl3") ;
      ImperP1Pl    => plus tulla (tagVerb "Impv" "Act" "Pl1") ;
      ImpNegPl     => plus tulla (tagVerb "Impv" "Act" "ConNeg") ;
      PassPresn True    => plus tulla (tagVerb "Prs" "Pass" "Pe4") ;
      PassPresn False   => plus tulla (tagVerb "Prs" "Pass" "ConNeg") ;
      PassImpf True     => plus tulla (tagVerb "Pst" "Pass" "Pe4") ;  --# notpresent
      PassImpf False    => plus tulla (tagVerb "Pst" "Pass" "Pe4" + mkTag "ConNeg") ;  --# notpresent
      PassCondit True   => plus tulla (tagVerb "Cond" "Pass" "Pe4") ;  --# notpresent
      PassCondit False  => plus tulla (tagVerb "Cond" "Pass" "Pe4" + mkTag "ConNeg") ;  --# notpresent
      PassPotent True   => plus tulla (tagVerb "Pot" "Pass" "Pe4") ;  --# notpresent
      PassPotent False  => plus tulla (tagVerb "Pot" "Pass" "Pe4" + mkTag "ConNeg") ;  --# notpresent
      PassImper True    => plus tulla (tagVerb "Impv" "Pass" "Pe4") ;
      PassImper False   => plus tulla (tagVerb "Impv" "Pass" "Pe4" + mkTag "ConNeg") ;

      PresPartAct (AN n)  => tulevaN.s ! n ;
      PresPartAct AAdv    => plus tuleva ("sti" + mkTag "Adv") ;
      PresPartPass (AN n)  => tultavaN.s ! n ;
      PresPartPass AAdv    => plus tultava ("sti" + mkTag "Adv") ;

      PastPartAct (AN n)  => tullutN.s ! n ;
      PastPartAct AAdv    => plus tullee ("sti" + mkTag "Adv") ;
      PastPartPass (AN n) => tultuN.s ! n ;
      PastPartPass AAdv   => plus tullu ("sti" + mkTag "Adv") ;

      AgentPart (AN n)  => tulemaN.s ! n ;
      AgentPart AAdv    => plus tulema ("sti" + mkTag "Adv")

      } ;
    sc = SCNom ;
    lock_V = <>
    } ;


  predSV : SVerb1 -> VP = \sv ->
    predV sv ; 
    -- (sverb2verbSep sv ** {p = sv.p ; sc = sv.sc ; h = sv.h}) ;

-- word formation functions

  sverb2snoun : SVerb1 -> SNoun = \v ->    -- syöminen
    let teke = v.s ! 3 in 
    nforms2snoun (dNainen (teke + "minen")) ;


  --the first form is the verb stem + V <Other tags> combination
  --before the nominal/adjectival tags, which are added in snoun2noun.
  --Second form does not have V + PrsPrc + etc. tags
  --but it is used only as an adverb stem.
  --This is very hacky and confusing.
  sverb2nounPresPartAct : SVerb1 -> SNoun = \v ->  -- syövä
    let tulla  = v.s ! 0 ;
	tullee = v.s ! 1 
    in { s = table SNForm [
           glue tulla (tagVerb "PrsPrc" "Act" "Pos") ;
           tullee ] ; --this is not used elsewhere but adverb formation
         h = Back ;	 
    } ; 

  sverb2nounPresPartPass : SVerb1 -> SNoun = \v ->  -- syötävä
    let tulla = v.s ! 0 ;
        tultava = v.s ! 0 + " TODO (stupid adverbs)"
    in { s = table SNForm [
           glue tulla (tagVerb "PrsPrc" "Pass" "Pos") ;
           tultava ] ;
         h = Back ;
    } ;

  dLava : Str -> NForms = \s -> dUkko s (s + "n") ;
  
 --- to use these at run time in ParseFin
  partPlus = glue ;

-- auxiliary

    plusIf : Bool -> Str -> Str -> Str = \b,x,y -> case b of {
      True  => x + y ;
      False => glue x y 
      } ;

-- for Symbol

  addStemEnding : Str -> SPN = \i -> 
    {s = \\c => i ++ BIND ++ defaultCaseEnding c} ;
--    {s = \\c => i ++ bindColonIfS (NCase Sg c) ++ defaultCaseEnding c} ;

  bindIfS : SNForm -> Str = \c -> case c of {
    --NCase Sg Nom => [] ;
    _ => BIND
    } ;

  bindColonIfS : SNForm -> Str = \c -> case c of {
    --NCase Sg Nom => [] ;
    _ => BIND ++ ":" ++ BIND
    } ;

-----------------------------------------------------------------------
---- a hack to make VerbFin compile accurately for library, 
---- and in a simplified way for ParseFin (in stemmed/)
---- and even more mysterious way for tagged fin (here)

  slashV2VNP : (SVerb1 ** {c2 : Compl ; vi : VVType}) -> (NP ** {isNeg : Bool}) -> 
    (VP ** {c2 : Compl}) -> (VP ** {c2 : Compl}) = 
    \v, np, vp -> {
      s  = v ;
      s2 = \\fin,b,a =>  "HARGLE" ; -- appCompl fin b v.c2 np ++ v.s ! SVInf ;
                         ---- infVP v.sc b a vp v.vi ;
                         -- ignoring Acc variation and pre/postposition and proper inf form
      ext = [] ;
      adv = \\_ => v.p ;
      vptyp = vp.vptyp ; -- ignoring np.isNeg
      c2 = vp.c2  -- ** {p1="BARGLE"}
      } ;

  -- slashV2VNP : (SVerb1 ** {c2 : Compl ; vi : VVType}) -> (NP ** {isNeg : Bool}) -> 
  --   (VP ** {c2 : Compl}) -> (VP ** {c2 : Compl}) 
  --   = \v, np, vp -> 
  --     insertObjPre np.isNeg
  --       (\fin,b,a ->  appCompl fin b v.c2 np ++ 
  --                     infVP v.sc b a vp (vvtype2infform v.vi)) 
  --         (predSV v) ** {c2 = vp.c2} ;


---- VP now stemming-dependent. AR 7/12/2013

  VP = {
    s   : SVerb1 ;
    s2  : Bool => Polarity => Agr => Str ; -- talo/talon/taloa
    adv : Polarity => Str ; -- ainakin/ainakaan
    ext : Str ;
    vptyp : {isNeg : Bool ; isPass : Bool} ;-- True if some complement is negative, and if the verb is rendered in the passive
    } ;

  defaultVPTyp = {isNeg = False ; isPass = False} ;
    
--  HVerb : Type = Verb ** {sc : SubjCase ; h : Harmony ; p : Str} ;
 
  predV : SVerb1 -> VP = \verb -> {
    s = verb ;
    s2 = \\_,_,_ => [] ;
    adv = \\_ => verb.p ; -- the particle of the verb
    ext = [] ;
    vptyp = defaultVPTyp ;
    } ;

  old_VP = {
    s   : VIForm => Anteriority => Polarity => Agr => {fin, inf : Str} ; 
    s2  : Bool => Polarity => Agr => Str ; -- talo/talon/taloa
    adv : Polarity => Str ; -- ainakin/ainakaan
    ext : Str ;
    sc  : SubjCase ;
    isNeg : Bool ; -- True if some complement is negative
    h   : Harmony
    } ;

  vp2old_vp : VP -> old_VP = \vp -> 
    let 
     verb = sverb2verbSep vp.s ; 
     sverb :  VIForm => Anteriority => Polarity => Agr => {fin, inf : Str} = \\vi,ant,b,agr0 => 
      let
        agr = verbAgr agr0 ;
        verbs = verb.s ;
        part  : Str = case vi of {
          VIPass _ => verbs ! PastPartPass (AN (NCase agr.n Nom)) ; 
          _        => verbs ! PastPartAct (AN (NCase agr.n Nom))
          } ; 

        eiv : Str = case agr of {
          {n = Sg ; p = P1} => "en" ;
          {n = Sg ; p = P2} => "et" ;
          {n = Sg ; p = P3} => "ei" ;
          {n = Pl ; p = P1} => "emme" ;
          {n = Pl ; p = P2} => "ette" ;
          {n = Pl ; p = P3} => "eivät"
          } ;

        einegole : Str * Str * Str = case <vi,agr.n> of {
          <VIFin Pres,        _>  => <eiv, verbs ! Imper Sg,     "ole"> ;
          <VIFin Fut,         _>  => <eiv, verbs ! Imper Sg,     "ole"> ;   --# notpresent
          <VIFin Cond,        _>  => <eiv, verbs ! Condit Sg P3, "olisi"> ;  --# notpresent
          <VIFin Past,        Sg> => <eiv, part,                 "ollut"> ;  --# notpresent
          <VIFin Past,        Pl> => <eiv, part,                 "olleet"> ;  --# notpresent
          <VIImper,           Sg> => <"älä", verbs ! Imper Sg,   "ole"> ;
          <VIImper,           Pl> => <"älkää", verbs ! ImpNegPl, "olko"> ;
          <VIPass Pres,       _>  => <"ei", verbs ! PassPresn False,  "ole"> ;
          <VIPass Fut,        _>  => <"ei", verbs ! PassPresn False,  "ole"> ; --# notpresent
          <VIPass Cond,       _>  => <"ei", verbs ! PassCondit False,  "olisi"> ; --# notpresent
          <VIPass Past,       _>  => <"ei", verbs ! PassImpf False,  "ollut"> ; --# notpresent
          <VIInf i,           _>  => <"ei", verbs ! Inf i, "olla"> ----
          } ;

        ei  : Str = einegole.p1 ;
        neg : Str = einegole.p2 ;
        ole : Str = einegole.p3 ;

        olla : VForm => Str = table {
           PassPresn  True => verbOlla.s ! Presn Sg P3 ;
           PassImpf   True => verbOlla.s ! Impf Sg P3 ;   --# notpresent
           PassCondit True => verbOlla.s ! Condit Sg P3 ; --# notpresent
           vf => verbOlla.s ! vf
           } ;

        vf : Str -> Str -> {fin, inf : Str} = \x,y -> 
          {fin = x ; inf = y} ;
        mkvf : VForm -> {fin, inf : Str} = \p -> case <ant,b> of {
          <Simul,Pos> => vf (verbs ! p) [] ;
          <Anter,Pos> => vf (olla ! p)  part ;    --# notpresent
          <Anter,Neg> => vf ei          (ole ++ part) ;   --# notpresent
          <Simul,Neg> => vf ei          neg
          } ;
        passPol = case b of {Pos => True ; Neg => False} ;
      in
      case vi of {
        VIFin Past => mkvf (Impf agr.n agr.p) ;     --# notpresent
        VIFin Cond => mkvf (Condit agr.n agr.p) ;  --# notpresent
        VIFin Fut  => mkvf (Presn agr.n agr.p) ;  --# notpresent
        VIFin Pres => mkvf (Presn agr.n agr.p) ;
        VIImper    => mkvf (Imper agr.n) ;
        VIPass Past    => mkvf (PassImpf passPol) ;  --# notpresent
        VIPass Cond    => mkvf (PassCondit passPol) ; --# notpresent
        VIPass Fut     => mkvf (PassPresn passPol) ;  --# notpresent
        VIPass Pres    => mkvf (PassPresn passPol) ;  
        VIInf i    => mkvf (Inf i)
        }
    in {
    s = case vp.vptyp.isPass of {
          True => \\vif,ant,pol,agr =>
                     case vif of {
                       VIFin t  => sverb ! VIPass t ! ant ! pol ! agr ;
		       _        => sverb ! vif ! ant ! pol ! agr 
                     } ;
          _ => sverb
          } ;
    s2 = \\b,p,a => vp.s2 ! b ! p ! a ; --extra verb forms appear here???
    adv = vp.adv ; -- the particle of the verb
    ext = vp.ext ;
    sc = vp.s.sc ;
    h = vp.s.h ;
    isNeg = vp.vptyp.isNeg 
    } ;

  insertObj : (Bool => Polarity => Agr => Str) -> VP -> VP = \obj,vp -> {
    s = vp.s ;
    s2 = \\fin,b,a => vp.s2 ! fin ! b ! a  ++  obj ! fin ! b ! a ;
    adv = vp.adv ;
    ext = vp.ext ;
    sc = vp.sc ; 
    h = vp.h ;
    vptyp = vp.vptyp
    } ;

  insertObjPre : Bool -> (Bool -> Polarity -> Agr -> Str) -> VP -> VP = \isNeg, obj,vp -> {
    s = vp.s ;
    s2 = \\fin,b,a => obj fin b a ++ vp.s2 ! fin ! b ! a ;
    adv = vp.adv ;
    ext = vp.ext ;
    vptyp = {isNeg = orB vp.vptyp.isNeg isNeg ; isPass = vp.vptyp.isPass} ;
    } ;

  insertAdv : (Polarity => Str) -> VP -> VP = \adv,vp -> {
    s = vp.s ;
    s2 = vp.s2 ;
    ext = vp.ext ;
    adv = \\b => vp.adv ! b ++ adv ! b ;
    sc = vp.sc ; 
    h = vp.h ;
    vptyp = vp.vptyp --- missään
    } ;

  passVP : VP -> Compl -> VP = \vp,pr -> {
    s = {s = vp.s.s ; h = vp.s.h ; p = vp.s.p ; sc = npform2subjcase pr.c} ; -- minusta pidetään ---- TODO minun katsotaan päälle
    s2 = \\b,p,a => pr.s.p1 ++ vp.s2 ! b ! p ! a ++ pr.s.p2 ; ---- possessive suffix
    ext = vp.ext ;
    adv = vp.adv ;
    vptyp = {isNeg = vp.vptyp.isNeg ; isPass = True} ; 
    } ;

  insertExtrapos : Str -> VP -> VP = \obj,vp -> {
    s = vp.s ;
    s2 = vp.s2 ;
    ext = vp.ext ++ obj ;
    adv = vp.adv ;
    sc = vp.sc ; 
    h = vp.h ;
    vptyp = vp.vptyp
    } ;

  mkClausePol : Bool -> (Polarity -> Str) -> Agr -> VP -> Clause = 
    \isNeg,sub,agr,vp -> {
      s = \\t,a,b => 
         let
           pol = case isNeg of {
            True => Neg ; 
            _ => b
            } ; 
           c = (mkClausePlus sub agr vp).s ! t ! a ! pol 
         in 
         table {
           SDecl  => c.subj ++ c.fin ++ c.inf ++ c.compl ++ c.adv ++ c.ext ;
           SQuest => c.fin ++ BIND ++ questPart c.h ++ c.subj ++ c.inf ++ c.compl ++ c.adv ++ c.ext
           }
      } ;
  mkClause : (Polarity -> Str) -> Agr -> VP -> Clause = 
    \sub,agr,vp -> {
      s = \\t,a,b => let c = (mkClausePlus sub agr vp).s ! t ! a ! b in 
         table {
           SDecl  => c.subj ++ c.fin ++ c.inf ++ c.compl ++ c.adv ++ c.ext ;
           SQuest => c.fin ++ BIND ++ questPart c.h ++ c.subj ++ c.inf ++ c.compl ++ c.adv ++ c.ext
           }
      } ;

  mkClausePlus : (Polarity -> Str) -> Agr -> VP -> ClausePlus =
    \sub,agr,vp0 -> let vp = vp2old_vp vp0 in {
      s = \\t,a,b => 
        let 
          agrfin = case vp.sc of {
                    SCNom => <agr,True> ;
                    _ => <agrP3 Sg,False>      -- minun täytyy, minulla on
                    } ;
          verb  = vp.s ! VIFin t ! a ! b ! agrfin.p1 ;
        in {subj = sub b ; 
            fin  = verb.fin ; 
            inf  = verb.inf ; 
            compl = vp.s2 ! agrfin.p2 ! b ! agr ;
            adv  = vp.adv ! b ; 
            ext  = vp.ext ; 
            h    = selectPart vp0 a b
            }
     } ;

  selectPart : VP -> Anteriority -> Polarity -> Harmony = \vp,a,p -> 
    case p of {
      Neg => Front ;  -- eikö tule
      _ => case a of {
        Anter => Back ; -- onko mennyt --# notpresent
        _ => vp.s.h  -- tuleeko, meneekö
        }
      } ;

-- the first Polarity is VP-internal, the second comes form the main verb:
-- ([main] tahdon | en tahdo) ([internal] nukkua | olla nukkumatta)
  infVPGen : Polarity -> SubjCase -> Polarity -> Agr -> VP -> InfForm -> Str =
    \ipol,sc,pol,agr,vp0,vi ->
        let 
          vp = vp2old_vp vp0 ;
          fin = case sc of {     -- subject case
            SCNom => True ; -- minä tahdon nähdä auton
            _ => False           -- minun täytyy nähdä auto
            } ;
          verb = case ipol of {
            Pos => <vp.s ! VIInf vi ! Simul ! Pos ! agr, []> ; -- nähdä/näkemään
            Neg => <(vp2old_vp (predV (vpVerbOlla ** {sc = SCNom ; h = Back ; p = []}))).s ! VIInf vi ! Simul ! Pos ! agr,
                    (vp.s ! VIInf Inf3Abess ! Simul ! Pos ! agr).fin> -- olla/olemaan näkemättä
            } ;
          vph = vp.h ;
          poss = case vi of {
            InfPresPartAgr => possSuffixGen vph agr ; -- toivon nukkuva + ni
            _ => []
            } ;
          compl = vp.s2 ! fin ! pol ! agr ++ vp.adv ! pol ++ vp.ext     -- compl. case propagated
        in
        verb.p1.fin ++ verb.p1.inf ++ poss ++ verb.p2 ++ compl ;

  infVP : SubjCase -> Polarity -> Agr -> VP -> InfForm -> Str = infVPGen Pos ;

  vpVerbOlla : SVerb1 = {s = ollaSVerbForms ; sc = SCNom ; h = Back ; p = []} ;

}
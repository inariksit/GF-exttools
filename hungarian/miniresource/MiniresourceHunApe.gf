concrete MiniresourceHunApe of Miniresource = 
  MiniresourceHun - [i_NP, youSg_NP, he_NP, she_NP, it_NP, we_NP, youPl_NP, they_NP] 
   with (TagHun = TagHunApe) ** {

 
lin
  --Some prefabricated noun phrases:
  i_NP = pronNP "én" Sg P1 ;

  youSg_NP = pronNP "te" Sg P2 ;

  he_NP = pronNP "ő" Sg P3 ;

  she_NP = pronNP "ő" Sg P3 ;

	we_NP = pronNP "mi" Pl P1 ;

	youPl_NP = pronNP "ti" Pl P2 ;

	they_NP = pronNP "ők" Pl P3 ;

oper

  pronNP : Str -> Number -> Person -> NP = \en,n,p ->
   let P = per p ;
       N = num n ;
   in lin NP { s = en + prn + pers + P + mf + N ;
               a = Ag n p } ;
}
--A miniature resource grammar for a kind of "pidgin" English with no inflection.
--Generates sentences such as "the man arrive", "you be big", "me love you not".
--Every category is linearized as just a single string. All strings are built as concatenations of other strings.
--Negative polarity is linearized by adding "not" to the end of the sentence.
--The perfect tense is linearized by adding "in the past" to the end of the sentence.
--You can adapt this for your language by gradually adding more detail.

--Michal Boleslav Měchura, http://www.lexiconista.com/gf
--Last updated: 14 September 2014

incomplete concrete MiniresourceHun of Miniresource = open Prelude, TagHun in {
	flags coding = utf8;


	--SENTENCE:
	lincat S = SS ;
	
	--Build a sentence from a tense, a polarity and a clause:
	lin UseCl t p cl = { --UseCl : Tense -> Pol -> Cl -> S
		s = cl.subj ++ p.s ++ cl.s ! t.t 
	} ;
	
	--Build a new sentence by connecting two existing sentences with a conjunction:
	lin ConjS co sx sy = { --ConjS : Conj -> S  -> S  -> S
		s = sx.s ++ co.s ++ sy.s
	} ;
	
	
	
	--TENSE:
	lincat Tense = { s : Str ; t : PTense } ;
	
	--Two prefabricated tenses:
	lin Pres = { s = [] ; t = PPres };
	lin Perf = { s = [] ; t = PPerf };
	
	
	
	--POLARITY:
	lincat Pol = SS ;
	
	--Two prefabricated polarities:
	lin Pos = {s = []};
	lin Neg = {s = wb ("nem" + adv) };
	

	
	--CLAUSE:
	lincat Cl = { s : PTense => Str ; 
								subj : Str } ;
	
	--Build a clause from a noun phrase (= the subject) and a verb phrase:
	lin PredVP np vp = { --PredVP : NP -> VP -> Cl
		s = vp.s ! np.a ; 
		subj = np.s ++ BIND 
		     ++ nom ++ BIND 
		     ++ endWord --Subject case is finalised, can add the $ sign
	} ;
	
	

	--VERB PHRASE:
	lincat VP = { s : Agr => PTense => Str } ;
	
	--Build a verb phrase by elevating a verb:
	lin UseV v = {
	  s = \\a,t => wb ( v.s ++ BIND 
    				  		 ++ v.tags ++ BIND 
    				  		 ++ tense t ++ BIND 
    							 ++ agr a )
  } ;
	
	--Build a verb phrase from a two-place verb and a noun phrase (= the object):
	lin ComplV2 v2 np = { --ComplV2 : V2 -> NP -> VP
		s = \\a,t => wb ( v2.s ++ BIND 
						 			 ++ v2.tags ++ BIND 
             			 ++ tense t ++ BIND
          		     ++ agr a )
                --    ++ BIND ++ np.agr  --Obj agr distinguishes only for def, indef and 2nd person
   						++ np.s ++ BIND
   						++ v2.compl ++ BIND 
   						++ endWord --we know object case, can add $
	} ;
	
	--Build a verb phrase from an adjective phrase, using the verb 'to be' ("big" --> "is big"):
	lin CompAP ap = ap ** { --CompAP : AP -> VP
		s = \\a,t => chooseCopula True ++ BIND ++ tense t ++ ap.s
	};
	
	
	
	--VERB:
	lincat V = Word ;
	
	--Some prefabricated verbs:
	lin walk_V =  mkV "jár" ;
--	lin arrive_V = mkV "arrive" ;
	
	
	
	--TWO-PLACE VERB:
	lincat V2 = Word ** { compl : Tag } ;
	
	--Some prefabricated two-place verbs:
	lin love_V2 = mkV2 "szeret" acc ;
--	lin please_V2 = mkV "please" ;


	
	--NOUN PHRASE:
	lincat NP = { s : Str ; a : Agr } ;
	
	--Some prefabricated noun phrases:
	--lin i_NP = {s = "me"};
	--lin youSg_NP = {s = "you"};
	-- lin he_NP = { s = "ő" + prn ; 
	--	 						 a = Ag Sg P3 } ;
	--lin she_NP = {s = "her"};
	--lin we_NP = {s = "us"};
	--lin youPl_NP = {s = "you"};
	--lin they_NP = {s = "them"};
	
	--Build a noun phrase from a determiner and a common noun:
	lin DetCN det cn = { --DetCN : Det -> CN -> NP
		s = (wb det.s)
		 ++ startWord ++ BIND --CN starts with ^, we add end when we know the case
     ++ cn.s ++ BIND 
     ++ cn.tags ;
    a = det.a ;
	} ;
	
	--Build a new noun phrase by connecting two existing noun phrases with a conjunction:
	--lin ConjNP co nx ny = { --ConjNP : Conj -> NP -> NP -> NP
	--	s = nx.s ++ co.s ++ ny.s
	--};
	
	
	
	--DETERMINER:
	lincat Det = { s : Str ; a : Agr };
	
	--Some prefabricated determiners:
  lin a_Det = {s = "egy" + det + ind ; a = Ag Sg P3};
	lin the_Det = {s = "a" + det + defi ; a = Ag Sg P3};
	--lin every_Det = {s = "minden" + det + def ; a = Ag Sg P3};
 --   lin this_Det = {s = "this"};
	--lin these_Det = {s = "these"};
 --   lin that_Det = {s = "that"};
	--lin those_Det = {s = "those"};
	
	
	
	--COMMON NOUN:
	lincat CN = Word;
	
	--Build a common noun by elevating a noun:
	lin UseN n = n ;  --UseN : N -> CN
	
	--Build a new common noun by adding an adjective phrase to an existing common noun:
	lin ModCN ap cn = cn ** { --ModCN : AP -> CN -> CN
		s = cn.s ++ ap.s
	};
	
	
	
	--NOUN:
	lincat N = Word;
	
	--Some prefabricated nouns:
	--lin man_N = {s = "man"};
	--lin woman_N = {s = "woman"};
	lin house_N = mkN "ház" ;
	--lin tree_N = {s = "tree"};
	
	
	
	--ADJECTIVE PHRASE:
	lincat AP = Word;
	
	--Build an adjective phrase by elevating an adjective:
	lin UseA a = a ; --UseA : A -> AP
	
	--Build a new adjective phrase by adding an ad-adjective to an existing adjective phrase:
	lin AdAP ad ap = ap ** { --AdAP : AdA -> AP -> AP
		s = ad.s ++ ap.s
	};
	
	
	
	--ADJECTIVE:
	lincat A = Word;
	
	--Some prefabricated adjectives:
	--lin big_A = {s = "big"};
	--lin small_A = {s = "small"};
	--lin green_A = {s = "green"};
	
	
	
	--AD-ADJECTIVE:
	lincat AdA = SS ;
	
	--A prefabricated ad-adjective:
	lin very_AdA = {s = wb ("nagyon" + adv) } ;
	
	
	
	--CONJUNCTION:
	lincat Conj = SS ;
	
	--Two prefabricated conjunctions:
	lin and_Conj = {s = wb ("és" + conj) } ;
--	lin or_Conj = {s = "or"};


oper

  chooseCopula : Bool -> Str = \true -> if_then_Str true "van" [] ;

  mkV : Str -> Word = \jar -> { s = jar ; tags = vblex } ;

  mkV2 : Str -> Tag -> Word ** {compl : Tag } 
    = \szeret,acc -> { s = szeret ; tags = vblex ; compl = acc } ;

  mkN : Str -> Word = \haz -> { s = haz ; tags = n } ;

}
# Outsourcing morphology in Grammatical Framework: a case study for Hungarian

### Abstract

We implement a miniature resource grammar in Grammatical Framework (GF) by using 
resources developed in the Apertium community: a finite-state morphological 
transducer and disambiguator. The entries in the GF grammar consist of
just base lemmas with tags added, such as `én<prn><p1><sg><acc>` and
`én<prn><p1><sg><dat>` instead of *engem* and *nekem*, respectively. The user
of such our grammar would type in the full forms, and the input is analysed 
by the external morphological analyser, which is further disambiguated by a 
Constraint Grammar (CG). Only then is the sentence given to the GF grammar, 
which will return the syntactic parse tree.

Our goals are twofold: to share resources within the rule-based community, as 
well as to prevent the GF grammar growing in size. Especially for languages with 
complex morphology, not having to store large inflection tables makes the grammar
smaller and faster. As for development effort, we hope that the external resources
would also save time in the grammar writing process; however, the design we propose
is arguably less intuitive than writing a traditional GF grammar. The next steps are 
to scale up to a full resource grammar, and do tagset stuff.
, and scaling up to 
a full resource grammar, we may find incompatibilities in the design of the morphology.
the development of the miniature resource, and 
there 


## Introduction

Grammatical Framework is a 
Usually, writing the morphology in GF is the first step in starting a resource grammar. A new grammar writer can easily spend
weeks in investigating old grammar books. Even if the language has a finite-state morphology, the representation is very
different from the GF morphology, and hardly lends itself for inspiration. Another option is to generate forms to feed in the
GF lexicon using an existing morphological analyser---this frees the grammar writer from spending time on elegant
morphological rules, but does not solve the problem of grammar blowup.


## Implementation

## Discussion

Tagset compatibility: 

compare Apertium and Ocamorph tags:

```
> egy
egy/ART
egy/ADV
egy/NUM
> fa
fa/NOUN
> nem
nem/UTT-INT
nem/ADV
nem/NOUN
> szeret
szeret/VERB
> engem
én/NOUN<PERS<1>><CAS<ACC>>

^egy/egy<det><ind>$ ^fa/fa<n><sg><nom>$ ^nem/nem<adv>/nem<ij>$ ^szeret/szeret<vblex><pri><p3><sg>$ ^engem/én<prn><pers><p1><mf><sg><acc>$
```




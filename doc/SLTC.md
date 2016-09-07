# Outsourcing morphology in Grammatical Framework: a case study for Hungarian

### Abstract

We implement a miniature resource grammar in Grammatical Framework (GF) by using 
resources developed in the Apertium community: a finite-state morphological 
transducer and a disambiguation grammar. 
Our goals are twofold: to share resources within the rule-based community, as 
well as to prevent the GF grammar growing in size. Especially for languages with 
complex morphology, not having to store large inflection tables makes the grammar
smaller and faster. As for development effort, we hope that the external resources
would also save time in the grammar writing process. The next steps are to scale up
to a full resource grammar, and parametrise the grammar for different tagsets.


## Introduction

Grammatical Framework (GF, \cite{aarne}) is a grammar formalism and a programming language for writing multilingual grammars.
Like xxx, GF makes a distinction between phenogrammatical and tectogrammatical structure; in GF terms expressed as *abstract* and *concrete* syntax. 
By allowing multiple concrete syntaxes for a single abstract syntax, GF grammars are a natural choice for interlingual translation.

One of the most important contributions of GF is its Resource Grammar Library \cite{rglLILT}, which contains 40 languages as of September 2016. All the languages share the same core abstract syntax, and each language can have an extra module for constructions that are particular to that language.

Usually, writing the morphology in GF is the first step in starting a resource grammar. A new grammar writer can easily spend
weeks in investigating old grammar books. Even if the language has a finite-state morphology, the representation is very
different from the GF morphology, and hardly lends itself for inspiration. Another option is to generate forms to feed in the
GF lexicon using an existing morphological analyser---this frees the grammar writer from spending time on elegant
morphological rules, but does not solve the problem of grammar blowup.


## Implementation

We implemented a miniature version (44 functions) of the GF resource grammar \cite{rglLILT} for this work. At this stage, the grammar is not suited for more than toy applications, but the 

The entries in the GF grammar consist of just base lemmas with tags added: for example, the accusative and dative forms of the first person singular pronoun are stored as `én<prn><p1><sg><acc>` and `én<prn><p1><sg><dat>` 
instead of their inflected forms, *engem* and *nekem* respectively.
The user of our grammar types in normal Hungarian words, and the input is analysed 
by the external morphological analyser, which is further disambiguated by a 
Constraint Grammar (CG). Only then is the sentence given to the GF grammar, 
which will return the syntactic parse tree.


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




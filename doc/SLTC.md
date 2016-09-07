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

Grammatical Framework (GF, \cite{aarne}) is a grammar formalism and a programming 
language for writing multilingual grammars.
GF belongs to the family of categorial grammars, and hence makes a distinction between 
phenogrammatical and tectogrammatical structure; in GF terms expressed as *abstract* 
and *concrete* syntax. 
By allowing multiple concrete syntaxes for a single abstract syntax, GF is a natural 
choice for interlingual translation.

One of the most important contributions of GF is its Resource Grammar Library 
\cite{rglLILT}, which contains 40 languages as of September 2016. All the languages 
share the same core abstract syntax, and each language can have an extra module for 
constructions that are particular to that language. Via the common core, we can 
translate basic syntactic structures between any pair of the 40 languages; any new 
language added to the library will be connected to all of the existing languages.

Typically, writing a morphological description is the first step in starting a resource grammar.
A new grammar writer can easily spend weeks or months in defining inflection classes 
and writing morphophonological transformation rules for their language. 
Such a description in GF may well be valuable on its own right: it may provide 
insights about e.g. morphological complexity \cite{DetrezRanta}, or even lend itself in language description.
However, creating a morphological description in GF is time-consuming, and the result 
is often less efficient than a finite-state description.
If a new RGL language already has morphological resources, using them would ideally
speed up both development and performance of the resource grammar\footnote{Of course, we are not denying anyone their fun of writing a morphological description in GF.}.


## Implementation

We implemented a miniature version (44 functions) of the GF resource grammar \cite{rglLILT} for this work. At this stage, the grammar is not suited for more than toy applications, but the 

The entries in the GF grammar consist of just base lemmas with tags added: for example, the accusative and dative forms of the first person singular pronoun are stored as `én<prn><p1><sg><acc>` and `én<prn><p1><sg><dat>` 
instead of their inflected forms, *engem* and *nekem* respectively.
The user of our grammar types in normal Hungarian words, and the input is analysed 
by the external morphological analyser, which is further disambiguated by a 
Constraint Grammar (CG). Only then is the sentence given to the GF grammar, 
which will return the syntactic parse tree.


## Discussion

Using GF+Apertium+CG in one tool introduces more dependencies, that is, makes the 
system more complex. In comparison, a 100% GF grammar is easier to understand, and
changes in the other systems will not affect it.

Apertium is nice and modular: text is processed through a pipeline, and you can swap a CG for HMM in the disambiguation step and it still works fine. Can we emulate this for our GF+Apertium+CG hybrid?

One of the most unnecessary annoyances in computational linguistics is tagset incompatibility. To demonstrate, here are two parses of the Hungarian sentence *a fa nem szeret engem* 'the tree doesn't love me', in Ocamorph and Apertium:

```
> a
a/ART
> fa
fa/NOUN
> nem
nem/ADV
nem/UTT-INT
> szeret
szeret/VERB
> engem
én/NOUN<PERS<1>><CAS<ACC>>

^a/a<det><def>$ 
^fa/fa<n><sg><nom>$ 
^nem/nem<adv>/nem<ij>$ 
^szeret/szeret<vblex><pri><p3><sg>$ 
^engem/én<prn><pers><p1><mf><sg><acc>$
```

The tags in Ocamorph look scarce, because the tagset defines certain features to be unmarked: e.g. the nominative case and the 3rd person singular verb. In contrast, the Apertium tagset marks with abundance: the tagset is written with translation in mind, and a rule for chunking, disambigation or transfer will need as much information as possible in order to match. 

We have implemented the GF grammar using the Apertium tagset. But how easy would it be to support Ocamorph tagset?
GF offers a construction called a *functor*: parametrised module. In the trivial case, we have two tagsets that make the exact same distinctions, but the tags are just called differently. Then we can refactor our grammar in the following way:

```haskell
interface TagHun = {

 oper

	Tag : Type = Str ;

	det : Tag ;
	def : Tag ;
}

incomplete concrete LangHun of Lang ** open TagHun in {
	
  lin
    the_Det = { s = "a" + det + def } ; 
}

```

Instead of concrete tag strings such as "<det>" and "<def>", we have implemented an *interface* module TagHun, with the declarations of a data type `Tag` and two examples, `det` and `def`. Then we create an *incomplete* concrete syntax, where we open the interface and use the declared data types. In order to finalise the Apertium and Ocamorph versions, we need an instance of `TagHun` and `LangHun` for each. 
Below is the instance of `TagHun` for Apertium:

```haskell
instance TagHunApertium = {

  oper

	det = "<det>" ;
	def = "<def>" ;
}
```

And here for Ocamorph:

```haskell
instance TagHunOcamorph = {

  oper

	det = "/ART" ;
	def = "" ;  -- Feature not included
}
```

With the instances in place, the concrete syntaxes of TagHunApertium and TagHunOcamorph become one-liners:

```haskell
concrete LangHunApertium of Lang ** LangHun with (TagHun=TagHunApertium) ;

concrete LangHunOcamorph of Lang ** LangHun with (TagHun=TagHunOcamorph) ;
```

With this modified solution, we can produce `a<det><def>` from one concrete syntax and `a/ART` from the other. However, we have not implemented this further from an absolute toy example; in reality, we expect to find more challenging incompatibilities from the different tagsets. We may need to add subcategories to the tags, along with their selectional restrictions. The order of the tags varies between tagsets: a simple concatenation as in `"walk" + present + p1 + sg` will not work, thus we will also parametrise the manner of combining different tags. A simplified version is below:

```haskell

interface TagHun = {

 oper

	Tag : Type ;

	det : Tag ;
	def : Tag ;

	combine : [Tag] -> Tag ;
}
```

The function `combine` takes a string and a list of tags, and combines them into a string. It may then be used in the incomplete concrete syntax as follows:

```haskell
incomplete concrete LangHun of Lang ** open TagHun in {
	
  lin
    the_Det = {s = "a" ; tags = combine [det,def] } ; 
}
```

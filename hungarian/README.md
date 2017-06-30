# GF-hun-exttools
GF resources for Hungarian, using external morphological analyser and CG. You need GF and the PGF2 library, lttoolbox and vislcg3 to run this.

## Example usage:

```
$ runghc pgf-shell.hs Miniresource.pgf 
Miniresource> 
```

Here we load a pgf called `Miniresource.pgf`, which is compiled from `MiniresourceHunApe.gf` in the `miniresource/` directory. This grammar uses Apertium tags instead of inflected forms.

Once you have loaded the grammar, you can use the command `pa` to input text in actual Hungarian. 

```
Miniresource> pa egy fa nem szeret engem
```

Then the pgf-shell will call cg-conv, lt-proc and vislcg3 for the input you typed. This is how it looks like after those steps:

```
egy<det><ind> fa<n><sg><nom> nem<adv> szeret<vblex><pri><p3><sg> én<prn><pers><p1><mf><sg><acc>
```

Finally, the string is parsed in the actual GF grammar. Here's the output:

```
(UseCl ? Neg (PredVP (DetCN a_Det (UseN tree_N)) (ComplV2 love_V2 i_NP)),11.597855)
```

----

## Why?


This is mostly a showcase in sharing resources. Instead of writing a morphology in GF, we can reuse an existing finite-state morphology, and only concentrate on the syntax. 

A traditional GF grammar would store an inflection table in the entries for noun and verb, and the function for building sentences would pick the right forms for the subject, verb and object. See an example below:

```haskell
lincat S  = Str ;
lincat NP = { s : Case => Str ; agr : Agr } ;
lincat V2 = { s : Agr => Str ; compl : Case } ;

fun MakeSentence : NP -> V2 -> NP -> S ;
lin MakeSentence subj verb obj = subj.s ! Nom 
							  ++ v2.s ! subj.agr 
							  ++ obj.s ! v2.compl ;
```

The function takes a subject, object and a verb, and produces a sentence. We can assume that NP contains a large inflection table, for all combinations of number and case. The verb, likewise, contains all the conjugation, including tense, mood, person, aspect, and even non-finite forms (e.g. *singing*). 

To store such a lexicon makes a grammar big and slow.
Usually, writing the morphology in GF is the first step in starting a resource grammar. A new grammar writer can easily spend weeks in investigating old grammar books. Even if the language has a finite-state morphology, the representation is very different from the GF morphology, and hardly lends itself for inspiration. Another option is to generate forms to feed in the GF lexicon using an existing morphological analyser---this frees the grammar writer from spending time on elegant morphological rules, but does not solve the problem of grammar blowup.

To contrast, here is a version of the `MakeSentence` function using existing tags. The entries for nouns and verbs do not contain tables this time, but *tags*, represented as simple strings.


```haskell
lincat S  = Str ;
lincat NP = { s : Str ; agr : Str } ;
lincat V2 = { s : Str ; compl : Str } ;

fun MakeSentence : NP -> V2 -> NP -> S ;
lin MakeSentence subj verb obj = subj.s + "<nom>"
							  ++ v2.s + subj.agr 
							  ++ obj.s + v2.compl ;
```

```
Miniresource> gr -cat=S | l -bind
a<det><def> ház<n><sg><nom> nem<adv> szeret<vblex><past><p3><sg> én<prn><pers><p1><mf><sg><acc>
```
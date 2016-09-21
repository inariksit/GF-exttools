# Some thoughts on CG-GF synergy

In the Hungarian GF grammar(s) developed in this repository, we use CG as a preprocessing step for our GF grammar. Consider for a moment the traditional workflow of a GF grammar:

*egy fa nem jár*

Words *egy* and *nem* are ambiguous, so they are also ambiguous in the GF grammar. The user has typed in this particular sentence, and this is what the GF grammar will parse. There is more than one production that has the string "nem" and "egy" as the terminal symbol, and it is all up to the internals of the GF parser how to solve the ambiguity for those two words.

## Exploiting CG for GF

Now, we are in the new style GF grammar, where the terminals are actually called `nem<ij>` and `nem<adv>`. In other words, the GF grammar is not ambiguous anymore. But the words are still ambiguous: the problem has shifted from GF grammar to the morphological analyser. The analyser gives us the following set of cohorts for our simple 4-word sentence:

```
^egy/egy<num>/egy<det><ind>$
^fa/fa<n><sg><nom>$
^nem/nem<ij>/nem<adv>$
^jár/jár<vblex><...>$
```

We cannot just give `egy<num>/egy<det><ind>` to the GF grammar, that would be a parse error. So here we see how the CG comes in handy: if we can reduce the output from the morphological analyser so it contains only one reading per cohort, we can just feed that directly into our GF grammar. It won't be ambiguous, and the rules are simple, so the whole process should be pretty quick. We get a nice GF tree, hurray.


## Exploiting GF for CG

Assume we don't have a CG, or that the one we have is not very good, and it leaves entries ambiguous. Since these readings each are a different string in the GF grammar, we need to do some preprocessing before we give the input to GF: that is, we have to split the cohorts into all combinations of readings. The input to GF would be the following:

```
1) egy<num> fa<n> nem<ij> jár<vblex>
2) egy<num> fa<n> nem<adv> jár<vblex>
3) egy<det> fa<n> nem<ij> jár<vblex>
4) egy<det> fa<n> nem<adv> jár<vblex>
```

(Side note: we see here a disadvantage of GF compared to CG. To disambiguating a single cohort, we must input all candidate combinations. In contrast, CG doesn't have to generate all combinations, it simply operates on a list of options for each cohort.)

Out of these 4 combinations we input to GF, only one will result in a parse: option 4), where *egy* is a determiner and *nem* is an adverb.
We get the nice GF tree again, with some extra work by trying out all combinations. But we also get something more.

### 1) GF can replace manual annotation

We can trivially generate an unambiguous version of an initially ambiguous morphological parse, like the following.

```
"<egy>" 
  "egy" det ind
  ; "egy" num

"<fa>"
  "fa" n sg nom

"<nem>" 
  "nem" adv
  ; "nem" ij

"<jár>"
  "jár" vblex
 ```

 Usually, we need human effort to produce a disambiguated gold standard. This disambiguated data can then be used to induce CG rules. For that purpose, it is important to keep the information of initial ambiguities: otherwise there would be no way to derive what to remove or select in the first place. Based on this single example, we could automatically a number of **possible** rules, such as `REMOVE adv IF (1 vblex)`, or `REMOVE num IF (3 "jár")`, and rank them by how likely they are to be **good** rules.

 As it happens, the first rule is an ok generalisation, but the second rule is very bad; the choice of the POS for *egy* has nothing to do with the word 3 places ahead, and less about its lemma. But with a large number of examples, the rules like the latter would be so infrequent, that they would not get a good score. On the other hand, the negation adverb, which is ambiguous with the negative interjection, appears very often before the verb, and if it is always disambiguated the same way, then this particular rule would seem to do the right thing very often.

(Another side note: the method I have described has been used for >20 years, so this is by no means the main point of this post. I'm just explaining how the method works on a high level.)

So, if we have a GF grammar that includes these tags as entries, then we can trivially construct large amounts of disambiguated sentences, along with their discarded ambiguous readings. This will take a long time: with a sentence of 2 ambiguous words, we needed to give 4 different sentences for GF. For any longer sentence, the combinations become much larger. But we only need to do this once, to create training data for the rule induction algorithm.

## 2) GF can give us ambiguities, and contexts where they are(n't) ambiguous

Look at the first claim: GF can give us ambiguities. So what, big deal, even `lt-expand apertium-<lang>.<lang>.dix | cut -f1 -d ":" | sort | uniq -c` can give us ambiguities. And it doesn't take 6 months to write.

But could we find what is it in the context that disambiguates them? That would be handy for writing CG rules. 

Let's look at the Finnish word form *takaa*. It can be one of the following (and some more too):

* Postposition, "from behind"
* Noun, https://en.wikipedia.org/wiki/Bangladeshi_taka in partitive
* Verb, 'to guarantee' in 3rd person singular indicative present tense


As said, we can detect lexical ambiguity easily. Convert the GF grammar into the low-level PGF format, and look for same thing appearing multiple times on the right-hand side of the rule set. (In general, we can't just grep strings from the high-level GF code, because--hopefully--most strings are not explicitly typed in the grammar, but rather generated by morphological functions.)

Here's a fake PGF grammar. The real thing looks like that, but much more numbers.

```haskell
    F23 := (S18) [from7behind_Prep]
    ...
    F38 := (S17,S19,S18,...) [taka_N]
    ...
    F50 := (S20,S21,S18,...) [guarantee_VS]
```

Look at that suspicious-looking `S18` over there, isn't that just...

```haskell
    S18 := "takaa"
```

So we did the first part. How about the second?

It might be useful to have one concrete syntax with tags and another without tags. We could now jump into the tagged grammar, but modify it for the part of `taka<n><sg><par>`, `takaa<post>`, `taata<vblex><sg><p3><pri>`: replace all of these entries with the string *takaa*. 
Then we are in the situation where we were in the original grammar, except that the other words are tagged now.

I was browsing through papers on GF parser. The following is from [http://www.cse.chalmers.se/~peb/pubs/AngelovLjunglof2014a%20-%20Fast%20Statistical%20Parsing%20with.pdf](Angelov & Ljunglöf 2014):

> The parsing algorithm is basically an extension of
> Earley’s  (1970)  algorithm,  except  that  the  parse
> items in the chart also keep track of the categories
> for the arguments.  In the particular case, the cor-
> responding chart item will be updated to point to
> Conj2 instead of Conj.  This guarantees that only
> *and* will be accepted as a second constituent after
> seeing that the first constituent is *both*.

This quote explains how they manage to accept exactly "both x and y" and "either a or b",
but no other combination of the conjunctions.

Let's think of ambiguities now. We can use the string *takaa* in utterances like *300 takaa* '300 takas'; *NN takaa että <S>* 'NN guarantees that <S>', or *puun takaa* 'from behind the tree' (also 'part of the tree's taka' -- statistically this should rank much lower than the first).

If we just get a hold of the final tree, we see that sure yes, throwing some syntax helps resolve lexical ambiguity! But if we think like a CG grammarian, we don't want all the fancy structure. We just want to know: **after reading which word(s) did the parser know to discard the irrelevant analyses of** *takaa* **?**


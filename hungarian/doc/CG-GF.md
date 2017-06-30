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

Let's think of ambiguities now. We can use the string *takaa* in utterances like *300 takaa* '300 takas'; *NN takaa että S* 'NN guarantees that S', or *puun takaa* 'from behind the tree' (also 'part of the tree's taka' -- statistically this should rank much lower than the first).

If we just get a hold of the final tree, we see that sure yes, throwing some syntax helps resolve lexical ambiguity! But if we think like a CG grammarian, we don't want all the fancy structure. We just want to know: **after reading which word(s) did the parser know to discard the irrelevant analyses of** *takaa* **?**

Or let's be more modest: is there even anything fancy here? Is it always the previous word? 

### Something more practical

Disregard the previous, let's start from a simpler grammar and a simpler formalism. Here's a simple CFG that you can read in Python NLTK, and a very realistic corpus. (Katten dog. It makes sense in Swedish.) Also I have no idea what I'm doing, I'm just thinking aloud on GitHub.

```python
grammar = nltk.CFG.fromstring("""
 S -> NP VP
 PP -> P NP
 NP -> Det N | NP PP | NP N 
 VP -> V NP | VP PP | V
 Det -> 'a' | 'the'
 N -> 'dog' | 'cat'
 V -> 'chased' | 'sat' | 'dog' | 'cat'
 P -> 'on' | 'in'
 """)
 corpus = "the cat dog sat"
 ```

We parse it with Earley parser.

```python
* Processing queue: 0

Predictor Rule:
|>           .           .           .| [0:0] NP -> * Det N
|>           .           .           .| [0:0] NP -> * NP PP
|>           .           .           .| [0:0] NP -> * NP N
Predictor Rule:
|>           .           .           .| [0:0] Det -> * 'the'
```
It's just predicting stuff to start with NP, which is how an S starts. And a NP can start only with so many ways.

Now we actually read *the*. I don't know what is happening here, but the fact that 'cat' appears, suggests that the parser is doing some fancy lookahead.

```haskell
* Processing queue: 1 

Scanner Rule:
|[-----------]           .           .| [0:1] Det -> 'the' *
Completer Rule:
|[----------->           .           .| [0:1] NP -> Det * N
Predictor Rule:
|.           >           .           .| [1:1] N  -> * 'cat'
```

The completer rule tells us that the only possible analysis for the upcoming *cat* is N. We're in the middle of a NP here, give me N and no bullshit.

Time to read the first ambiguous token, *cat*. Seems kind of boring cause we know already what it is.

```java
* Processing queue: 2 

Scanner Rule:
|.           [-----------]           .| [1:2] N  -> 'cat' *
Completer Rule:
|[-----------------------]           .| [0:2] NP -> Det N *
```

So, indeed, after reading places 0:2, we have a NP. But this may not be the full NP. We have actually 3 more completer hypotheses coming up at step 2. 

```java
Completer Rule:
|[----------------------->           .| [0:2] S  -> NP * VP
|[----------------------->           .| [0:2] NP -> NP * PP
|[----------------------->           .| [0:2] NP -> NP * N
```

We may be done with the NP, and looking for a VP next. Or we may be inside a more complex NP, and just waiting to get a PP or an apposition N next. Our parser gives us a whole bunch of predictions how it might analyse the next token. The first and the last make it apparent that the parser actually knows it's going to be *dog*. But it doesn't seem to have made its mind how to parse it: both options are open.

```java
Predictor Rule:
|.           .           >           .| [2:2] N  -> * 'dog'
Predictor Rule:
|.           .           >           .| [2:2] PP -> * P NP
Predictor Rule:
|.           .           >           .| [2:2] VP -> * V NP
|.           .           >           .| [2:2] VP -> * VP PP
|.           .           >           .| [2:2] VP -> * V
Predictor Rule:
|.           .           >           .| [2:2] V  -> * 'dog'
```

Now we can finally read the damn *dog*.


```python
* Processing queue: 3 

Scanner Rule:
|.           .           [-----------]| [2:3] N  -> 'dog' *
|.           .           [-----------]| [2:3] V  -> 'dog' *
```

As hinted at the predictor rules, now the parser is considering both options. First let's see how it can complete the subclause 0:3 if *dog* is a verb:

```python
Completer Rule:
|.         .         [--------->         .| [2:3] VP -> V * NP
|.         .         [---------]         .| [2:3] VP -> V *
Completer Rule:
|[-----------------------------]         .| [0:3] S  -> NP VP *
|.         .         [--------->         .| [2:3] VP -> VP * PP
```

It even found a S, nice. But the sentence is not complete, so it's not very helpful. *Dog* has hope of staying as a verb if the next word can be (the start of) an NP or a PP.

```python
Predictor Rule:
|.         .         .         >         .| [3:3] PP -> * P NP
Predictor Rule:
|.         .         .         >         .| [3:3] NP -> * Det N
|.         .         .         >         .| [3:3] NP -> * NP PP
|.         .         .         >         .| [3:3] NP -> * NP N
```

The predictor rules are saying what I just said in words: continuing the hypothesis that *dog* is a V, we better have some PP or NP next, or I'll stop believing that *dog* is a verb.

And now comes the other hypothesis, what if *dog* is a noun. 

```python
Completer Rule:
|[-----------------------------]         .| [0:3] NP -> NP N *
Completer Rule:
|[----------------------------->         .| [0:3] S  -> NP * VP
|[----------------------------->         .| [0:3] NP -> NP * PP
|[----------------------------->         .| [0:3] NP -> NP * N
```

First completer rule says, "I believe that *the cat dog* is a complete NP, by rule NP -> NP N". The second completer rule says, "Ok I take your word, so here's how your cute little completion stands in the big picture. Don't think too much of yourself just because you got the ] bracket!"

And here come the predictor rules for the hypothesis that *dog* is a N.

```python
Predictor Rule:
|.         .         .         >         .| [3:3] VP -> * V NP
|.         .         .         >         .| [3:3] VP -> * VP PP
|.         .         .         >         .| [3:3] VP -> * V
Predictor Rule:
|.         .         .         >         .| [3:3] V  -> * 'sat'
```

Does something look different? There's an actual terminal! And there was none for the hypothesis that *dog* is V. Uh-oh. Does this little *sat* destroy the hopes for the verb version of *dog*? I can't bear the suspension...

Now we enter the final phase.

```haskell
* Processing queue: 4 

Scanner Rule:
|.         .         .         [---------]| [3:4] V  -> 'sat' *
Completer Rule:
|.         .         .         [--------->| [3:4] VP -> V * NP
|.         .         .         [---------]| [3:4] VP -> V *
Completer Rule:
|[=======================================]| [0:4] S  -> NP VP *
```

Scanner rule, understandably, doesn't include anything else because *sat* isn't ambiguous. The first completer rule is all busy looking into future, and thinking maybe there will be an NP after our SAT. Or there may not be, that's also fine.
Second completer rule senses something special going on: we've got an S! And not just any [----] rule S, but an extra special [=====] S, spanning all the sentence from 0 to 4.

At this point, some predictor rules come late to the party and try to predict some NPs or PPs, but the rest of the rules are already celebrating on the streets. We got an S! With double line! Woooo!
Someone prints out the final solution:

```haskell
(S (NP (NP (Det the) (N cat)) (N dog)) (VP (V sat)))
```

Then everyone gets wasted and trash lots of public property. The end.

I still don't know when does the parser actually decide when to give up on the hypothesis that *dog* is a verb. But assuming that things work like in this piece of fiction, maybe it could be feasible to use that info for deciding which words **syntactically** disambiguate ambiguous words?
# Hungarian
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

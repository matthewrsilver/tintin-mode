### Variable definitions

Some keywords have atypical ways of defining new variables (e.e. pare and loop)
 - loop
 - parse
Other keywords utilize existing variables
 - list

What about the oddities with associative arrays/tables?

### Comments!

How did I only just discover you can comment with /* */

...unfortunately this standard method conflicts with the #no.*; style comment:

```
(modify-syntax-entry ?\/ ". 14" st)
(modify-syntax-entry ?* ". 23" st)
```

so I need to either find way of getting the syntax entries to play nice, or I need to take one of these comments into the font-lock-keywords portion

### Misc
 * enable configuration of # symbol, as tintin does
 * highlight other important "literals" like dice rolls (3d5) and speedwalks (2s3w etc.)
 * clean up regular expressions, figure out word boundaries and #.
 * seems somewhat slow to load
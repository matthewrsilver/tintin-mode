### Variable definition and usage patterns

Some keywords have atypical ways of defining and using variables
 - parse
 - list
 - foreach
 - line (only with the "capture" mode)
 - script

Other oddities:
 - The #class command behaves differently than most others
 - The #delay command can sometimes create a named delay, should consider handling it as a variable
 - The #ignore, #info, #kill, and #message commands highlight a _type_ i.e. actions or aliases
 - The #session command creates a named session
 - the #ticker command has a named entity

Work out a generic tintin token regexp
 - oddities with associative arrays/tables which can include spaces
 - capturing and non-capturing
 - joining with spaces between, and interaction with { and }

### Comments!

How did I only just discover you can comment with /* */

...unfortunately this standard method conflicts with the #no.*; style comment:

```
(modify-syntax-entry ?\/ ". 14" st)
(modify-syntax-entry ?* ". 23" st)
```

so I need to either find way of getting the syntax entries to play nice, or I need to take one of these comments into the font-lock-keywords portion

### Misc
 * variables in e.g. #replace with &0
 * limit %99
 * enable configuration of # symbol, as tintin does
 * clean up regular expressions, figure out word boundaries and special characters {, }, and #.
 * seems somewhat slow to load
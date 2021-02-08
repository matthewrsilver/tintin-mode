### Odd commands that need special handling

 - The #script command can create a variable optionally
 - The #class command behaves differently than most others
 - The #delay command can sometimes create a named delay, should consider handling it as a variable
 - The #ignore, #info, #kill, and #message commands highlight a _type_ i.e. actions or aliases
 - The #session command creates a named session
 - the #ticker command has a named entity

Work out a generic tintin token regexp
 - oddities with associative arrays/tables which can include spaces

### Comments!

How did I only just discover you can comment with /* */

...unfortunately this standard method conflicts with the #no.*; style comment:

```
(modify-syntax-entry ?\/ ". 14" st)
(modify-syntax-entry ?* ". 23" st)
```

so I need to either find way of getting the syntax entries to play nice, or I need to take one of these comments into the font-lock-keywords portion

... also technically a space is required after the o or the p :O
 * Oh! I could highlight #nop?[^ ] to indicate a comment is broken

### Testing

 - Want to continue to expand the test.tt file to check various cases.

### Misc
 * @ symbol breaks tintin args in matching
 * can still highlight 1d${something}
 * issues with variables inside matchers/formatters e.g. %.${value}s in #format
 * enable configuration of # symbol, as tintin does
 * clean up regular expressions, figure out word boundaries and special characters {, }, and #.
 * seems somewhat slow to load
 * variables inside other contexts are highlighted, but braces break things
 * variables that use arrays are not correctly highlighted.
 * spaces, not tabs, are required after table/square braces???
 * use slightly different color for contents of square braces??
 * highlight ;; as it is an error, though ignored sfely when reading script files

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

### Testing

 - Want to continue to expand the test.tt file to check various cases.

### Misc
 * two variables together without a space (${first}$second) second doesn't highlight
 * variable at the start of a line doesn't highlight (though it does on load, initially!?)
 * limit %99
 * @ symbol breaks tintin args in matching
 * can still highlight 1d${something}
 * enable configuration of # symbol, as tintin does
 * clean up regular expressions, figure out word boundaries and special characters {, }, and #.
 * seems somewhat slow to load
 * switch to macros to reduce the number of functions etc necessary? is this even possible?
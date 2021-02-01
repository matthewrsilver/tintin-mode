### Refactor
 - Want to make a major shift in strategy. Using the anchored matcher works in many caes but can be limiting in other cases -- i.e. wherein there are further items that need to be highlighted by the same matcher on the same line:

```
#if {$condition} {#var result true} {#var result false};
```

In this case, the first #var and result get highlighted correctly, but the second #var is highlighted as font-lock-variable-name-face and the variable name itself gets the default face. It's almost as if the anchored matcher continues on but in some poorly understood (by me) way.

The alternative is to pack all of the variable definitions and variable usages, and command subtypes associated with a command, e.g.:

```
#list existing_list add value;
```

into the top-level regex so that all are matched at once, and each group is highlighted once the entire match occurs. This will mean more top-level regexes (i.e. one for `#list x add`, but also `create`, etc.) which could have implications in terms of performance. It also means that things don't get highlighted until the entire match occurs. This is particularly annoying in cases where ww want at least the tintin command to be highlighted when complete. To compensate for this, we can add bare matchers to match the command alone at an earlier stage in the matcher list.

Also part of the refactor -- and in service of the last piece mentioned above -- I'd like to split the functionality for generating the command regex out of the `build-command-matcher` function.

Further, I'd like to switch to macros rather than defining a bunch of specific functions for each type.

### Variable definition and usage patterns

Some keywords have atypical ways of defining and using variables
 - parse
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
 * two variables together without a space (${first}$second) second doesn't highlight
 * limit %99
 * @ symbol breaks tintin args in matching
 * can still highlight 1d${something}
 * enable configuration of # symbol, as tintin does
 * clean up regular expressions, figure out word boundaries and special characters {, }, and #.
 * seems somewhat slow to load

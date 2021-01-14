### Variable definitions

Many keywords define variables that should be highlighted
 - list
 - local
 - math
 - format
 - loop
 - ...i.e. anything that creates/updates a variable provided as first (nth) arg

### Problems with the letter n

When the letter n arrives near a word boundary, it can cause highlighting issues. For example, variable definitions where the first letter is an n are highlighted but without the n. This issue was also affecting the #function command, but a change in regex addressed it.

The issue arises, I think, because n is one of two options for the second character of a two-character comment symbol sequence (i.e. #n). Not quite sure what actions are available to rememdy this.

### Misc
 * only "real" keywords, not everything that starts with #
 * enable configuration of # symbol, as tintin does
 * variable definitions different color than usages?
 * do better on % vars i.e. in format like %-20s and also with %%
 * highlight other important "literals" like dice rolls: 3d5
 * clean up regular expressions, figure out word boundaries and #. Maybe this is syntax table?
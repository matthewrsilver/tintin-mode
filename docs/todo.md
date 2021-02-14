### General
 * Expand documentation to cover the basic competencies
 * Work through issues exposed in warning fonts test
 * Need to go back through the tests now and clean up

 * set font-lock-multiline globally, then use simpler regexps throughout

Work out a generic tintin token regexp
 * oddities with associative arrays/tables which can include spaces

### Commands that need work
 * The #bell command has modes that should be highlighted
 * The #buffer command has modes that should be highlighted

### Comments
 * Switch to using font-lock-multiline?
   * https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiline-Font-Lock.html#Multiline-Font-Lock
 * highlight #nop?[^ ] to indicate a comment is broken

### Misc
 * @ symbol breaks tintin args in matching
 * issues with variables inside matchers/formatters e.g. %.${value}s in #format
 * enable configuration of # symbol and ~ symbol, as tintin does
 * clean up regular expressions, figure out word boundaries and special characters {, }, and #.
 * seems somewhat slow to load
 * variables inside other contexts are highlighted, but braces break things
 * use slightly different color for contents of square braces??
 * highlight ;; as it is an error, though ignored safely when reading script files


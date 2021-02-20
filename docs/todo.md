### General
 * Expand documentation to cover the basic competencies
 * Work through issues exposed in warning fonts test
 * Need to go back through the tests now and clean up

 * Complete migration to simplified class-based approach
   * all functions to methods?
   * which vars can go there, which need to stay
   * clarify approach to quoted symbols
   * maybe add a few more instances to cover common cases
   * don't specify regexes for subcommands, just lists
   * switch the basic elements (e.g. `tintin-argument`) to functions that return the needed info
     and also allow for inheritance and extension in a clean way. Then update the main fontificator
     function so that if it gets a function it just calls it, otherwise it accepts the return?

 * Work out a generic tintin token regexp
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


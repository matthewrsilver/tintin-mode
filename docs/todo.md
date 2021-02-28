### General
 * Expand documentation to cover the basic competencies
 * Work through issues exposed in warning fonts test
 * Need to go back through the tests now and clean up

 * Complete migration to simplified class-based approach
   * don't specify regexes for subcommands, just lists
   * eliminate double quoting

 * Work out a generic tintin token regexp
   * oddities with associative arrays/tables which can include spaces

### Commands that need work
 * The #buffer command's info and find modes need a look

### Comments
 * Comment command stuff doesn't quite work per
   * http://ergoemacs.org/emacs/elisp_comment_command.html

### Misc
 * @ symbol breaks tintin args in matching
 * issues with variables inside matchers/formatters e.g. %.${value}s in #format
 * enable configuration of # symbol and ~ symbol, as tintin does
 * clean up regular expressions, figure out word boundaries and special characters {, }, and #.
 * seems somewhat slow to load
 * variables inside other contexts are highlighted, but braces break things
 * use slightly different color for contents of square braces??
 * highlight ;; as it is an error, though ignored safely when reading script files


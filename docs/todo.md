### General
 - Add example and associated screenshot
 - Expand documentation to cover the basic competencies
 - Add a list of known problems
 - Work through issues exposed in warning fonts test
 - Need to go back through the tests now and clean up

Work out a generic tintin token regexp
 - oddities with associative arrays/tables which can include spaces

### Misc
 * @ symbol breaks tintin args in matching
 * issues with variables inside matchers/formatters e.g. %.${value}s in #format
 * enable configuration of # symbol and ~ symbol, as tintin does
 * clean up regular expressions, figure out word boundaries and special characters {, }, and #.
 * seems somewhat slow to load
 * variables inside other contexts are highlighted, but braces break things
 * spaces, not tabs, are required after table/square braces???
 * use slightly different color for contents of square braces??
 * highlight ;; as it is an error, though ignored safely when reading script files
 * highlight #nop?[^ ] to indicate a comment is broken

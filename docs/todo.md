### General
 - Add example and associated screenshot
 - Expand documentation to cover the basic competencies
 - Add a list of known problems
 - Work through issues exposed in warning fonts test
 - Need to go back through the tests now and clean up

### Odd commands that need special handling

 - The #delay command can sometimes create a named delay, should consider handling it as a variable
 - The #ignore, #info, #kill, and #message commands highlight a _type_ i.e. actions or aliases
 - The #session command creates a named session
 - the #ticker command has a named entity

Work out a generic tintin token regexp
 - oddities with associative arrays/tables which can include spaces

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
 * highlight ;; as it is an error, though ignored safely when reading script files
 * highlight #nop?[^ ] to indicate a comment is broken

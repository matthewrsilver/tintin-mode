### Variable definitions

Many keywords define variables that should be highlighted
 - list
 - local
 - math
 - format
 - loop
 - ...i.e. anything that creates/updates a variable provided as first (nth) arg

What about the oddities with associative arrays/tables?

### Special characters
!   - is for repeating commands
#\d - is for executing something \d times
^\  - is send verbatim if you are connected
~   - captures action text raw with color codes
\.. - escape codes (https://tintin.mudhalla.net/manual/escape_codes.php)

### Comments!

How did I only just discover you can comment with /* */

### Misc
 * enable configuration of # symbol, as tintin does
 * do better on % vars i.e. in format like %-20s and also with %% and %*
 * highlight other important "literals" like dice rolls (3d5) and speewalks (2s3w etc.)
 * clean up regular expressions, figure out word boundaries and #.
 * seems somewhat slow to load
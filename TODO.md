### Boundary issues
This mode highlights tintin commands even when a word character precedes the `#`. For example, the following code gets highlighted with `#alias` as a command, but this is invalid tintin.

```
bad#alias
```

### Case sensitivity issues

Function to generate the prefix-accepting regex that handles #var #vari #varia...

### Variable definitions

Many keywords define variables that should be highlighted
 - list
 - local
 - math
 - format
 - loop
 - ...i.e. anything that creates/updates a variable provided as first (nth) arg

### Misc
 * only "real" keywords, not everything that starts with #
 * enable configuration of # symbol, as tintin does
 * tintin %2 matchers and variables highlighted inside strings but not comments
 * highlight variables after & (i.e. when testing for existence)
 * variable definitions different color than usages
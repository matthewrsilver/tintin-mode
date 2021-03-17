# tintin-mode - Major mode for editing TinTin++ scripts

*Author:* Matthew Silver<br>
*Version:* 1.0.1<br>
*URL:* [https://github.com/matthewrsilver/tintin-mode](https://github.com/matthewrsilver/tintin-mode)<br>

An emacs major mode for editing [TinTin++][1] scripts.

This major mode focuses on highlighting as many aspects of the TinTin++ scripting language as
possible, grouping commands into functional groups and highlighting specific modes that many
commands use to accomplish different tasks. An example highlighted tintin script:

![Example TinTin++ script](docs/sample_tintin_script.png)

## Usage

To use `tintin-mode` add this repo, or the `tintin-mode` directory, inside `.emacs.d` (or
wherever else you prefer) and then update your `.emacs` file to add the mode, e.g.:

```lisp
(add-to-list 'load-path "~/.emacs.d/tintin-mode")
(require 'tintin-mode)
```

## Key Features

### Highlighting of TinTin++ commands

Commands are broken into three categories according to their roles when writing scripts to
automate gameplay and interact with TinTin++:
  * Flow control and variable/function definitions are highlighted as keywords
  * Commands for interacting with TinTin++ itself are highlighted as built-ins
  * Scripting commands and those for game interaction are highlighted separately

Further, commands are case-insensitive and may be abbreviated, so the following commands are
equivalent

![Case-insensitive highlighting with abbreviation.](docs/variable_command_highlighting.png)

For many commands, positional arguments are highlighted based on their function, i.e. as
variable definitions/assignments, variable usages, or options/types. The syntax of each
of the arguments is based on the value of command option arguments, which specify specific
command subtypes. Thus the `create` and `find` options of the `#list` command produce
different highlighting behaviors:

![Highlighting of the #list command based on options.](docs/list_options_highlighting.png)

### Variables and Functions

Variables and function definitions and usages are highlighted under most circumstances. When
TinTin++ variables are surrounded by braces they may contain just about any character. This is
supported in most cases for variable uses (e.g. `${this "works" & just wow!}`) and to a limited
extent in definitions and assignments (where some issues with quotes remain).

### Comments

C-style `/* comments */` and the TinTin++ no-op `#no comment-like thing;` are well-supported.

Unexpected behavior of the comment-like `#nop` command is captured such that failure to
terminate a `#nop` command with a semicolon allows the comment to flow onto subsequent lines.
This is an easily-overlooked behavior of the `#nop` command, and can be instrumental in
identifying misbehaving comments that are unintentionally consuming additional lines.

### Miscellaneous helpful highlights

Support for various syntactical constructs that are used to manipulate text and facilitate
game interaction in TinTin++, including:
  * Regular expressions, formatter specifiers, and pattern matchers
  * Various color and grayscale markers
  * Dice roll and speedwalk strings

## Customization

TinTin++ allows users to customize their scripts in a number of ways. The most critical is the
ability to alter the character used for TinTin++ commands, which is `#` by default. To configure
`tintin-mode` to use a different character (e.g. `/`), set `tintin-command-character` to a
string with the desired character. This can be done by adding to `custom-set-variables`

```lisp
(custom-set-variables '(tintin-command-character "/"))
```

but can also be done safely with `setq` prior to loading/requiring the mode

```lisp
(setq tintin-command-character "/")
(require 'tintin-mode)
```

## Known Issues

* The presence of an `@`, typically used in a function call, can break matching for arguments
  in different tintin commands.
* A number of commands have subcommands toggled by options that should be highlighted
  along with various argument roles as in commands like `#list`.
* Variable definitions do not correctly handle the presence of quotes in braced and unbraced
  contexts.
* The string `"#nop"` is _always_ highlighted as a comment, but in some cases this string may
  be safely incuded within braces as part of an argument to another TinTin++ command. In these
  cases, code is incorrectly highlighted as a comment.
* Comments created with the `#nop` command are not highlighted at all if there is no semicolon
  found before the end of the buffer.

## Origin

This major mode initially derives from one that was originally developed by user [Nephillim][2]
in the TinTin++ forum hosted at sourceforge. It was shared there in an April, 2011 post titled
["Emacs major mode for .tin files"][3]. That post links to a raw [tintin-mode.el][4] file also
at sourceforge. A response to this post indicates that the major mode is available in marmalade,
though I haven't verified this yet.

There doesn't appear to be any license information accompanying this piece of code in the
original post. There is, however, another github repo [sunwayforever/tintin-mode][5] that offers
Nephillim's major mode and links back to the [tintin-mode.el][4] file above as the original. That
repo, further, extends the major mode to include indentation logic and offers the code with an
Apache License 2.0.

I chose to begin a fresh repo leveraging [sunwayforever/tintin-mode][5] as a starting point, and
will continue to use that license until additional information suggests another is appropriate.

Since the original copy, this code has evolved into an original work of its own, significantly
expanding upon the syntax highlighting capabilities of the original. Indentation is still done
using the function provided in [sunwayforever/tintin-mode][5], which is notably not in the
original post.

[1]: https://tintin.mudhalla.net/index.php
[2]: https://tintin.sourceforge.io/forum/memberlist.php?mode=viewprofile&u=887&sid=8b1fd8823d0768fca47317d3961d2ffc
[3]: https://tintin.sourceforge.io/forum/viewtopic.php?t=1447#p5500
[4]: http://dawn-e.users.sourceforge.net/tintin-mode.el
[5]: https://github.com/sunwayforever/tintin-mode


---
Converted from `tintin-mode.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).

# tintin-mode - Major mode for editing TinTin++ scripts

*Author:* Matthew Silver<br>
*Version:* 1.0.1<br>
*URL:* [https://github.com/matthewrsilver/tintin-mode](https://github.com/matthewrsilver/tintin-mode)<br>

An emacs major mode for editing [TinTin++][1] scripts.

## Usage

To use tintin-mode, add tintin-mode.el alone or in a directory inside `.emacs.d` and then update
your `.emacs` file to add the mode:

```lisp
(add-to-list 'load-path "~/.emacs.d/tintin-mode")
(require 'tintin-mode)
```

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

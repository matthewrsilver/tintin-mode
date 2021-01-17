tintin-mode
===========

An emacs major mode for editing [TinTin++][1] scripts.

# Origin

This major mode initially derives from one that was originally developed by user [Nephillim][2] in the TinTin forum hosted at sourceforge and release in an April, 2011 post titled ["Emacs major mode for .tin files"][3]. That post links to a raw [tintin-mode.el][4] file also at sourceforge. A response to this post indicates that that major mode is available in marmalade, though I haven't verified this yet.

There doesn't appear to be any license information accompanying this piece of code in the original post. There is, however, another github repo [sunwayforever/tintin-mode][5] that offers Nephillim's major mode and links back to the [tintin-mode.el][4] file above as the original. That repo, further, extends the major mode to include indentation logic and offers the code with an Apache License 2.0.

I've chosen to begin a fresh repo leveraging [sunwayforever/tintin-mode][5] as a starting point, and as such I will work off of that license until I'm able to learn more about the original author's intentions.

# Usage

To use tintin-mode, add tintin-mode.el alone or in a directory inside `.emacs.d` and then update your `.emacs` file to add the mode:

```lisp
(add-to-list 'load-path "~/.emacs.d/tintin-mode")
(require 'tintin-mode)
```

# Intentions

This major mode works reasonably well, though there are a number of circumstances where syntax highlighting breaks down; the highlighting is too permissive (e.g. highlighting all strings that begin with hashes, as if they were all valid [TinTin++ commands][6]. The indentation is also decent, but could benefit from some attention.

I plan to update this major mode so it is a bit tighter, and works with as many aspects of the TinTin++ scripting language as possible.

[1]: https://tintin.mudhalla.net/index.php
[2]: https://tintin.sourceforge.io/forum/memberlist.php?mode=viewprofile&u=887&sid=8b1fd8823d0768fca47317d3961d2ffc
[3]: https://tintin.sourceforge.io/forum/viewtopic.php?t=1447#p5500
[4]: http://dawn-e.users.sourceforge.net/tintin-mode.el
[5]: https://github.com/sunwayforever/tintin-mode
[6]: https://tintin.mudhalla.net/manual/

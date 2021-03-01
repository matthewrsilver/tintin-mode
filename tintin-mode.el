;;; tintin-mode.el --- Major mode for editing TinTin++ scripts

;; Author: Matthew Silver
;; Keywords: faces, languages
;; Version: 1.0.1
;; URL: https://github.com/matthewrsilver/tintin-mode

;;; Commentary:

;; An emacs major mode for editing [TinTin++][1] scripts.

;; This major mode focuses on highlighting as many aspects of the TinTin++ scripting language as
;; possible, grouping commands into functional groups and highlighting specific modes that many
;; commands use to accomplish different tasks. An example highlighted tintin script:
;;
;; ![Example TinTin++ script](docs/sample_tintin_script.png)

;; Usage:
;;
;; To use tintin-mode add this repo, or the tintin-mode directory, inside `.emacs.d` (or wherever
;; else you prefer) and then update your `.emacs` file to add the mode, e.g.:
;;
;; ```lisp
;; (add-to-list 'load-path "~/.emacs.d/tintin-mode")
;; (require 'tintin-mode)
;; ```

;; Known Issues:
;;
;; * Variables with braces `${x}` can disrupt highlighting of command arguments. For example, when
;;   definig a new variable whose name incorporates the value of that variable
;;
;;   ```
;;   #var {my_${x}} {data};
;;   ```
;;
;;   the non-variable text `my_` should be highlighted as a variable definition but is not. This
;;   issue will in some cases obliterate highlighting of all arguments associated with the command.
;;
;;   A similar issue affects pattern matchers and dice rolls when variables are used within.
;;
;; * A number of commands have subcommands toggled by options that should be highlighted
;;   along with various argument roles as in commands like `#list`.

;; Origin:
;;
;; This major mode initially derives from one that was originally developed by user [Nephillim][2]
;; in the TinTin++ forum hosted at sourceforge. It was shared there in an April, 2011 post titled
;; ["Emacs major mode for .tin files"][3]. That post links to a raw [tintin-mode.el][4] file also
;; at sourceforge. A response to this post indicates that the major mode is available in marmalade,
;; though I haven't verified this yet.
;;
;; There doesn't appear to be any license information accompanying this piece of code in the
;; original post. There is, however, another github repo [sunwayforever/tintin-mode][5] that offers
;; Nephillim's major mode and links back to the [tintin-mode.el][4] file above as the original. That
;; repo, further, extends the major mode to include indentation logic and offers the code with an
;; Apache License 2.0.
;;
;; I chose to begin a fresh repo leveraging [sunwayforever/tintin-mode][5] as a starting point, and
;; will continue to use that license until additional information suggests another is appropriate.
;;
;; Since the original copy, this code has evolved into an original work of its own, significantly
;; expanding upon the syntax highlighting capabilities of the original. Indentation is still done
;; using the function provided in [sunwayforever/tintin-mode][5], which is notably not in the
;; original post.

;; [1]: https://tintin.mudhalla.net/index.php
;; [2]: https://tintin.sourceforge.io/forum/memberlist.php?mode=viewprofile&u=887&sid=8b1fd8823d0768fca47317d3961d2ffc
;; [3]: https://tintin.sourceforge.io/forum/viewtopic.php?t=1447#p5500
;; [4]: http://dawn-e.users.sourceforge.net/tintin-mode.el
;; [5]: https://github.com/sunwayforever/tintin-mode

;;; Code:

(require 'tintin-commands)

(defvar tintin-mode-hook nil)
(defvar tintin-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent) ;placeholder
    map)
  "Keymap for tintin major mode")

(add-to-list 'auto-mode-alist '("\\.tt" . tintin-mode))

(defun optional-braces (rgx &optional capture)
  (let ((capture (or capture t)))
    (concat (if capture "\\(" "\\(?:") rgx "\\|{" rgx "}\\)")))


;;
;; Handle pattern matchers, formatters, regular expressions
(defvar tintin-format-basic "[acdfghlmnprstuwxACDHLMSTUX]")
(defvar tintin-format-numeric "[-+.][0-9]+s")
(defvar tintin-regex-classes "\\(+[0-9]+\\(\\.\\.[0-9]*\\)?\\)?[aAdDpPsSuUwW]")
(defvar tintin-regex-ops (concat "\\(" tintin-regex-classes "\\|[+?.*]\\|[iI]\\)"))
(defvar tintin-regex-ops-wrapped (concat "!?" (optional-braces tintin-regex-ops) ))
(defvar tintin-numeric-capture "[1-9]?[0-9]")
(defvar tintin-captures
  (concat "\\(\\%[\\%\\\\]?\\("
          tintin-format-basic      "\\|"
          tintin-format-numeric    "\\|"
          tintin-regex-ops-wrapped "\\|"
          tintin-numeric-capture   "\\|"
          "\*\\)\\|\\%\\%\\)"))
(defvar tintin-regex-matches "\\(&[1-9]?[0-9]\\)")

;;
;; Handle various simple highlighted faces
(defvar var-prefix "\\([$&*]\\)")
(defvar var-chars "[a-zA-Z_][a-zA-Z0-9_]*")
(defvar var-table "\\(?:\\[.*]\\)?")
(defvar hex-chars "[a-fA-F0-9]")
(defvar tintin-variable (concat "\\(" var-prefix (optional-braces (concat var-chars var-table)) "\\)"))
(defvar tintin-function "\\(@[a-zA-Z_][a-zA-Z0-9_]*\\){")
(defvar ansi-color-code (concat "\\(\<[FB]?" hex-chars "\\{3\\}\>\\)"))
(defvar ansi-gray-code "\\(\<[gG][0-9]\\{2\\}\>\\)")
(defvar tintin-repeat-cmd (concat "\\(" tintin-command-character "[0-9]+\\)\\(?:[\s\t;]\\|$\\)"))
(defvar tintin-special-symbols "\\(^[\!\\]\\|~\\).*")
(defvar double-semicolon-warning ";\\(;\\)")
(defvar default-chars "\\(\\]\\|\\[\\)\\|[{}$]")

;;
;; Deal with comments, which are bonkers in TinTin++
(defvar comment-start-regexp (concat tintin-command-character "[nN][oO][pP]?[ \t\n]+"))
(defvar comment-regexp (concat "\\(" comment-start-regexp "[^;]*?;\\)"))
(defvar insert-comment-str (concat tintin-command-character "nop "))

(defun tintin-comment-extend-region ()
  "Mark multiline constructs for highlighting #no comments"
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found-point (re-search-backward comment-start-regexp nil t)))
      (if found-point
          (progn
            (goto-char font-lock-end)
            (if (re-search-forward ";" nil t)
                (setq font-lock-end (point)))
            (setq font-lock-beg found-point))))))

;;
;; There are a number of different escape codes, all beginning with a `\`
(rx-define basic-escape (any "aefnrtv"))
(rx-define control-char (: "c" (any alphanumeric)))
(rx-define brace-hex-escape (: "x" (or (: "7" (any "BD") ) (= 2 hex))))
(rx-define no-line-feed (: line-end))
(rx-define unicode-16-bit (: "u" (= 4 hex)))
(rx-define unicode-21-bit (: "U" (= 6 hex)))
(defvar tintin-escape-codes (rx (group (: (syntax escape) (or
    no-line-feed basic-escape control-char brace-hex-escape unicode-16-bit unicode-21-bit)))))

(defun escape-code-matcher-func (limit)
  (let ((case-fold-search nil))
    (re-search-forward tintin-escape-codes limit t)))

;;
;; Regular expressions for speedwalks and dice rolls, which are syntactically similar
;; and collide often, so need to be handled together
(rx-define start-marker (or (any "{\s\t") line-start))
(rx-define end-marker (or (any "}\s\t;") line-end))
(rx-define move-direction (any "nsewud"))
(rx-define no-pad-int (or "0" (: (any "1-9") (* (any "0-9")))))
(defvar dice-roll
  (rx (: start-marker
         (group (+ no-pad-int) "d" (+ no-pad-int))
         (not move-direction)
         end-marker)))
(defvar speedwalk
  (rx (: start-marker
         (group (+ (: (+ (any "0-9")) move-direction)))
         end-marker)))

;;
;; Command lists for different classes of TinTin++ commands
(defvar toggle-constant-values '("off" "on"))

(defvar variable-commands-list
  '( "variable" 3   "local" 3      "cat" 0
     "format" 3     "math" 0       "replace" 3))
(defvar unvariable-commands-list '( "unvariable" 5 "unlocal" 5))
(defvar function-command-list '("function" 3))
(defvar unfunction-command-list '("unfunction" 5))
(defvar loop-command-list '("loop" 0))
(defvar parse-foreach-command-list '("parse" 0 "foreach" 0))
(defvar flow-control-command-list
  '( "if" 0         "else" 0       "elseif" 0     "return" 3
     "while" 0      "break" 0      "continue" 4
     "switch" 0     "case" 0       "default" 3))
(defvar mud-command-list
  '(  "action" 3     "alias" 0      "echo" 0       "showme" 4
      "highlight" 4  "substitute" 3 "ticker" 4
      "delay" 3      "cr" 0         "gag" 0
      "tab" 0        "event" 0      "send" 0))
(defvar unmud-command-list
  '( "unaction" 5   "unalias" 0    "unticker" 6
     "ungag" 0      "untab" 0      "unevent" 0))
(defvar script-command-list '("script" 3))
(defvar builtin-command-list
  '( "all" 0        "buffer" 4     "chat" 0       "gts" 0
     "commands" 4   "config" 4     "cursor" 3     "daemon" 3
     "debug" 0      "draw" 0       "edit" 0       "end" 0
     "grep" 0       "help" 0       "history" 4    "run" 0
     "ignore" 3     "info" 3       "kill" 0       "log" 0
     "macro" 3      "map" 0        "mesage" 4     "port" 0
     "path" 0       "pathdir" 5    "prompt" 4     "regexp" 3
     "read" 0       "scan" 1       "screen" 3     "session" 3
     "snoop" 0      "split" 3      "ssl" 0        "detatch" 0
     "textin" 4     "write" 0      "zap" 0        "ats" 0))

;;
;; Special handling for the #bell command and its subcommands
(defvar bell-command-list '("bell" 0))
(defvar bell-ring-options '("ring"))
(defvar bell-volume-options '("volume"))
(defvar bell-toggle-options '("flash" "focus" "margin"))

;;
;; Special handling for the #buffer command and its subcommands
(defvar buffer-command-list '("buffer" 0))
(defvar buffer-get-options '("get"))
(defvar buffer-toggle-options '("lock"))
(defvar buffer-standard-options '("home" "end" "find" "up" "down" "clear" "write" "info"))

;;
;; Special handling for the #line command and its subcommands
(defvar line-command-list '("line" 1))
(defvar line-gag-options '("gag"))
(defvar line-capture-options '("capture"))
(defvar line-standard-options
  '("strip"     "substitute" "background" "convert" "debug"      "ignore"
    "local"     "log"        "logmode"    "msdp"    "multishot"  "oneshot"
    "quiet"     "verbatim"   "verbose"    "logverbatim"))

;;
;; Special handling for the #list command and its subcommands
(defvar list-command-list '("list" 3))
(defvar list-create-options '("create" "tokenize"))
(defvar list-size-options '("size"))
(defvar list-retrieval-options '("find" "get"))
(defvar list-standard-options
  '("add"      "clear"     "collapse"  "delete"    "explode"   "index"     "insert"
    "order"     "shuffle"   "set"       "simplify"  "sort"))

;;
;; Special handling for the #class command and its subcommands
(defvar class-command-list '("class" 2))
(defvar class-use-options '("assign" "list" "save" "write" "clear" "close" "kill"))
(defvar class-create-options '("load" "open" "read"))
(defvar class-size-options '("size"))


;;
;; Now set up the font faces and stand up a few tintin-argument instances that
;; enable us to concisely define many commands using tintin-command.el.

(defface tintin-ansi-face '((t (:foreground "#c95d5d"))) "*Face for ansi color codes.")
(defface tintin-capture-face '((t (:foreground "#8dd110"))) "*Face for capture variables.")
(defface tintin-function-face '((t (:foreground "#5dc9c9"))) "*Face for user functions.")
(defface tintin-command-face '((t (:foreground "#5d74c9"))) "*Face for user hash commands.")
(defface tintin-variable-usage-face '((t (:foreground "#e09d02"))) "*Face for variable usages.")

(setq arg (tintin-argument))
(setq final-arg (clone arg :regexp tintin-final-arg))

(setq var-usage (clone arg :face 'tintin-variable-usage-face :override 'keep))
(setq final-var-usage (clone var-usage :regexp tintin-final-arg))
(setq var-assignment (clone arg :face 'font-lock-variable-name-face :override 'keep))
(setq final-var-assignment (clone var-assignment :regexp tintin-final-arg))

(setq function-name (clone arg :face 'font-lock-function-name-face :override 'keep))
(setq command-type (clone arg :face 'font-lock-type-face))
(setq toggle-value (tintin-argument :vals toggle-constant-values :face 'font-lock-constant-face))

(setq tintin-font-lock-keywords (append

  `(;; Begin building tintin-font-lock-keywords with a list of simple matchers
    ;; Highlight captures in actions, aliases, etc.
    (,tintin-captures . 'tintin-capture-face)
    (,tintin-regex-matches . 'tintin-capture-face)

    ;; Highlight all braces and $, setting to default before anything else can get to them
    ;; This is made necessary by two items below:
    ;;   1. Variables that occur inside a table lookup: $foo[$bar]
    ;;   2. Braces after the variable prefix: ${foo}
    (,default-chars (0 'default keep))

    ;; Highlight variables as they're used. This is done up top and we're explicit
    ;; about the default face of the initial symbol [&$*] so subsequent elements
    ;; in font-lock-keywords can use the `keep` override mode, filling in the
    ;; unhighighted adjacent characters as veriable definitions as necessary, for
    ;; example in this case:
    ;;
    ;;   #var var_name thing;
    ;;   #var {the_$var_name} data;
    ;;
    ;; a variable called `the_thing` is created with value `data` and we'd want
    ;; `the_` to be highlighted as font-lock-variable-name-face but then the
    ;; remaining portion to be highlighted as a variable usage.
    (,tintin-variable
     (2 'default-face keep)
     (3 'tintin-variable-usage-face keep))

    ;; Handle functions as they're used
    (,tintin-function 1 'tintin-function-face))

  ;; Highlight the #list command and its various modes
  (let ((list-command (tintin-command :cmds 'list-command-list))
        (list-create-keyword (tintin-option :vals list-create-options))
        (list-standard-keyword (tintin-option :vals list-standard-options))
        (list-size-keyword (tintin-option :vals list-size-options))
        (list-retrieval-keyword (tintin-option :vals list-retrieval-options)))
    (fontify-tintin-cmd list-command
                        '(var-assignment list-create-keyword)
                        '(var-usage list-standard-keyword)
                        '(var-usage list-size-keyword final-var-assignment)
                        '(var-usage list-retrieval-keyword arg final-var-assignment)))

  ;; Highlight variable defining and deleting commands like #var and #unvar
  (let ((variable-command (tintin-command :cmds 'variable-commands-list)))
    (fontify-tintin-cmd variable-command
                        '(var-assignment)))
  (let ((unvariable-command (tintin-command :cmds 'unvariable-commands-list)))
    (fontify-tintin-cmd unvariable-command
                        '(final-var-usage)))

  ;; Highlight the #class command and its various modes
  (let ((class-command (tintin-command :cmds 'class-command-list))
        (class-use-keyword (tintin-option :vals class-use-options))
        (class-create-keyword (tintin-option :vals class-create-options))
        (class-size-keyword (tintin-option :vals class-size-options)))
    (fontify-tintin-cmd class-command
                        '(var-usage class-use-keyword)
                        '(var-assignment class-create-keyword)
                        '(var-usage class-size-keyword final-var-assignment)))

  ;; Highlight the #function and #unfunction commands
  (let ((function-command (tintin-command :cmds 'function-command-list)))
    (fontify-tintin-cmd function-command
                        '(function-name)))
  (let ((unfunction-command (tintin-command :cmds 'unfunction-command-list)))
    (fontify-tintin-cmd unfunction-command
                        '(final-var-usage)))

  ;; Highlight the #loop command
  (let ((loop-command (tintin-command :cmds 'loop-command-list)))
    (fontify-tintin-cmd loop-command
                        '(arg arg var-assignment)))

  ;; Highlight the #parse and #foreach commands
  (let ((parse-foreach-command (tintin-command :cmds 'parse-foreach-command-list)))
    (fontify-tintin-cmd parse-foreach-command
                        '(arg var-assignment)))

  ;; Highlight flow control commands such as #if and #else
  (let ((flow-control-command (tintin-command :cmds 'flow-control-command-list)))
    (fontify-tintin-cmd flow-control-command))

  ;; Highlight the #line command
  (let ((line-command (tintin-command :cmds 'line-command-list :face ''tintin-command-face))
        (line-standard-keyword (tintin-option :vals line-standard-options))
        (line-gag-keyword (tintin-option :vals line-gag-options))
        (line-capture-keyword (tintin-option :vals line-capture-options)))
    (fontify-tintin-cmd line-command
                        '(line-standard-keyword)
                        '(line-gag-keyword)
                        '(line-capture-keyword var-assignment)))

  ;; Highlight mud scripting commands
  (let ((mud-command (tintin-command :cmds 'mud-command-list :face ''tintin-command-face)))
    (fontify-tintin-cmd mud-command))
  (let ((unmud-command (tintin-command :cmds 'unmud-command-list :face ''tintin-command-face)))
    (fontify-tintin-cmd unmud-command))

  ;; Highlight tintin builtins for working with tintin or setting up sessions
  (let ((built-command (tintin-command :cmds 'builtin-command-list :face ''font-lock-builtin-face)))
    (fontify-tintin-cmd built-command))
  (let ((script-command (tintin-command :cmds 'script-command-list :face ''font-lock-builtin-face)))
    (fontify-tintin-cmd script-command
                        '(var-assignment arg)))

  ;; Highlight #bell command
  (let ((bell-command (tintin-command :cmds 'bell-command-list :face ''font-lock-builtin-face))
        (bell-ring-keyword (tintin-option :vals bell-ring-options))
        (bell-volume-keyword (tintin-option :vals bell-volume-options))
        (bell-toggle-keyword (tintin-option :vals bell-toggle-options))
        (toggle-value (tintin-argument :vals toggle-constant-values :face 'font-lock-constant-face)))
    (fontify-tintin-cmd bell-command
                        '(bell-ring-keyword)
                        '(bell-volume-keyword final-arg)
                        '(bell-toggle-keyword toggle-value)))

  ;; Highlight #buffer command
  (let ((buffer-command (tintin-command :cmds 'buffer-command-list :face ''font-lock-builtin-face))
        (buffer-standard-keyword (tintin-option :vals buffer-standard-options))
        (buffer-get-keyword (tintin-option :vals buffer-get-options))
        (buffer-toggle-keyword (tintin-option :vals buffer-toggle-options)))
    (fontify-tintin-cmd buffer-command
                        '(buffer-standard-keyword)
                        '(buffer-get-keyword var-assignment)
                        '(buffer-toggle-keyword toggle-value)))

  `(;; Continue building tintin-font-lock-keywords with a ist of simpler matchers

    ;; Handle repeat command
    (,tintin-repeat-cmd 1 'tintin-command-face)

    ;; Handle colors.
    (,ansi-color-code . 'tintin-ansi-face)
    (,ansi-gray-code . 'tintin-ansi-face)

    ;; Handle special symbols, speedwalk, and dice rolls
    (,tintin-special-symbols 1 'font-lock-warning-face)
    (,'escape-code-matcher-func 1 'font-lock-warning-face keep)
    (,speedwalk 1 'font-lock-warning-face)
    (,dice-roll 1 'font-lock-warning-face keep)
    (,double-semicolon-warning 1 'font-lock-warning-face)
    (,comment-regexp 1 'font-lock-comment-face t))))

(defvar tintin-mode-syntax-table
  (let ((st (make-syntax-table)))

    (modify-syntax-entry ?_ "w" st)      ; sets underscore to be counted as word
    (modify-syntax-entry ?# "w" st)      ; sets hash to be counted as word

    (modify-syntax-entry ?\/ ". 14" st)  ; support for c-style multiline comments
    (modify-syntax-entry ?* ". 23" st)   ;  which are apparently acceptable!?

    (modify-syntax-entry ?\' "w" st)     ; quotes are super weird in tintin, and are
    (modify-syntax-entry ?\" "w" st)     ;   kind of just normal word characters

    st)
  "Syntax table for tintin-mode")

;;;###autoload
(defun tintin-mode ()
  "Major mode for editing tintin config files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table tintin-mode-syntax-table)
  (use-local-map tintin-mode-map)

  (set (make-local-variable 'tab-width) 4)
  (set (make-local-variable 'indent-line-function) 'tintin-indent-line)

  (set (make-local-variable 'comment-start) insert-comment-str)
  (set (make-local-variable 'comment-start-skip) comment-start-regexp)
  (set (make-local-variable 'comment-end) ";")

  (set (make-local-variable 'font-lock-defaults) '(tintin-font-lock-keywords nil t))
  (make-local-variable 'font-lock-extend-region-functions)
  (add-hook 'font-lock-extend-region-functions 'tintin-comment-extend-region)

  (setq major-mode 'tintin-mode)
  (setq mode-name "TinTin++")
  (run-hooks 'tintin-mode-hook)
)

(defun tintin-indent-line ()
  "Indent current line as TinTin++ code"
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; Check for rule 1
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)

      (if (looking-at "^[ \t]*}[ ]*\\({.+}\\)*;?[ \t]*$"); Check for rule 2
          (progn
            (save-excursion
              (forward-line -1)
              (if (looking-at "^[ \t]*.*?{[ \t]*$")
                  (setq cur-indent (current-indentation))
                  (setq cur-indent (- (current-indentation) tab-width)))
              )
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}[ ]*\\({[0-9\.]+}\\)*;?[ \t]*$") ; Check for rule 3
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
                                        ; Check for rule 4
              (if (looking-at "^[ \t]*.*?{[ \t]*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp) ; Check for rule 5
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(provide 'tintin-mode)


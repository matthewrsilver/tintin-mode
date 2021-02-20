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
;; * Comments are highlighted whenever `#no` is found until the next semicolon, as TinTin++ comment
;;   behavior demands. Currently, this mode does not require a space after `#no` or `#nop`, but if
;;   no space is present TinTin++ attempts to evaluate subsequent text as part of the command. This
;;   issue is due to a limit on the number of characters that can be used in syntax table comments,
;;   but the multi-line requirements make it difficult to use font-locking.
;;
;; * A number of commands, for example `#bell` and `#buffer` have special modes that should be
;;   highlighted as in commands like `#list`.
;;
;;

;; Usage:
;;
;; To use tintin-mode, add tintin-mode.el alone or in a directory inside `.emacs.d` and then update
;; your `.emacs` file to add the mode:
;;
;; ```lisp
;; (add-to-list 'load-path "~/.emacs.d/tintin-mode")
;; (require 'tintin-mode)
;; ```

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

;; Basic regex components
(defvar var-chars "[a-zA-Z_][a-zA-Z0-9_]*")
(defvar var-table "\\(?:\\[.*]\\)?")
(defvar var-prefix "\\([$&*]\\)")
(defvar hex-chars "[a-fA-F0-9]")

;;
;; Handle regular expressions, captures, formatters, etc.
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
(defvar tintin-variable (concat "\\(" var-prefix (optional-braces (concat var-chars var-table)) "\\)"))
(defvar tintin-function "\\(@[a-zA-Z_][a-zA-Z0-9_]*\\){")
(defvar ansi-color-code (concat "\\(\<[FB]?" hex-chars "\\{3\\}\>\\)"))
(defvar ansi-gray-code "\\(\<[gG][0-9]\\{2\\}\>\\)")
(defvar tintin-repeat-cmd "\\(#[0-9]+\\)\\(?:[\s\t;]\\|$\\)")
(defvar tintin-special-symbols "\\(^[\!\\]\\|~\\).*")
(defvar default-chars "\\(\\]\\|\\[\\)\\|[{}$]")

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
(defvar variable-commands-list
  '( "#variable" 3   "#local" 3      "#cat" 0
     "#format" 4     "#math" 0       "#replace" 3))
(defvar unvariable-commands-list '( "#unvariable" 5 "#unlocal" 5))
(defvar function-command-list '("#function" 3))
(defvar unfunction-command-list '("#unfunction" 5))
(defvar loop-command-list '("#loop" 0))
(defvar parse-foreach-command-list '("#parse" 0 "#foreach" 0))
(defvar flow-control-command-list
  '( "#if" 0         "#else" 0       "#elseif" 0     "#return" 3
     "#while" 0      "#break" 0      "#continue" 4
     "#switch" 0     "#case" 0       "#default" 3))
(defvar mud-command-list
  '(  "#action" 3     "#alias" 0      "#echo" 0       "#showme" 4
      "#highlight" 4  "#substitute" 3 "#ticker" 4
      "#delay" 3      "#cr" 0         "#gag" 0
      "#tab" 0        "#event" 0      "#send" 0))
(defvar unmud-command-list
  '( "#unaction" 5   "#unalias" 0    "#unticker" 6
     "#ungag" 0      "#untab" 0      "#unevent" 0))
(defvar script-command-list '("#script" 4))
(defvar builtin-command-list
  '( "#all" 0        "#bell" 0       "#buffer" 4     "#chat" 0
     "#commands" 4   "#config" 4     "#cursor" 3     "#daemon" 3
     "#debug" 0      "#draw" 0       "#edit" 0       "#end" 0
     "#grep" 0       "#help" 0       "#history" 4    "#run" 0
     "#ignore" 3     "#info" 3       "#kill" 0       "#log" 0
     "#macro" 3      "#map" 0        "#mesage" 4     "#port" 0
     "#path" 0       "#pathdir" 5    "#prompt" 4     "#regexp" 3
     "#read" 0       "#scan" 1       "#screen" 3     "#session" 3
     "#snoop" 0      "#split" 3      "#ssl" 0        "#detatch" 0
     "#textin" 4     "#write" 0      "#zap" 0))

;;
;; Special handling for the #line command and its subcommands
(defvar line-command-list '("#line" 1))
(defvar line-standard-regex
  (build-command-arg-regex
   (regexp-opt
    '("strip"     "substitute" "background" "convert" "debug"      "ignore"
      "local"     "log"        "logmode"    "msdp"    "multishot"  "oneshot"
      "quiet"     "verbatim"   "verbose"    "logverbatim")
    t)))
(defvar line-gag-regex
    (build-command-arg-regex (regexp-opt '("gag") t)))
(defvar line-capture-regex
    (build-command-arg-regex (regexp-opt '("capture") t)))

;;
;; Special handling for the #list command and its subcommands
(defvar list-command-list '("#list" 3))
(defvar list-standard-regex
  (build-command-arg-regex
   (regexp-opt
    '("add"      "clear"     "collapse"  "delete"    "explode"   "index"     "insert"
     "order"     "shuffle"   "set"       "simplify"  "sort")
   t)))
(defvar list-create-regex
  (build-command-arg-regex (regexp-opt '("create" "tokenize") t)))
(defvar list-size-regex
  (build-command-arg-regex (regexp-opt '("size") t)))
(defvar list-setvar4-regex
  (build-command-arg-regex (regexp-opt '("find" "get") t)))

;;
;; Special handling for the #class command and its subcommands
(defvar class-command-list '("#class" 2))
(defvar class-use-regex
  (build-command-arg-regex
   (regexp-opt '("assign" "list" "save" "write" "clear" "close" "kill") t)))
(defvar class-create-regex
  (build-command-arg-regex
   (regexp-opt '("load" "open" "read") t)))
(defvar class-size-regex
  (build-command-arg-regex
   (regexp-opt '("size") t)))


;;
;; Now set up the font faces and stand up a few tintin-argument instances that
;; enable us to concisely define many commands using tintin-command.el.

(defface tintin-ansi-face `((t (:foreground ,"#c95d5d"))) "*Face for ansi color codes.")
(defface tintin-capture-face `((t (:foreground ,"#8dd110"))) "*Face for capture variables.")
(defface tintin-function-face `((t (:foreground ,"#5dc9c9"))) "*Face for user functions.")
(defface tintin-command-face `((t (:foreground ,"#5d74c9"))) "*Face for user hash commands.")
(defface tintin-variable-usage-face `((t (:foreground ,"#e09d02"))) "*Face for variable usages.")

(setq arg (tintin-argument))
(setq var-usage (tintin-argument :face 'tintin-variable-usage-face :override 'keep))
(setq final-var-usage (tintin-argument :regexp 'tintin-final-arg :override 'keep :face 'tintin-variable-usage-face))
(setq var-assignment (tintin-argument :face 'font-lock-variable-name-face :override 'keep))
(setq final-var-assignment (tintin-argument :regexp 'tintin-final-arg :override 'keep :face 'font-lock-variable-name-face))
(setq function-name (tintin-argument :face 'font-lock-function-name-face :override 'keep))
(setq command-type (tintin-argument :face 'font-lock-type-face))

(setq tintin-font-lock-keywords
  `(

    ;; Handle captures in actions, aliases, etc.
    (,tintin-captures . 'tintin-capture-face)
    (,tintin-regex-matches . 'tintin-capture-face)

    ;; Handle all braces and $, setting to default before anything else can get to them
    ;; This is made necessary by two items below:
    ;;   1. Variables that occur inside a table lookup: $foo[$bar]
    ;;   2. Braces after the variable prefix: ${foo}
    (,default-chars (0 'default keep))

    ;; Handle variables as they're used. This is done up top and we're explicit
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
    (,tintin-function 1 'tintin-function-face)

    ;; Handle the #list command
    ,(tintin-command-fontificator (tintin-command :cmds 'list-command-list))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'list-command-list)
      var-assignment
      (tintin-subcommand :regexp 'list-create-regex))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'list-command-list)
      var-usage
      (tintin-subcommand :regexp 'list-standard-regex))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'list-command-list)
      var-usage
      (tintin-subcommand :regexp 'list-size-regex)
      final-var-assignment)
    ,(tintin-command-fontificator
      (tintin-command :cmds 'list-command-list)
      var-usage
      (tintin-subcommand :regexp 'list-setvar4-regex)
      arg
      final-var-assignment)

    ;; Handle the variable-defining commands
    ,(tintin-command-fontificator (tintin-command :cmds 'variable-commands-list))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'variable-commands-list)
      var-assignment)
    ,(tintin-command-fontificator (tintin-command :cmds 'unvariable-commands-list))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'unvariable-commands-list)
      final-var-usage)

    ;; Handle the #class command
    ,(tintin-command-fontificator (tintin-command :cmds 'class-command-list))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'class-command-list)
      var-usage
      (tintin-subcommand :regexp 'class-use-regex))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'class-command-list)
      var-assignment
      (tintin-subcommand :regexp 'class-create-regex))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'class-command-list)
      var-usage
      (tintin-subcommand :regexp 'class-size-regex)
      final-var-assignment)

    ;; Handle the #function command
    ,(tintin-command-fontificator (tintin-command :cmds 'function-command-list))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'function-command-list)
      function-name)
    ,(tintin-command-fontificator (tintin-command :cmds 'unfunction-command-list))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'unfunction-command-list)
      final-var-usage)

    ;; Handle the #loop command
    ,(tintin-command-fontificator (tintin-command :cmds 'loop-command-list))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'loop-command-list)
      arg
      arg
      var-assignment)

    ;; Handle the #parse and #foreach commands
    ,(tintin-command-fontificator (tintin-command :cmds 'parse-foreach-command-list))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'parse-foreach-command-list)
      arg
      var-assignment)

    ;; Handle flow control commands:  #if, #while, etc.
    ,(tintin-command-fontificator (tintin-command :cmds 'flow-control-command-list))

    ;; Handle the #line command
    ,(tintin-command-fontificator
      (tintin-command :cmds 'line-command-list :face ''tintin-command-face))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'line-command-list :face ''tintin-command-face)
      (tintin-subcommand :regexp 'line-standard-regex))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'line-command-list :face ''tintin-command-face)
      (tintin-subcommand :regexp 'line-gag-regex))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'line-command-list :face ''tintin-command-face)
      (tintin-subcommand :regexp 'line-capture-regex)
      var-assignment)

    ;; Generic mud scripting commands
    ,(tintin-command-fontificator
      (tintin-command :cmds 'mud-command-list :face ''tintin-command-face))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'unmud-command-list :face ''tintin-command-face))

    ;; Handle tintin builtins for working with tintin or setting up sessions
    ,(tintin-command-fontificator
      (tintin-command :cmds 'builtin-command-list :face ''font-lock-builtin-face))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'script-command-list :face ''font-lock-builtin-face))
    ,(tintin-command-fontificator
      (tintin-command :cmds 'script-command-list :face ''font-lock-builtin-face)
      var-assignment
      arg)

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

    ))

(defvar tintin-mode-syntax-table
  (let ((st (make-syntax-table)))

    (modify-syntax-entry ?_ "w" st)   ; sets underscore to be counted as word
    (modify-syntax-entry ?# "w" st)   ; sets hash to be counted as word

    (modify-syntax-entry ?\' "w" st)  ; quotes are super weird in tintin, and are
    (modify-syntax-entry ?\" "w" st)  ;   kind of just normal word characters

    (modify-syntax-entry ?# ". 1" st) ; sets comments to start with:
    (modify-syntax-entry ?n ". 2" st) ;  `#n` or
    (modify-syntax-entry ?N ". 2" st) ;  `#N`
    (modify-syntax-entry ?\; ">" st)  ; and run until terminated by a semicolon!

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
  (set (make-local-variable 'comment-start) "#nop")
  (set (make-local-variable 'comment-start-skip) "#nop")
  (set (make-local-variable 'font-lock-defaults) '(tintin-font-lock-keywords nil t))
  (set (make-local-variable 'indent-line-function) 'tintin-indent-line)
  (set (make-local-variable 'defun-prompt-regexp) "^#.*")
  (setq major-mode 'tintin-mode)
  (setq mode-name "tintin++")
  (run-hooks 'tintin-mode-hook)
)

(defun tintin-indent-line ()
  "Indent current line as WPDL code"
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


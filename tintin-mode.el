;;; tintin-mode.el --- Mayor mode for editing tintin++ scripts

;; Name: tintin-mode
;; Author: Nephillim <dawn-e@users.sourceforge.net>
;; Version: 1.0.0
;; URL: http://dawn-e.users.sourceforge.net/tintin-mode.el

;;;


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

;; Basic regex components
(defvar var-chars "[a-zA-Z_][a-zA-Z0-9_]*")
(defvar var-table "\\(?:\\[.*]\\)?")
(defvar var-prefix "\\([$&*]\\)")
(defvar hex-chars "[a-fA-F0-9]")
(defvar default-chars "\\(\\]\\|\\[\\)\\|[{}$]")
(defvar brace-or-space "\\(?:[}\s;]\\|$\\)")

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
;; Handle various simple font faces
(defvar tintin-variable (concat "\\(" var-prefix (optional-braces (concat var-chars var-table)) "\\)"))
(defvar tintin-function "\\(@[a-zA-Z_][a-zA-Z0-9_]*\\){")
(defvar ansi-color-code (concat "\\(\<[FB]?" hex-chars "\\{3\\}\>\\)"))
(defvar ansi-gray-code "\\(\<[gG][0-9]\\{2\\}\>\\)")
(defvar tintin-repeat-cmd "\\(#[0-9]+\\)\\(?:[\s\;]\\|$\\)")
(defvar tintin-special-symbols "\\(^[\!\\]\\|~\\).*")

;; There are a number of different escape codes, all beginning with a `\`
(rx-define basic-escape (any "acefnrtv"))
(rx-define brace-hex-escape (: "x" (optional (: "7" (any "BD") ))))
(rx-define no-line-feed (: line-end))
(rx-define unicode-16-bit (: "u" (= 4 hex)))
(rx-define unicode-21-bit (: "U" (= 6 hex)))
(defvar tintin-escape-codes (rx (group (: (syntax escape) (or
    no-line-feed basic-escape brace-hex-escape unicode-16-bit unicode-21-bit)))))

;; Regular expressions for speedwalks and dice rolls, which are syntactically similar
;; and collide often, so need to be handled together
(rx-define move-direction (any "nsewud"))
(rx-define no-pad-int (or "0" (: (any "1-9") (* (any "0-9")))))
(defvar dice-roll (rx (: (+ no-pad-int) "d" (+ no-pad-int) (not move-direction))))
(defvar speedwalk (rx (+ (: (+ (any "0-9")) move-direction))))

;;
;; Provide compact regexes for handling arguments in commands
(defvar capture-chars "[@$&*%a-zA-Z0-9_\"]*")
(defvar tintin-arg (concat "{?\\(" capture-chars var-table "\\)\\(?:[}\s]\\|$\\)"))
(defvar tintin-uncaptured-arg (concat "{?\\(?:" capture-chars var-table "\\)\\(?:[}\s]\\|$\\)"))
(defvar tintin-final-arg (concat "{?\\(" capture-chars var-table "\\)\\(?:[}\s;]\\|$\\)"))
(defvar tintin-space "\\(?:\s+\\)")
(defvar tintin-delimiter "\\(?:\s*\\)")
(defvar tintin-endable "\\(?:\s+;?\\|;\\|$\\)")


;;
;; Functions to build up seach-based fontificators
(defun initial-substrings-helper (word start)
  (cond
   ((> (length word) start)
    (cons word (initial-substrings-helper (substring word 0 -1) start)))
   (t '())))

(defun initial-substrings (word &optional start)
  (unless (> start 1) (setq start (- (length word) 1)))
  (initial-substrings-helper word start))

(defun initial-substrings-list (word-data)
  (cond
   ((> (length word-data) 0)
    (append
     (apply 'initial-substrings (last word-data 2))
     (initial-substrings-list (butlast word-data 2))))
   (t '())))

(defun build-tintin-command-regex (word-data)
  (regexp-opt (initial-substrings-list word-data)))

(defun tintin-command-font-lock-matcher (command &optional args)
  (let ((args (or args tintin-delimiter))
        (case-fold-search t))
    (re-search-forward (concat "\\(" command "\\)" args) limit t)))

(defun build-command-arg-regex (command)
  (concat "{?" command brace-or-space))

;;
;; Tools for highlighting #variable-like commands, where the first
;; argument after the command defines a new variable
(defvar variable-command-regex
  (build-tintin-command-regex
   '( "#variable" 3   "#local" 3      "#cat" 0
      "#format" 4     "#math" 0       "#replace" 3
      )))
(defun bare-variable-command-matcher (limit)
  (tintin-command-font-lock-matcher variable-command-regex tintin-endable))
(defun variable-command-matcher (limit)
  (let ((args-regex (concat tintin-space tintin-arg)))
    (tintin-command-font-lock-matcher variable-command-regex args-regex)))
(defvar unvariable-command-regex
  (build-tintin-command-regex '("#unvariable" 5)))
(defun bare-unvariable-command-matcher (limit)
  (tintin-command-font-lock-matcher unvariable-command-regex tintin-endable))
(defun unvariable-command-matcher (limit)
  (let ((args-regex (concat tintin-space tintin-final-arg)))
    (tintin-command-font-lock-matcher unvariable-command-regex args-regex)))

;;
;; Tools for highlighting the #class command;
(defvar class-command-regex (build-tintin-command-regex '("#class" 2)))
(defun bare-class-command-matcher (limit)
  (tintin-command-font-lock-matcher class-command-regex tintin-endable))
(defvar class-use-regex
  (build-command-arg-regex
   (regexp-opt '("assign" "list" "save" "write" "clear" "close" "kill") t)))
(defun class-use-command-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-arg
          tintin-delimiter class-use-regex
          )))
    (tintin-command-font-lock-matcher class-command-regex args-regex)))
(defvar class-create-regex
  (build-command-arg-regex
   (regexp-opt '("load" "open" "read") t)))
(defun class-create-command-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-arg
          tintin-delimiter class-create-regex
          )))
    (tintin-command-font-lock-matcher class-command-regex args-regex)))
(defvar class-size-regex
  (build-command-arg-regex
   (regexp-opt '("size") t)))
(defun class-size-command-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-arg
          tintin-delimiter class-size-regex
          tintin-delimiter tintin-final-arg
          )))
    (tintin-command-font-lock-matcher class-command-regex args-regex)))

;;
;; Tools for highlighting the #function command, where the first
;; argument after the command defines a new function
(defvar function-command-regex
  (build-tintin-command-regex '("#function" 3)))
(defun bare-function-command-matcher (limit)
  (tintin-command-font-lock-matcher function-command-regex tintin-endable))
(defun function-command-matcher (limit)
  (let ((args-regex (concat tintin-space tintin-arg)))
    (tintin-command-font-lock-matcher function-command-regex args-regex)))
(defvar unfunction-command-regex
  (build-tintin-command-regex '("#unfunction" 5)))
(defun bare-unfunction-command-matcher (limit)
  (tintin-command-font-lock-matcher unfunction-command-regex tintin-endable))
(defun unfunction-command-matcher (limit)
  (let ((args-regex (concat tintin-space tintin-final-arg)))
    (tintin-command-font-lock-matcher unfunction-command-regex args-regex)))

;;
;; Tools for highlighting the #list command and the arguments
;; associated with its various modes
(defvar list-command-regex
  (build-tintin-command-regex '("#list" 3)))
(defun bare-list-command-matcher (limit)
  (tintin-command-font-lock-matcher list-command-regex tintin-endable))
(defvar list-standard-regex
  (concat "{?" (regexp-opt
   '("add"       "clear"     "collapse"  "delete"    "explode"   "index"     "insert"
     "order"     "shuffle"   "set"       "simplify"  "sort"       "tokenize")
   t) "}?" ))
(defun list-standard-mode-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-arg
          tintin-delimiter list-standard-regex)))
    (tintin-command-font-lock-matcher list-command-regex args-regex)))
(defun list-create-mode-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-arg
          tintin-delimiter "{?\\(create\\)}?")))
    (tintin-command-font-lock-matcher list-command-regex args-regex)))
(defun list-size-mode-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-arg
          tintin-delimiter "{?\\(size\\)}?"
          tintin-delimiter tintin-final-arg)))
    (tintin-command-font-lock-matcher list-command-regex args-regex)))
(defvar list-setvar4-regex (concat "{?" (regexp-opt '("find" "get") t) "}?" ))
(defun list-setvar4-mode-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-arg
          tintin-delimiter list-setvar4-regex
          tintin-delimiter tintin-uncaptured-arg
          tintin-delimiter tintin-final-arg)))
    (tintin-command-font-lock-matcher list-command-regex args-regex)))

;;
;; Tools for highlighting the #loop command, a special flow control
;; command that defines a variable for use in the loop
(defvar loop-command-regex
  (build-tintin-command-regex '("#loop" 0)))
(defun bare-loop-command-matcher (limit)
  (tintin-command-font-lock-matcher loop-command-regex tintin-endable))
(defun loop-command-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-uncaptured-arg
          tintin-delimiter tintin-uncaptured-arg
          tintin-delimiter tintin-arg)))
    (tintin-command-font-lock-matcher loop-command-regex args-regex)))

;;
;; Tools for highlighting the #parse and #foreach commands, which are
;; special flow control commands that define a variable for use in the loop
(defvar parse-foreach-command-regex
  (build-tintin-command-regex '("#parse" 0 "#foreach" 0)))
(defun bare-parse-foreach-command-matcher (limit)
  (tintin-command-font-lock-matcher parse-foreach-command-regex tintin-endable))
(defun parse-foreach-command-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-uncaptured-arg
          tintin-delimiter tintin-arg)))
    (tintin-command-font-lock-matcher parse-foreach-command-regex args-regex)))

;;
;; Tools for highlighting remaining flow control commands
(defvar flow-control-command-regex
  (build-tintin-command-regex
   '( "#if" 0         "#else" 0       "#elseif" 0     "#return" 3
      "#while" 0      "#break" 0      "#continue" 4
      "#switch" 0     "#case" 0       "#default" 3
      )))
(defun flow-control-command-matcher (limit)
  (tintin-command-font-lock-matcher flow-control-command-regex tintin-endable))

;;
;; Tools for highlighting the #line command, which has a number of
;; modes and can, in some cases, define variables
(defvar line-command-regex
  (build-tintin-command-regex '("#line" 0)))
(defun bare-line-command-matcher (limit)
  (tintin-command-font-lock-matcher line-command-regex tintin-endable))
(defvar line-standard-regex
  (concat "{?" (regexp-opt
   '("strip"     "substitute" "background" "convert" "debug"      "ignore"
     "local"     "log"        "logmode"    "msdp"    "multishot"  "oneshot"
     "quiet"     "verbatim"   "verbose")
   t) "}?" ))
(defun line-standard-mode-matcher (limit)
  (let ((args-regex (concat tintin-space line-standard-regex)))
    (tintin-command-font-lock-matcher line-command-regex args-regex)))
(defun line-gag-mode-matcher (limit)
  (let ((args-regex (concat tintin-space "{?\\(gag\\)}?" tintin-endable)))
    (tintin-command-font-lock-matcher line-command-regex args-regex)))
(defun line-capture-mode-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space "{?\\(capture\\)}?"
          tintin-delimiter tintin-arg)))
    (tintin-command-font-lock-matcher line-command-regex args-regex)))

;;
;; Tools for highlighting scripting commands
;; TODO come up with a better name because it's confusing with #script command
(defvar scripting-command-regex
  (build-tintin-command-regex
   '( "#action" 3     "#alias" 0      "#echo" 0       "#showme" 4
      "#highlight" 4  "#substitute" 3 "#ticker" 4
      "#delay" 3      "#cr" 0         "#gag" 0
      "#tab" 0        "#event" 0      "#send" 0
      )))
(defun scripting-command-matcher (limit)
  (tintin-command-font-lock-matcher scripting-command-regex tintin-endable))
(defvar unscripting-command-regex
  (build-tintin-command-regex
   '( "#unaction" 5   "#unalias" 0    "#unticker" 6
      "#ungag" 0      "#untab" 0      "#unevent" 0
      )))
(defun unscripting-command-matcher (limit)
  (tintin-command-font-lock-matcher unscripting-command-regex tintin-endable))

;;
;; Tools for highlighting the #script command, which can define a variable sometimes
(defvar script-command-regex (build-tintin-command-regex '("#script" 4)))
(defun bare-script-command-matcher (limit)
  (tintin-command-font-lock-matcher script-command-regex tintin-endable))
(defun script-variable-command-matcher (limit)
  (let ((args-regex
         (concat
          tintin-space tintin-arg
          tintin-delimiter tintin-uncaptured-arg)))
    (tintin-command-font-lock-matcher script-command-regex args-regex)))


;;
;; Tools for highlighting builtin commands
(defvar builtin-command-regex
  (build-tintin-command-regex
   '( "#all" 0        "#bell" 0       "#buffer" 4     "#chat" 0
      "#commands" 4   "#config" 4     "#cursor" 3     "#daemon" 3
      "#debug" 0      "#draw" 0       "#edit" 0       "#end" 0
      "#grep" 0       "#help" 0       "#history" 4    "#run" 0
      "#ignore" 3     "#info" 3       "#kill" 0       "#log" 0
      "#macro" 3      "#map" 0        "#mesage" 4     "#port" 0
      "#path" 0       "#pathdir" 5    "#prompt" 4     "#regexp" 3
      "#read" 0       "#scan" 1       "#screen" 3     "#session" 3
      "#snoop" 0      "#split" 3      "#ssl" 0        "#detatch" 0
      "#textin" 4     "#write" 0      "#zap" 0
      )))
(defun builtin-command-matcher (limit)
  (tintin-command-font-lock-matcher builtin-command-regex tintin-endable))

;;
;; Set a tintin-specific font-lock-keywords variable that can be
;; provided to the major mode at the end
(defface tintin-ansi-face `((t (:foreground ,"#c95d5d"))) "*Face for ansi color codes.")
(defface tintin-capture-face `((t (:foreground ,"#8dd110"))) "*Face for capture variables.")
(defface tintin-function-face `((t (:foreground ,"#5dc9c9"))) "*Face for user functions.")
(defface tintin-command-face `((t (:foreground ,"#5d74c9"))) "*Face for user hash commands.")
(defface tintin-variable-usage-face `((t (:foreground ,"#e09d02"))) "*Face for variable usages.")

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
    (,'bare-list-command-matcher (1 'font-lock-keyword-face))
    (,'list-create-mode-matcher
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face keep)
     (3 'font-lock-type-face))
    (,'list-standard-mode-matcher
     (1 'font-lock-keyword-face)
     (2 'tintin-variable-usage-face keep)
     (3 'font-lock-type-face))
    (,'list-size-mode-matcher
     (1 'font-lock-keyword-face)
     (2 'tintin-variable-usage-face keep)
     (3 'font-lock-type-face)
     (4 'font-lock-variable-name-face keep))
    (,'list-setvar4-mode-matcher
     (1 'font-lock-keyword-face)
     (2 'tintin-variable-usage-face keep)
     (3 'font-lock-type-face)
     (4 'font-lock-variable-name-face keep))

    ;; Handle the variable-defining commands
    (,'bare-variable-command-matcher (1 'font-lock-keyword-face))
    (,'variable-command-matcher
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face keep))
    (,'bare-unvariable-command-matcher (1 'font-lock-keyword-face))
    (,'unvariable-command-matcher
     (1 'font-lock-keyword-face)
     (2 'tintin-variable-usage-face keep))

    ;; Handle the #class command
    (,'bare-class-command-matcher (1 'font-lock-keyword-face))
    (,'class-use-command-matcher
     (1 'font-lock-keyword-face)
     (2 'tintin-variable-usage-face keep)
     (3 'font-lock-type-face))
    (,'class-create-command-matcher
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face keep)
     (3 'font-lock-type-face))
    (,'class-size-command-matcher
     (1 'font-lock-keyword-face)
     (2 'tintin-variable-usage-face keep)
     (3 'font-lock-type-face)
     (4 'font-lock-variable-name-face keep))

    ;; Handle the #function command
    (,'bare-function-command-matcher (1 'font-lock-keyword-face))
    (,'function-command-matcher
     (1 'font-lock-keyword-face)
     (2 'font-lock-function-name-face keep))
    (,'bare-unfunction-command-matcher (1 'font-lock-keyword-face))
    (,'unfunction-command-matcher
     (1 'font-lock-keyword-face)
     (2 'tintin-variable-usage-face keep))

    ;; Handle the #loop command
    (,'bare-loop-command-matcher (1 'font-lock-keyword-face))
    (,'loop-command-matcher
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face keep))

    ;; Handle the #parse and #foreach commands
    (,'bare-parse-foreach-command-matcher (1 'font-lock-keyword-face))
    (,'parse-foreach-command-matcher
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face keep))

    ;; Handle flow control commands:  #if, #while, etc.
    (,'flow-control-command-matcher . 'font-lock-keyword-face)

    (,'bare-line-command-matcher (1 'tintin-command-face))
    (,'line-standard-mode-matcher
     (1 'tintin-command-face)
     (2 'font-lock-type-face))
    (,'line-gag-mode-matcher
     (1 'tintin-command-face)
     (2 'font-lock-type-face))
    (,'line-capture-mode-matcher
     (1 'tintin-command-face)
     (2 'font-lock-type-face)
     (3 'font-lock-variable-name-face keep))

    ;; Generic scripting commands
    (,'scripting-command-matcher . 'tintin-command-face)
    (,'unscripting-command-matcher . 'tintin-command-face)
    (,tintin-repeat-cmd 1 'tintin-command-face)

    ;; Handle tintin builtins for working with tintin or setting up sessions
    (,'builtin-command-matcher . 'font-lock-builtin-face)
    (,'bare-script-command-matcher (1 'font-lock-builtin-face))
    (,'script-variable-command-matcher
     (1 'tintin-command-face)
     (2 'font-lock-variable-name-face))

    ;; Handle colors.
    (,ansi-color-code . 'tintin-ansi-face)
    (,ansi-gray-code . 'tintin-ansi-face)

    ;; Handle special symbols, speedwalk, and dice rolls
    (,tintin-special-symbols 1 'font-lock-warning-face)
    (,tintin-escape-codes 1 'font-lock-warning-face keep)
    (,speedwalk . 'font-lock-warning-face)
    (,dice-roll (0 'font-lock-warning-face keep))

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

    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)

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
  (set (make-local-variable 'font-lock-defaults) '(tintin-font-lock-keywords))
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


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

(let (
      (red "#c95d5d") ;
      (green "#359440") ;
      (yellow "#c3c95d") ;
      (blue "#5d74c9") ;
      (cyan "#5dc9c9") ;
     )

  (defface tintin-ansi-face
    `((t (:foreground ,red)))
    "*Face for ansi color codes."
    :group 'tintin-faces :group 'faces)
  (defface tintin-capture-face
    `((t (:foreground ,"#8dd110")))
    "*Face for capture variables."
    :group 'tintin-faces :group 'faces)
  (defface tintin-function-face
    `((t (:foreground ,cyan)))
    "*Face for user functions."
    :group 'tintin-faces :group 'faces)
  (defface tintin-command-face
    `((t (:foreground ,blue)))
    "*Face for user hash commands."
    :group 'tintin-faces :group 'faces)
  (defface tintin-variable-usage-face
    `((t (:foreground ,"#e09d02")))
    "*Face for user hash commands."
    :group 'tintin-faces :group 'faces)

)
(defvar tintin-format-basic "[acdfghlmnprstuwxACDHLMSTUX]")
(defvar tintin-format-numeric "[-+\.][0-9]+s")
(defvar tintin-captures
  (concat "\\(\\%[\\%\\\\]?"
   "\\("
     tintin-format-basic   "\\|"
     tintin-format-numeric "\\|"
     "[0-9]+"              "\\|"
   "\*\\)\\|\\%\\%\\)"))
(defvar tintin-token "{?\\([a-zA-Z_][a-zA-Z0-9_]*\\)}?[\s\[].*")
(defvar tintin-endable-token "{?\\([a-zA-Z_][a-zA-Z0-9_]*\\)}?[\s\[]?.*\;?")
(defvar tintin-variable "[^\\%]\\([$\&\*]{?\\([a-zA-Z_][a-zA-Z0-9_]*\\)}?\\)")
(defvar tintin-function "\\(@[a-zA-Z_][a-zA-Z0-9_]*\\){")
(defvar ansi-color-code "\\(\<[FB]?[0-9a-fA-F]\\{3\\}\>\\)")
(defvar ansi-gray-code "\\(\<[gG][0-9]\\{2\\}\>\\)")
(defvar tintin-repeat-cmd "\\(#[0-9]*\\)[\s\;]")
(defvar tintin-special-symbols "\\(^[\!\\]\\|~\\).*")
(defvar tintin-escape-codes "\\(\\\\[acefnrtv]\\|\\\\x\\(7[BD]\\)?\\|\\\\$\\)")
(defvar tintin-unicode-escape-codes "\\(\\\\u[a-fA-F0-9]\\{4\\}\\|\\\\U[a-fA-F0-9]\\{6\\}\\)[\s\;]")
(defvar tintin-speedwalk-dice "\\([0-9]+d[0-9]+\\|\\([0-9]+[nsewud]\\)+\\)[\;\}\s\n]")

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

(defun build-command-matcher (word-data)
  (setq command-regex (regexp-opt (initial-substrings-list word-data)))
  (let ((case-fold-search t))
    (re-search-forward (concat command-regex "\\(?:\s\\|$\\|;\\)") limit t)))

;; Define custom matchers
(defun function-command-matcher (limit) (build-command-matcher '("#function" 3)))
(defun list-command-matcher (limit) (build-command-matcher '("#list" 3)))
(defun unvariable-command-matcher (limit) (build-command-matcher '("#unvariable" 5)))
(defun unfunction-command-matcher (limit) (build-command-matcher '("#unfunction" 5)))

(defun unscripting-command-matcher (limit)
  (build-command-matcher
   '( "#unaction" 5   "#unalias" 0    "#unticker" 6
      "#ungag" 0      "#untab" 0      "#unevent" 0
      )))
(defun variable-command-matcher (limit)
  (build-command-matcher
   '( "#variable" 3   "#local" 3
      "#cat" 0        "#format" 4     "#math" 0       "#replace" 3
      "#class" 0
      )))
(defun statement-command-matcher (limit)
  (build-command-matcher
   '( "#if" 0         "#else" 0       "#elseif" 0
      "#foreach" 0    "#loop" 0       "#while" 0      "#parse" 0
      "#break" 0      "#continue" 4   "#return" 3
      "#switch" 0     "#case" 0       "#default" 3
      )))
(defun scripting-command-matcher (limit)
  (build-command-matcher
   '( "#action" 3     "#alias" 0      "#echo" 0       "#showme" 4
      "#highlight" 4  "#substitute" 3 "#ticker" 4     "#gag" 0
      "#delay" 3      "#line" 0       "#cr" 0
      "#tab" 0        "#event" 0      "#send" 0
     )))
(defun builtin-command-matcher (limit)
  (build-command-matcher
   '( "#all" 0        "#bell" 0       "#buffer" 4     "#chat" 0
      "#commands" 4   "#config" 4     "#cursor" 3     "#daemon" 3
      "#debug" 0      "#draw" 0       "#edit" 0       "#end" 0
      "#grep" 0       "#help" 0       "#history" 4    "#run" 0
      "#ignore" 3     "#info" 3       "#kill" 0       "#log" 0
      "#macro" 3      "#map" 0        "#mesage" 4     "#port" 0
      "#path" 0       "#pathdir" 5    "#prompt" 4     "#regexp" 3
      "#read" 0       "#scan" 1       "#screen" 3     "#script" 5
      "#session" 3    "#snoop" 0      "#split" 3      "#ssl" 0
      "#detatch" 0    "#textin" 4     "#write" 0      "#zap" 0
      )))

(setq tintin-font-lock-keywords
  `(
    ;; Handle captures in actions, aliases, etc.
    (,tintin-captures . 'tintin-capture-face)

    ;; Handle the variable-defining commands
    (,'list-command-matcher
     (0 'font-lock-keyword-face)
     ;; Capture only the first argument (with or without braces) as a variable name
     (,tintin-token nil nil (1 'font-lock-variable-name-face))
     ;; TODO handle named actions of the list command
     )

    (,'variable-command-matcher
     (0 'font-lock-keyword-face)
     ;; Capture only the first argument (with or without braces) as a variable name
     (,tintin-token nil nil (1 'font-lock-variable-name-face)))

    (,'unvariable-command-matcher
     (0 'font-lock-keyword-face)
     ;; Capture only the first argument (with or without braces) as a variable name
     (,tintin-endable-token nil nil (1 'tintin-variable-usage-face)))

    ;; Handle variables as they're used
    (,tintin-variable 2 'tintin-variable-usage-face)

    ;; User the #function command
    (,'function-command-matcher
      (0 'font-lock-keyword-face)
      ;; Capture the first value after a function definition as the function name
      (,tintin-token nil nil (1 'font-lock-function-name-face)))

    (,'unfunction-command-matcher
     (0 'font-lock-keyword-face)
     ;; Capture only the first argument (with or without braces) as a variable name
     (,tintin-endable-token nil nil (1 'tintin-variable-usage-face)))

    ;; Handle functions as they're used
    (,tintin-function 1 'tintin-function-face)

    ;; Handle flow control, called statelents in tintin:  #if, #while, etc.
    (,'statement-command-matcher . 'font-lock-keyword-face)

    ;; Scripting commands for interacting with MUD
    (,'scripting-command-matcher . 'tintin-command-face)
    (,'unscripting-command-matcher . 'tintin-command-face)
    (,tintin-repeat-cmd 1 'tintin-command-face)

    ;; Handle tintin builtins for working with tintin or setting up sessions
    (,'builtin-command-matcher . 'font-lock-builtin-face)

    ;; Handle colors.
    (,ansi-color-code . 'tintin-ansi-face)
    (,ansi-gray-code . 'tintin-ansi-face)

    ;; Handle special symbols, speedwalk, and dice rolls
    (,tintin-special-symbols 1 'font-lock-warning-face)
    (,tintin-escape-codes 1 'font-lock-warning-face)
    (,tintin-unicode-escape-codes 1 'font-lock-warning-face)
    (,tintin-speedwalk-dice 1 'font-lock-warning-face)

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


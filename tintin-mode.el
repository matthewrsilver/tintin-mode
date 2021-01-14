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
    `((t (:foreground ,yellow)))
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

)


(defvar tintin-captures "\\(\\%\\([a-zA-Z][a-zA-Z0-9]*\\|[0-9]*\\)\\)")
(defvar tintin-token "\\([a-zA-Z_]\\w*\\)[\s\[].*")
(defvar tintin-variable "\\([$\&]{?\\([a-zA-Z_]\\w*\\)}?\\)")
(defvar tintin-function "\\(@[a-zA-Z_]\\w*\\){.*")
(defvar ansi-color-code "\\(\<[FB]?[0-9a-fA-F]\\{3\\}\>\\)")
(defvar ansi-gray-code "\\(\<[gG][0-9]\\{2\\}\>\\)")
(defvar tintin-cmd-prior "\\(?:\s\\|^\\|{\\)")

(defun initial-substrings (word &optional start)
  (unless start (setq start 0))
  (cond
   ((> (length word) start)
    (cons word (initial-substrings (substring word 0 -1) start)))
   (t '())
   ))

(defun initial-substrings-list (word-data)
  (cond
   ((> (length word-data) 0)
    (append
     (apply 'initial-substrings (last word-data 2))
     (initial-substrings-list (butlast word-data 2))))
   (t '())
   ))

(defun build-command-matcher (word-data)
  (setq command-regex (regexp-opt (initial-substrings-list word-data)))
  (let ((case-fold-search t))
    (re-search-forward (concat tintin-cmd-prior command-regex "\\(?:\s\\|$\\)")
                       limit t)))

;; Define custom matchers
(defun variable-command-matcher (limit) (build-command-matcher '("#variable" 3)))
(defun function-command-matcher (limit) (build-command-matcher '("#function" 3)))
(defun statement-command-matcher (limit)
  (build-command-matcher
   '( "#if" 0         "#else" 0       "#elseif" 0
      "#foreach" 0    "#loop" 0       "#while" 0      "#parse" 0
      "#break" 0      "#continue" 4   "#return" 3
      "#switch" 0     "#case" 0       "#default" 3
      )))

(setq tintin-font-lock-keywords
  `(
    ;; Handle captures in actions, aliases, etc.
    (,tintin-captures . 'tintin-capture-face)

    ;; Handle the #variable command
    (,'variable-command-matcher
     (0 'font-lock-keyword-face)
     ;; Capture only the first argument (with or without braces) as a variable name
     (,tintin-token nil nil (1 'font-lock-variable-name-face)))

    ;; Handle variables as they're used
    (,tintin-variable 2 'font-lock-variable-name-face)

    ;; User the #function command
    (,'function-command-matcher
      (0 'font-lock-keyword-face)
      ;; Capture the first value after a function definition as the function name
      (,tintin-token nil nil (1 'font-lock-function-name-face)))

    ;; Handle functions as they're used
    (,tintin-function 1 'tintin-function-face)

    ;; Handle flow control, called statelents in tintin:  #if, #while, etc.
    (,'statement-command-matcher . 'font-lock-keyword-face)

    ;; Handle colors.
    (,ansi-color-code . 'tintin-ansi-face)
    (,ansi-gray-code . 'tintin-ansi-face)

    ;; All possible '#' commands, even '#EOUOEU'. Yes, I'm lazy.
    (,(concat tintin-cmd-prior "\\(#[a-zA-Z0-9]*\\)") . 'tintin-command-face)
    ))

(defvar tintin-mode-syntax-table
  (let ((st (make-syntax-table)))

    (modify-syntax-entry ?_ "w" st)   ; sets underscore to be counted as one_word

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


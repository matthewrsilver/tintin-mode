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

(let (
      (red "#c95d5d") ;
      (green "#359440") ;
      (yellow "#c3c95d") ;
      (off-yellow "#c3b95d") ;
      (orange "#ef6d22")
      (blue "#5d74c9") ;
      (cyan "#5dc9c9") ;
      (off-cyan "#5db9c9") ;
      (white "#ffffff") ;
      (purple "#a95dc9") ;
     )

  (defface tintin-ansi-face
    `((t (:foreground ,red)))
    "*Face for ansi color codes."
    :group 'tintin-faces :group 'faces)
  (defface tintin-symbol-face
    `((t (:foreground ,white)))
    "*Face for symbols."
    :group 'tintin-faces :group 'faces)
  (defface tintin-var-face
    `((t (:foreground ,yellow)))
    "*Face for variables."
    :group 'tintin-faces :group 'faces)
  (defface tintin-var-def-face
    `((t (:foreground ,off-yellow)))
    "*Face for variable definitions."
    :group 'tintin-faces :group 'faces)
  (defface tintin-conditional-face
    `((t (:foreground ,purple)))
    "*Face for #Ifs, #Loops."
    :group 'tintin-faces :group 'faces)
  (defface tintin-function-face
    `((t (:foreground ,cyan)))
    "*Face for user functions."
    :group 'tintin-faces :group 'faces)
  (defface tintin-function-def-face
    `((t (:foreground ,off-cyan)))
    "*Face for user function definitions."
    :group 'tintin-faces :group 'faces)
  (defface tintin-hash-face
    `((t (:foreground ,blue)))
    "*Face for user hash commands."
    :group 'tintin-faces :group 'faces)

)

; helps make the messy optimized regexps
(regexp-opt '(
"#v" "#V"
) t)

(defconst tintin-font-lock-keywords
  (list
   ;; Vars.
   '("\\(\%\\([a-zA-Z][a-zA-Z0-9]*\\|[0-9]*\\)\\)" . 'tintin-var-face)
   ;; More Vars
   '("\\($[a-zA-Z_][][a-zA-Z0-9_]*\\)" . 'tintin-var-face)
   ;; and more, the #VAR now
   '("\\(#\\(?:V\\(?:AR\\(?:I\\(?:A\\(?:B\\(?:LE?\\)?\\)?\\)?\\)?\\|ar\\(?:i\\(?:a\\(?:b\\(?:le?\\)?\\)?\\)?\\)?\\)\\|var\\(?:i\\(?:a\\(?:b\\(?:le?\\)?\\)?\\)?\\)?\\)\\) *\\([a-zA-Z_][][a-zA-Z0-9_-]*\\|{ *[a-zA-Z_][][ a-zA-Z0-9_-]*}\\)\\([ \t\n]\\| .?\\)" . 'tintin-var-def-face)
   ;; User functions. If the function args have quotes it fails... WTF is with the quotes anyway???
   '("\\(@[a-zA-Z_][][a-zA-Z0-9_]*\\) *{[^}]*}" . 'tintin-function-face)
   ;; User functions, definitions.
   '("\\(#\\(?:F\\(?:UN\\(?:C\\(?:T\\(?:I\\(?:ON?\\)?\\)?\\)?\\)?\\|un\\(?:c\\(?:t\\(?:i\\(?:on?\\)?\\)?\\)?\\)?\\)\\|fun\\(?:c\\(?:t\\(?:i\\(?:on?\\)?\\)?\\)?\\)?\\)\\) *\\([a-zA-Z_][][a-zA-Z0-9_-]*\\|{ *[a-zA-Z_][][ a-zA-Z0-9_-]*}\\)\\([ \t\n]\\| .?\\)" . 'tintin-function-def-face)
   ;; #IF #IFELSE #ELSE and #LOOP.
   '("\\(#\\(?:E\\(?:LSE\\(?:IF\\)?\\|lse\\(?:[Ii]f\\)?\\)\\|I[Ff]\\|L\\(?:OOP\\|oop\\)\\|else\\(?:if\\)?\\|if\\|loop\\)\\)[ \t\n]" . 'tintin-conditional-face)
   ;; Colours.
   '("\\(\<[0-9]*\>\\)" . 'tintin-ansi-face)
   ;; Curly brackets etc.
   '("\\([][(){};\+\*\-\/]\\)" . 'tintin-symbol-face)
   ;; All possible '#' commands, even '#EOUOEU'. Yes, I'm lazy.
   ;; '("\\(#[a-zA-Z0-9]*\\)[ \t\n]" . 'tintin-hash-face)
   '("\\(#[a-zA-Z0-9]*\\)" . 'tintin-hash-face)
  )
  "Default highlighting for tintin mode")

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
;;; tintin-mode.el ends here

(require 'cl-lib)
(require 'eieio)

;;
;; Provide compact regexes for handling arguments in commands
(defvar capture-chars "[@$&*%a-zA-Z0-9_\"]+")
(defvar var-table "\\(?:\\[.*]\\)?")  ;; TODO: this is in both....
(defvar tintin-arg (concat "{?\\(" capture-chars var-table "\\)\\(?:[}\s\t]\\|$\\)"))
(defvar tintin-final-arg (concat "{?\\(" capture-chars var-table "\\)\\(?:[}\s\t;]\\|$\\)"))
(defvar tintin-space "\\(?:[\s\t]+\\)")
(defvar tintin-delimiter "\\(?:[\s\t]*\\)")
(defvar tintin-endable "\\(?:[\s\t]+;?\\|;\\|$\\)")

;;
;; A few generic useful functions
(defun join (sep lst)
  (mapconcat 'identity lst sep))

(defun build-command-arg-regex (command)
  (let ((brace-or-space "\\(?:[}\s\t;]\\|$\\)"))
    (concat "{?" command brace-or-space)))

(defclass tintin-command ()
  ((cmds :initarg :cmds)
   (face :initarg :face :initform 'font-lock-keyword-face))
  "Base class for standard keyword faces")

(defclass tintin-argument ()
  ((regexp :initarg :regexp :initform tintin-arg)
   (face :initarg :face :initform nil)
   (override :initarg :override :initform nil))
  "Base class that represents an unhighlighted, generic TinTin++ argument.")

(defclass tintin-subcommand (tintin-argument)
  ((face :initform 'font-lock-type-face))
  "Command subtype argument class, that represents a subtype controlling a TinTin++ command.")

;;
;; Functions to build up seach-based fontificators
(defun initial-substrings-helper (word start)
  (if (> (length word) start)
      (cons word (initial-substrings-helper (substring word 0 -1) start))))

(defun initial-substrings (word &optional start)
  (let ((start (if (> start 0) start (- (length word) 1))))
    (initial-substrings-helper word start)))

(defun initial-substrings-list (word-data)
  (if (> (length word-data) 0)
    (append
     (apply 'initial-substrings (last word-data 2))
     (initial-substrings-list (butlast word-data 2)))))

(defun build-tintin-command-regex (word-data)
  (concat "\\(" (regexp-opt (initial-substrings-list word-data)) "\\)"))

(defun argspec-to-regexp (argspec)
  (symbol-value (slot-value argspec :regexp)))

(defun argspec-to-highlighter (argspec idx)
  (let ((face (slot-value argspec :face))
        (override (slot-value argspec :override)))
    (if face `(,idx ',face ,override))))

(defun make-highlighters (highlighter-list)
  (let* ((n (- (length highlighter-list) 1))
         (indices (number-sequence 2 (+ n 2))))
    (remove nil (cl-mapcar #'argspec-to-highlighter highlighter-list indices))))

(defun post-command-regexp (args-regexp)
  (if (eq "" args-regexp) tintin-endable (concat tintin-space args-regexp)))

(defun fontify-tintin-subcommand (&optional arguments)
  (let* ((arg-values (if arguments (mapcar #'eval arguments) '()))
         (args-regexp-list (mapcar #'argspec-to-regexp arg-values))
         (args-regexp (join tintin-delimiter args-regexp-list))
         (cmd-subtype-regexp (concat command-regexp (post-command-regexp args-regexp)))
         (highlighters (make-highlighters arg-values)))
    (append (list cmd-subtype-regexp `(1 ,command-face)) highlighters)))

(defun fontify-tintin-cmd (command-spec &rest subcommands)
  (let* ((command-face (slot-value command-spec :face))
         (command-list (symbol-value (slot-value command-spec :cmds)))
         (command-regexp (build-tintin-command-regex command-list))
         (base-and-subcommands (append '(nil) subcommands)))
    (mapcar #'fontify-tintin-subcommand base-and-subcommands)))

(provide 'tintin-commands)
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

(defun optional-braces (rgx &optional capture)
  (let ((capture (or capture t)))
    (concat (if capture "\\(" "\\(?:") rgx "\\|{" rgx "}\\)")))

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
  (cond
   ((> (length word) start)
    (cons word (initial-substrings-helper (substring word 0 -1) start)))
   (t '())))

(defun initial-substrings (word &optional start)
  (unless (> start 0) (setq start (- (length word) 1)))
  (initial-substrings-helper word start))

(defun initial-substrings-list (word-data)
  (cond
   ((> (length word-data) 0)
    (append
     (apply 'initial-substrings (last word-data 2))
     (initial-substrings-list (butlast word-data 2))))
   (t '())))

(defun build-tintin-command-regex (word-data)
  (concat "\\(" (regexp-opt (initial-substrings-list word-data)) "\\)"))

(defun argspec-to-regexp (argspec)
  (symbol-value (slot-value argspec :regexp)))

(defun argspec-to-highlighter (argspec idx)
  (let ((face (slot-value argspec :face))
        (override (slot-value argspec :override)))
    (cond
     ((eq nil face) nil)
     (t `(,idx ',face ,override)))))

(defun make-highlighters (highlighter-list)
  (let* ((n (- (length highlighter-list) 1))
        (indices (number-sequence 0 n))
        )
    (remove nil
            (mapcar
             (lambda (i)
               (argspec-to-highlighter (nth i highlighter-list) (+ i 2)))
             indices))))

(defun tintin-command-fontificator (command-spec &rest arguments)
  (let* ((command-list (symbol-value (slot-value command-spec :cmds)))
         (command-face (slot-value command-spec :face))
         (command-regexp (build-tintin-command-regex command-list)) ;; TODO: this becomes a method
         (args-regexp-list (mapcar 'argspec-to-regexp arguments))
         (args-regexp (join tintin-delimiter args-regexp-list))
         (post-cmd-regexp (if (eq "" args-regexp) tintin-endable (concat tintin-space args-regexp)))
         (cmd-subtype-regexp (concat command-regexp post-cmd-regexp))
         (highlighters (make-highlighters arguments))
         )
    (append (list cmd-subtype-regexp `(1 ,command-face)) highlighters)))

(provide 'tintin-commands)
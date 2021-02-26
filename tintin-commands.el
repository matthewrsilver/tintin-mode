(require 'cl-lib)
(require 'eieio)

;;
;; A few generic useful functions
(defun join (sep lst)
  "Join strings in LST with SEP between each, returning the result."
  (mapconcat 'identity lst sep))

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
;; Classes used to define commands, subcommands, and the arguments therein
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
(defun initial-substrings (word &optional min-len)
  "Given string WORD return a list of all initial substrings.
Each substring starts with the first character with successive substrings
decreasing in length. The optional MIN-LEN argument is an integer that
designates minimum length substring to generate; a value of 0 indicates that
MIN-LEN should be set to the length of WORD, i.e. that only one substring,
WORD itself, should be returned."
  (let ((min-len (if (= min-len 0) (length word) min-len)))
    (if (>= (length word) min-len)
        (cons word (initial-substrings (substring word 0 -1) min-len)))))

(defun initial-substrings-list (word-data)
  "Return a list of initial substrings for all words in WORD-DATA.
Pairs of elements comprise WORD-DATA; the first character element is a string
with a word whose initial substrings should be computed, and the second is
an integer describing the minimum length substring to be generated, per
`initial-substrings'. Successive pairs describe new words and minimum lengths.
All lists of initial substrings are appended together to create a single list
for all words in WORD-DATA."
  (if (> (length word-data) 0)
    (append (apply 'initial-substrings (last word-data 2))
            (initial-substrings-list (butlast word-data 2)))))

(defun build-tintin-command-regexp (word-data)
  "Return a regular expression that matches words in WORD-DATA.
Pairs of elements comprise WORD-DATA; the first character element is a string
with a word whose initial substrings should be computed, and the second is
an integer describing the minimum length substring to be generated, per
`initial-substrings'. Successive pairs describe new words and minimum lengths.

   '(\"#sample\" 5 \"#another\" 0)

Describes two words, '#sample' and '#another' for which initial substrings
should be generated:

    '#sample' '#sampl' '#samp' '#another'

An efficient regular expression that matches these words is then produced by
`regexp-opt', wrapped in paretheses to create a capture group, and returned."
  (concat "\\(" (regexp-opt (initial-substrings-list word-data)) "\\)"))

(defun argument-to-regexp (argument)
  "Retrieve the regular expression associated with ARGUMENT."
  (symbol-value (slot-value argument :regexp)))

(defun argument-to-highlighter (argument idx)
  "Produce a subexp-highlighter for ARGUMENT in capture group IDX.
This function retrieves information from a `tintin-argument' necessary to
generate a subexp-highlighter describing how a capture group in an earlier
matcher should he highlighted, per search-based fontification. Returns nil
if the value of the :face slot has not been populated."
  (let ((face (slot-value argument :face))
        (override (slot-value argument :override)))
    (if face `(,idx ',face ,override))))

(defun make-highlighters (highlighter-list)
  (let* ((n (- (length highlighter-list) 1))
         (indices (number-sequence 2 (+ n 2))))
    (remove nil (cl-mapcar #'argument-to-highlighter highlighter-list indices))))

(defun post-command-regexp (args-regexp)
  (if (eq "" args-regexp) tintin-endable (concat tintin-space args-regexp)))

(defun fontify-tintin-subcommand (&optional arguments)
  (let* ((arg-values (if arguments (mapcar #'eval arguments) '()))
         (args-regexp-list (mapcar #'argument-to-regexp arg-values))
         (args-regexp (join tintin-delimiter args-regexp-list))
         (cmd-subtype-regexp (concat command-regexp (post-command-regexp args-regexp)))
         (highlighters (make-highlighters arg-values)))
    (append (list cmd-subtype-regexp `(1 ,command-face)) highlighters)))

(defun fontify-tintin-cmd (command-spec &rest subcommands)
  (let* ((command-face (slot-value command-spec :face))
         (command-list (symbol-value (slot-value command-spec :cmds)))
         (command-regexp (build-tintin-command-regexp command-list))
         (base-and-subcommands (append '(nil) subcommands)))
    (mapcar #'fontify-tintin-subcommand base-and-subcommands)))

(provide 'tintin-commands)
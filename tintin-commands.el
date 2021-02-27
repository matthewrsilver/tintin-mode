(require 'cl-lib)
(require 'eieio)

;;
;; A few generic useful functions
(defun string-join (lst sep)
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
;; Utilities to support regexp generation for tintin-command instances
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
Pairs of elements comprise WORD-DATA; the first element is a string with a word
whose initial substrings should be computed, and the second is an integer that
defines the minimum length substring to be generated, per `initial-substrings'.
Successive pairs describe new words and minimum lengths. This function produces
an optimized regular expression that matches the words accordingly.

For example, the following progression from WORD-DATA to words to regexp:

   '(\"#sample\" 5 \"#another\" 0)
    '#sample' '#sampl' '#samp' '#another'
    '\\(?:#\\(?:another\\|samp\\(?:le?\\)?\\)\\)'

which is wrapped in paretheses to create a capture group, and returned."
  (concat "\\(" (regexp-opt (initial-substrings-list word-data)) "\\)"))

;;
;; Classes used to define commands, subcommands, and the arguments therein
(defclass tintin-command ()
  ((cmds :initarg :cmds)
   (face :initarg :face :initform 'font-lock-keyword-face)
   (regexp :initarg :regexp))
  "Base class for standard keyword faces")

(cl-defmethod initialize-instance :after ((obj tintin-command) &rest _)
  "Custom initializer for the `tintin-command' class"
  (let ((command-list (symbol-value (oref obj cmds))))
    (oset obj regexp (build-tintin-command-regexp command-list))))

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
(defun argument-to-regexp (argument)
  "Retrieve the regular expression associated with ARGUMENT."
  (symbol-value (slot-value argument :regexp)))

(defun argument-to-highlighter (argument idx)
  "Produce a subexp-highlighter for ARGUMENT in capture group IDX.
This function retrieves information from a `tintin-argument' and generates
an associated subexp-highlighter describing how a capture group in some other
matcher should he highlighted, per search-based fontification. Returns nil if
the value of the :face slot has not been populated."
  (let ((face (slot-value argument :face))
        (override (slot-value argument :override)))
    (if face `(,idx ',face ,override))))

(defun make-highlighters (highlighter-list)
  "Make a subexp-highlighter for each element in HIGHLIGHTER-LIST.
Each element of HIGHLIGHTER-LIST should be a `tintin-argument', and these
elements are ordered such that they describe capture groups in some other
matcher to be highlighted, per search-based fontification."
  (let* ((n (- (length highlighter-list) 1))
         (indices (number-sequence 2 (+ n 2))))
    (remove nil (cl-mapcar #'argument-to-highlighter highlighter-list indices))))

(defun post-command-regexp (args-regexp)
  "Return a regexp matching ARGS-REGEXP to follow a TinTin++ command.
When ARGS-REGEXP is populated, this return value is simply a delimiter followed
by ARGS-REGEXP itself. When ARGS-REGEXP is empty, then a special regexp is
returned that allows for the presence of a semicolon after the command."
  (if (eq "" args-regexp) tintin-endable (concat tintin-space args-regexp)))

(defun fontify-tintin-subcommand (command &optional arguments)
  "Return search-based fontifier for COMMAND and ARGUMENTS.
Intended as a utility for `fontify-tintin-cmd', this function processes the list
of arguments to produce a regular expression that matches them in sequence, and
concatenates it with a regular expression for the command itself, then returns
a search-based fontifier with a matcher for a command and arguments and
highlighters for each of the capture groups to be highlighted."
  (let* ((arg-values (mapcar #'eval arguments))
         (command-face (slot-value command :face))
         (command-regexp (slot-value command :regexp))
         (args-regexp-list (mapcar #'argument-to-regexp arg-values))
         (args-regexp (string-join args-regexp-list tintin-delimiter))
         (cmd-subtype-regexp (concat command-regexp (post-command-regexp args-regexp))))
    ;; Make a list with a regexp as a matcher, a highlighter, and subexp-highlighters
    (append (list cmd-subtype-regexp `(1 ,command-face))
            (make-highlighters arg-values))))

(defun fontify-tintin-cmd (command &rest subcommands)
  "Return search-based fontifiers for a COMMAND and all SUBCOMMANDS.
The COMMAND is a `tintin-command' and each element of SUBCOMMANDS is a list of
`tintin-argument' instances. From these, a list of highlighters is created that
can be incorporated into `font-lock-keywords' to highlight TinTin++ scripts."
  (let ((base-and-subcommands (append '(nil) subcommands)))
    (mapcar (lambda (args) (fontify-tintin-subcommand command args))
            base-and-subcommands)))

(provide 'tintin-commands)

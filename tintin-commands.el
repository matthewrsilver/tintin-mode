;;; tintin-commands.el --- classes and functions representing and highlighting TinTin++ commands

;; Author: Matthew Silver
;; Keywords: faces, languages
;; Version: 1.0.1
;; URL: https://github.com/matthewrsilver/tintin-mode

;;; Commentary:

;; In general, TinTin++ commands have the following form:
;;
;;   #command arg0 arg1 ... argN;
;;
;; Specific commands have set numbers and types of arguments, which may further vary within
;; a command when one argument is an option that specifies a subcommand. All arguments may
;; optionally be surrounded by braces and, if so, needn't be followed by a space. Commands
;; themselves are case insensitive and may be shortened as long as the substring uniquely
;; expands to a command. With these factors in mind, the following are equivalent:
;;
;;   #COMMAND arg0 {arg1} arg2;
;;   #cOmM {arg0}arg1 {arg2};
;;   #comman {arg0}{arg1}{arg2};
;;
;; Different groups of TinTin++ commands may be grouped into different command categories.
;; Some are for flow control, others to facilitate scripting, and still others interact
;; with the TinTin++ environment itself. Further, different argument positions may perform
;; different actions between commands and even between subcommands.
;;
;; To adequately highlight TinTin++ commands, it's important to indicate for a given command
;; the face that should be used to highlight it; how short the command can be; and the action
;; performed by each of the argument positions. For example, the following forms of the #list
;; command must be supported:
;;
;;   #list my_list create {a} {b} {c};
;;   #list my_list size num_items;
;;   #list my_list get $num_items my_value;
;;
;; Note that in the first example (create) `my_list` is a variable that is being assigned
;; but in the second and third examples (size and get) `my_list` is on the right hand side
;; and is being accessed to set some other variable.
;;
;; To highlight a command like this -- explicitly coloring all variable assignments and also
;; variable usages, as well as command subtype indicators (create, size and get) and the
;; command itself -- we need to represent all commands and their argument positions with
;; functional markers. As such, the #list command above could be represented by
;;
;;   (list-command
;;     '(var-assignment list-create-keyword)
;;     '(var-usage list-size-keyword final-var-assignment)
;;     '(var-usage list-get-keyword arg final-var-assignment))
;;
;; which would then get transformed into a list of regular expressions and associated font
;; faces describing how capture groups should be highlighted for each of the subcommands.
;; To take the size subcommand as an example, the following is shown by font-lock-studio:
;;
;;   "\\(\\(?:#list?\\)\\)\\(?:[ \t]+\\){?\\([@$&*%a-zA-Z0-9_\"]+\\(?:\\[.*]\\)?\\)\\(?:[} \t]\\|$\\)\\(?\:[ \t]*\\){?\\(size\\)\\(?:[} \t;]\\|$\\)\\(?:[ \t]*\\){?\\([@$&*%a-zA-Z0-9_\"]+\\(?:\\[.*]\\)?\\)\\\(?:[} \t;]\\|$\\)"
;;     (1 font-lock-keyword-face)
;;     (2 'tintin-variable-usage-face keep)
;;     (3 'font-lock-type-face nil)
;;     (4 'font-lock-variable-name-face keep)
;;
;; This code provides the means to specify commands in the concise form above and produce
;; appropriate search-based fontifiers for use in font-lock-keywords easily.

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defun string-join (lst sep)
  "Join strings in LST with SEP between each, returning the result."
  (mapconcat 'identity lst sep))

;; Set up the font faces and stand up a few tintin-argument instances that
;; enable us to concisely define many commands.
(defface tintin-ansi-face '((t (:foreground "#c95d5d"))) "*Face for ansi color codes.")
(defface tintin-capture-face '((t (:foreground "#8dd110"))) "*Face for capture variables.")
(defface tintin-function-face '((t (:foreground "#5dc9c9"))) "*Face for user functions.")
(defface tintin-command-face '((t (:foreground "#5d74c9"))) "*Face for user hash commands.")
(defface tintin-variable-usage-face '((t (:foreground "#e09d02"))) "*Face for variable usages.")

(defcustom  tintin-command-character "#"
  "The symbol used to mark the beginning of TinTin++ commands.")
(rx-define tintin-command-character (eval tintin-command-character))

;; Key regular expressions that are useful here in specifying broad TinTin++
;; command structure, but are also useful in highlighting key aspects of code.
(rx-define var-prefix (any "&$*"))
(rx-define var-chars (: (any "a-zA-Z_") (* (any "a-zA-Z0-9_.'"))))
(rx-define var-table (: "[" (* (not "]")) "]"))
(rx-define tintin-var-name (: var-chars (* var-table)))

;; Regular expressions that allow for specification of variables under a variety
;; of circumstances: with and without both grouping and presence/absence of braces
(rx-define optionally-braced (g rx-elem) (or (g rx-elem) (: "{" (g rx-elem) "}")))
(rx-define tintin-variable
  (: var-prefix (optionally-braced sequence tintin-var-name)))
(rx-define simple-variable
  (: (group var-prefix) (optionally-braced group var-chars)))
(rx-define simple-func-pattern
  (: "@" var-chars "{" (* (or (not "}") tintin-variable)) (or "}" eol)))

;; Regular expressions that match optionally braced variable names
(rx-define braced-content
  (* (or tintin-variable simple-func-pattern (not (any "}\n")))))
(rx-define variable-name-for-bracing
  (or tintin-var-name
      (: "\"" (? braced-content) "\"" (? braced-content) "\"")
      (: (not "\"") braced-content (not "\""))
      (: braced-content (not "\""))
      (: (not "\"") braced-content)))
(rx-define braced-variable-name (: "{" (group variable-name-for-bracing) (or "}" eol) ))
(rx-define optionally-braced-tintin-variable
  (: (group var-prefix) (or (group tintin-var-name) braced-variable-name)))

;; Provide compact regexes for handling arguments in commands
(rx-define capture-chars
  (+ (or tintin-variable
         simple-func-pattern
         (: (any alphanumeric "_")
            (* (any "@%\"'_.,!#^&*()?><:+=-" alphanumeric))))
     (* var-table)))

(rx-define tintin-argument (final) (group (or "{}"
  (: "{" braced-content (or "}" eol))
  (: capture-chars (or (any blank final) eol)))))
(defvar tintin-arg (rx (tintin-argument "")))
(defvar tintin-final-arg (rx (tintin-argument ";")))

(rx-define tintin-variable-argument (final) (group (or "{}"
  (: "{" variable-name-for-bracing (or "}" eol))
  (: capture-chars (or (any blank final) eol)))))
(defvar tintin-var-arg (rx (tintin-variable-argument "")))
(defvar tintin-final-var-arg (rx (tintin-variable-argument ";")))


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
      (append (apply #'initial-substrings (last word-data 2))
              (initial-substrings-list (butlast word-data 2)))))

(rx-define substrings (keyword-list)
  (regexp (eval (regexp-opt (initial-substrings-list keyword-list)))))

(defun firstword-matcher (word &optional min-len)
  "Return a WORD and MIN-LEN pair with only the first word if relevant.
When the MIN-LEN is greater than the length of the first word in WORD then
the pair is not suitable for matching an empty list is returned"
  (let* ((min-len (if (= min-len 0) (length word) min-len))
         (firstword (car (split-string word))))
    (if (>= (length firstword) min-len) `(,firstword ,min-len) '() )))

(defun firstwords-list (word-data)
  "Return a list of single-word and minimum character pairs from WORD-DATA.
Pairs of elements comprise WORD-DATA; the first character element is a string
with a word whose initial substrings should be computed, and the second is
and integer describing the minimum length substring to be generated, per
`initial-substrings'. Successive pairs describe new words and minimum lengths.
This function assesses each pair and identifies those with multi-word strings
with minimum lengths less than the number of characters in the first string.
When such a pair is encountered, it is included in the list that is returned."
  (if (> (length word-data) 0)
      (append (firstwords-list (butlast word-data 2))
              (apply #'firstword-matcher (last word-data 2)))))

(rx-define multiword-option (keyword-list)
  (group (or (: "{" (substrings keyword-list) (or "}" eol))
             (: (substrings (firstwords-list keyword-list))
                (or blank eol (not (any alphanumeric "{;"))))
             tintin-variable)))

(rx-define multiword-option-final (keyword-list)
  (group (or (: "{" (substrings keyword-list) (or "}" eol))
             (: (substrings (firstwords-list keyword-list))
                (or blank eol ";" (not (any alphanumeric "{"))))
             tintin-variable)))

(defun build-tintin-command-regexp (word-data)
  "Return a regular expression that matches words in WORD-DATA.
Pairs of elements comprise WORD-DATA; the first element is a string with a word
whose initial substrings should be computed, and the second is an integer that
defines the minimum length substring to be generated, per `initial-substrings'.
Successive pairs describe new words and minimum lengths. This function produces
an optimized regular expression that matches the words accordingly.

For example, the following progression from WORD-DATA to words to regexp:

   '(\"sample\" 4 \"another\" 0)
    'sample' 'sampl' 'samp' 'another'
    '\\(?:another\\|samp\\(?:le?\\)?\\)'

which is wrapped in paretheses to create a capture group. The configurable
`tintin-command-character' is prepended, and the regular expression returned."
  (rx (group tintin-command-character (substrings word-data))))

;; Functions to build up seach-based fontificators
(defun argument-to-regexp (argument)
  "Retrieve the regular expression associated with ARGUMENT."
  (slot-value argument :regexp))

(defun argument-to-highlighter (argument idx)
  "Produce a subexp-highlighter for ARGUMENT in capture group IDX.
This function retrieves information from a `tintin-argument' and generates
an associated subexp-highlighter describing how a capture group in some other
matcher should he highlighted, per search-based fontification. Returns nil if
the value of the :face slot has not been populated."
  (let ((face (slot-value argument :face))
        (override (slot-value argument :override))
        (optional (slot-value argument :optional)))
    (if face `(,idx ',face ,override ,optional))))

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
  (let ((tintin-endable (rx (group (or (: (+ blank) (? ";")) ";" eol))))
        (tintin-space (rx (+ blank))))
    (if (eq "" args-regexp) tintin-endable (concat tintin-space args-regexp))))

(defun fontify-tintin-subcommand (command &optional arguments)
  "Return search-based fontifier for COMMAND and ARGUMENTS.
Intended as a utility for `fontify-tintin-cmd', this function processes the list
of arguments to produce a regular expression that matches them in sequence, and
concatenates it with a regular expression for the command itself, then returns
a search-based fontifier with a matcher for a command and arguments and
highlighters for each of the capture groups to be highlighted."
  (let* ((tintin-delimiter (rx (* blank)))
         (arg-values (mapcar #'eval arguments))
         (command-face (slot-value command :face))
         (command-regexp (slot-value command :regexp))
         (args-regexp-list (mapcar #'argument-to-regexp arg-values))
         (args-regexp (string-join args-regexp-list tintin-delimiter))
         (cmd-subtype-regexp (concat command-regexp (post-command-regexp args-regexp))))
    ;; Make a list with a regexp as a matcher, a highlighter, and subexp-highlighters
    (append (list cmd-subtype-regexp `(1 ',command-face))
            (make-highlighters arg-values))))

(defun fontify-tintin-cmd (command &rest subcommands)
  "Return search-based fontifiers for a COMMAND and all SUBCOMMANDS.
The COMMAND is a `tintin-command' and each element of SUBCOMMANDS is a list of
`tintin-argument' instances. From these, a list of highlighters is created that
can be incorporated into `font-lock-keywords' to highlight TinTin++ scripts."
  (let ((base-and-subcommands (append '(nil) subcommands)))
    (mapcar (lambda (args) (fontify-tintin-subcommand command args))
            base-and-subcommands)))

;; Classes used to define commands, subcommands, and the arguments therein
(defclass tintin-command ()
  ((cmds :initarg :cmds)
   (face :initarg :face :initform font-lock-keyword-face)
   (regexp :initarg :regexp))
  "Base class for standard keyword faces")

(cl-defmethod initialize-instance :after ((obj tintin-command) &rest _)
  "Custom initializer for the `tintin-command' class."
  (let ((command-list (symbol-value (oref obj cmds))))
    (oset obj regexp (build-tintin-command-regexp command-list))))

(defclass tintin-argument ()
  ((regexp :initarg :regexp :initform (eval tintin-arg))
   (face :initarg :face :initform nil)
   (override :initarg :override :initform nil)
   (optional :initarg :optional :initform nil)
   (final :initarg :final :initform nil)
   (vals :initarg :vals :initform nil))
  "Base class that represents an unhighlighted, generic TinTin++ argument.")

(cl-defmethod initialize-instance :after ((obj tintin-argument) &rest _)
  "Custom initializer for the `tintin-argument' class."
  (let* ((value-list (oref obj vals))
         (values-regexp (if (oref obj final)
                            (rx (multiword-option-final value-list))
                          (rx (multiword-option value-list)))))
    (if value-list (oset obj regexp values-regexp))))

(defclass tintin-option (tintin-argument)
  ((face :initform 'font-lock-type-face)
   (override :initarg :override :initform 'keep))
  "Command option argument class, that represents an option specifying a subcommand type.")


(setq arg (tintin-argument))
(setq final-arg (clone arg :regexp tintin-final-arg))

(setq var-arg (tintin-argument :regexp tintin-var-arg :override 'keep))
(setq var-usage (clone var-arg :face 'tintin-variable-usage-face))
(setq final-var-usage (clone var-usage :regexp tintin-final-var-arg))
(setq var-assignment (clone var-arg :face 'font-lock-variable-name-face))
(setq final-var-assignment (clone var-assignment :regexp tintin-final-var-arg))

(setq function-name (clone arg :face 'font-lock-function-name-face :override 'keep))
(setq command-type (clone arg :face 'font-lock-type-face :override 'keep))

(defvar toggle-constant-values '("off" 2 "on" 1))
(defvar toggle-value
  (tintin-option :vals toggle-constant-values :face 'font-lock-constant-face :final t))

(defvar settable-character-regexp
  (rx (group (optionally-braced sequence (not (any "{};"))))))
(defvar settable-character
  (tintin-argument :regexp settable-character-regexp :override 'keep))

(provide 'tintin-commands)

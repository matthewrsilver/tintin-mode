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

(defcustom  tintin-command-character "#"
  "The symbol used to mark the beginning of TinTin++ commands.")
(rx-define tintin-command-character (eval tintin-command-character))

;;
;; A few generic useful functions
(defun string-join (lst sep)
  "Join strings in LST with SEP between each, returning the result."
  (mapconcat 'identity lst sep))

;;
;; Key regular expressions that are useful here in specifying broad TinTin++
;; command structure, but are also useful in highlighting key aspects of code.
(rx-define var-prefix (any "&$*"))
(rx-define var-chars (: (any "a-zA-Z_") (* (any "a-zA-Z0-9_.'"))))
(rx-define var-table (: "[" (* (not "]")) "]"))
(rx-define tintin-var-name (: var-chars (* var-table)))

;; Regular expressions that allow for specification of variables under a variety
;; of circumstances: with and without both grouping and presence/absence of braces
(rx-define grouped (&rest rx-elem) (group rx-elem))
(rx-define ungrouped (&rest rx-elem) (: rx-elem))
(rx-define braced (g rx-elem) (: "{" (g rx-elem) "}"))
(rx-define unbraced (g rx-elem) (g rx-elem))
(rx-define optionally-braced (g rx-elem) (or (g rx-elem) (: "{" (g rx-elem) "}")))
(rx-define tintin-var-pattern (g b) (: (g var-prefix) (b g tintin-var-name)))
(rx-define simple-var-pattern (g b) (: (g var-prefix) (b g var-chars)))

(rx-define tintin-variable (tintin-var-pattern grouped optionally-braced))
(rx-define simple-variable (simple-var-pattern grouped optionally-braced))
(rx-define ungrouped-tintin-variable (tintin-var-pattern ungrouped optionally-braced))
(rx-define braced-tintin-variable (tintin-var-pattern ungrouped braced))

(rx-define simple-func-pattern
  (: "@" var-chars "{" (* (or (not "}") ungrouped-tintin-variable)) "}"))

(rx-define braced-content (* (or (simple-var-pattern ungrouped braced) (not "}"))))
(rx-define variable-name-for-bracing
  (or (: "\"" (? braced-content) "\"" (? braced-content) "\"")
      (: (not "\"") braced-content (not "\""))
      (: braced-content (not "\""))
      (: (not "\"") braced-content)))
(rx-define braced-variable-name (: "{" (group variable-name-for-bracing) "}" ))
(rx-define optionally-braced-tintin-variable
  (: (group var-prefix) (or (group tintin-var-name) braced-variable-name)))

;;
;; Provide compact regexes for handling arguments in commands
(rx-define capture-chars
  (+ (or ungrouped-tintin-variable
         simple-func-pattern
         (: (any alphanumeric "_")
            (* (any "@%\"'_.,!#^&*()?><:+=-" alphanumeric))))
     (* var-table)))
(rx-define tintin-argument (final) (group (or "{}"
  (: "{" variable-name-for-bracing "}")
  (: capture-chars (or (any "\s\t" final) eol)))))
(defvar tintin-arg (rx (tintin-argument "")))
(defvar tintin-final-arg (rx (tintin-argument ";")))


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

   '(\"sample\" 4 \"another\" 0)
    'sample' 'sampl' 'samp' 'another'
    '\\(?:another\\|samp\\(?:le?\\)?\\)'

which is wrapped in paretheses to create a capture group. The configurable
`tintin-command-character' is prepended, and the regular expression returned."
  (rx (group (eval tintin-command-character)
             (regexp (eval (regexp-opt (initial-substrings-list word-data)))))))

(defun build-tintin-arg-regexp (value-list &rest others)
  "Return a regular expression matching all in VALUES-LIST or OTHERS.
Elements of VALUE-LIST are strings with possible values that should be matched
in completion. Optionally, additional arguments can be provided in OTHERS that
may be matches as well, though these are complete regular expressions."
  (let* ((brace-or-space "\\(?:[}\s\t;]\\|$\\)")
         (arg-regexp (regexp-opt value-list nil))
         (regexps (cons arg-regexp others)))
    (concat "{?\\(" (string-join regexps "\\|") "\\)" brace-or-space)))


;;
;; Classes used to define commands, subcommands, and the arguments therein
(defclass tintin-command ()
  ((cmds :initarg :cmds)
   (face :initarg :face :initform 'font-lock-keyword-face)
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
   (vals :initarg :vals :initform nil))
  "Base class that represents an unhighlighted, generic TinTin++ argument.")

(cl-defmethod initialize-instance :after ((obj tintin-argument) &rest _)
  "Custom initializer for the `tintina-argument' class."
  (let* ((value-list (oref obj vals))
         (values-regexp (build-tintin-arg-regexp value-list)))
    (if value-list (oset obj regexp values-regexp))))

(defclass tintin-option (tintin-argument)
  ((face :initform 'font-lock-type-face))
  "Command option argument class, that represents an option specifying a subcommand type.")

;;
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
  (let ((tintin-endable "\\(?:[\s\t]+;?\\|;\\|$\\)")
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

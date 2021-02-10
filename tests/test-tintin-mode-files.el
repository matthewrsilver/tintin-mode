;;; test-tintin-mode-samples.el --- Tests for TinTin++ syntax highlighting

;; Author: Matthew Silver
;; Keywords: faces languages

;; TODO: why do I have to add this to the load path??
(add-to-list 'load-path "~/.emacs.d/tintin-mode")
(require 'tintin-mode)
(require 'faceup)

(defvar tintin-font-lock-test-dir (faceup-this-file-directory))

(defun tintin-font-lock-test-file (file)
  "Test that the tintin .tt file is fontified as the .faceup file describes."
  (faceup-test-font-lock-file 'tintin-mode
                              (concat tintin-font-lock-test-dir file)))
(faceup-defexplainer tintin-font-lock-test-file)

(ert-deftest tintin-font-lock-file-test ()
  (should (tintin-font-lock-test-file "commands/class_command.tt"))
  (should (tintin-font-lock-test-file "commands/list_command.tt"))
  (should (tintin-font-lock-test-file "commands/script_command.tt"))

  (should (tintin-font-lock-test-file "misc/pattern_matching.tt"))
  (should (tintin-font-lock-test-file "misc/variable_definitions.tt"))
  (should (tintin-font-lock-test-file "misc/warning_fonts.tt"))

  )


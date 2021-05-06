;;; test-tintin-mode-files.el --- Tests for TinTin++ syntax highlighting

;; Author: Matthew Silver
;; Keywords: faces languages

;; TODO: why do I have to add this to the load path??
(add-to-list 'load-path "~/.emacs.d/tintin-mode/")
(require 'tintin-mode)
(require 'faceup)

(defvar tintin-font-lock-test-dir (faceup-this-file-directory))

(defun tintin-font-lock-test-file (file)
  "Test that the tintin .tt file is fontified as the .faceup file describes."
  (faceup-test-font-lock-file 'tintin-mode
                              (concat tintin-font-lock-test-dir file)))
(faceup-defexplainer tintin-font-lock-test-file)

(ert-deftest tintin-font-lock-builtin-commands-test ()
  (should (tintin-font-lock-test-file "commands/builtin_commands.tt")))

(ert-deftest tintin-font-lock-class-command-test ()
  (should (tintin-font-lock-test-file "commands/class_command.tt")))

(ert-deftest tintin-font-lock-config-command-test ()
  (should (tintin-font-lock-test-file "commands/config_command.tt")))

(ert-deftest tintin-font-lock-cursor-command-test ()
  (should (tintin-font-lock-test-file "commands/cursor_command.tt")))

(ert-deftest tintin-font-lock-daemon-command-test ()
  (should (tintin-font-lock-test-file "commands/daemon_command.tt")))

(ert-deftest tintin-font-lock-debug-command-test ()
  (should (tintin-font-lock-test-file "commands/debug_command.tt")))

(ert-deftest tintin-font-lock-history-command-test ()
  (should (tintin-font-lock-test-file "commands/history_command.tt")))

(ert-deftest tintin-font-lock-ignore-command-test ()
  (should (tintin-font-lock-test-file "commands/ignore_command.tt")))

(ert-deftest tintin-font-lock-line-command-test ()
  (should (tintin-font-lock-test-file "commands/line_command.tt")))

(ert-deftest tintin-font-lock-list-command-test ()
  (should (tintin-font-lock-test-file "commands/list_command.tt")))

(ert-deftest tintin-font-lock-script-command-test ()
  (should (tintin-font-lock-test-file "commands/script_command.tt")))

(ert-deftest tintin-font-lock-variable-definition-commands-test ()
  (should (tintin-font-lock-test-file "commands/variable_definition_commands.tt")))

(ert-deftest tintin-font-lock-comments-test ()
  (should (tintin-font-lock-test-file "misc/comments.tt")))

(ert-deftest tintin-font-lock-pattern-matching-test ()
  (should (tintin-font-lock-test-file "misc/pattern_matching.tt")))

(ert-deftest tintin-font-lock-variable-usage-test ()
  (should (tintin-font-lock-test-file "misc/variable_usage.tt")))

(ert-deftest tintin-font-lock-function-usage-test ()
  (should (tintin-font-lock-test-file "misc/function_usage.tt")))

(ert-deftest tintin-font-lock-warning-test ()
  (should (tintin-font-lock-test-file "misc/warning_fonts.tt")))


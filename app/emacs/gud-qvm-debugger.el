;;; gud-qvm-debugger.el --- GUD mode for the QVM debugger

;; Copyright (C) 2020 Rigetti Computing

;; Author: Juan Bello-Rivas <jbellorivas@rigetti.com>
;; Keywords: gud, quil, qvm

;;; Commentary:

;;; Code:

(require 'gud)

(defcustom gud-qvm-debugger-command-name "qvm --debugger"
  "Default command to run the QVM debugger."
  :type 'string
  :group 'gud)

(defun qvm-debugger (command-line)
  "Invoke the Quil debugger with COMMAND-LINE arguments."
  (interactive
   (list (gud-query-cmdline 'qvm-debugger)))

  (gud-common-init command-line 'gud-qvm-debugger-massage-args 'gud-qvm-debugger-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'qvm-debugger)

  (gud-def gud-step "step"        "\C-s" "Step one line with display.")
  (gud-def gud-cont "continue"    "\C-r" "Continue with display.")

  (setq comint-prompt-regexp "^(qvm-debugger) *")
  (setq paragraph-start comint-prompt-regexp))

(defun gud-qvm-debugger-massage-args (_file args)
  "Massage ARGS."
  args)

(defun gud-qvm-debugger-marker-filter (string)
  "Filter the markers in STRING."
  string)

(defvar gud-qvm-debugger-history nil)

(provide 'gud-qvm-debugger)
;;; gud-qvm-debugger.el ends here

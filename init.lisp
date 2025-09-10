;;; AUTHOR: Ethan Smith
;;; DATE: 9 September 2025
;;; DESCRIPTION: Configuration Loader For Lem
;;; This is the first file that is loaded by Lem.  This file is responsible for
;;; pulling any changes to the configuration from the git repository and then
;;; loading the files in the init directory.
;;;
;;; This is done so that you don't have to manually run `git pull' every time
;;; the config needs updated.  However, if this file is updated, `git pull' 
;;; may need to be run so that files are loaded correctly.

(in-package :lem-user)

;; Ensure configuration is up to date with git server

(unless (equal (uiop:run-program "cd ~/.lem && git fetch" :force-shell t :output :string)
               "")
  (if (prompt-for-y-or-n-p "Merge local config with remote?")
      (uiop:run-program "cd ~/.lem && git pull")))

;; Load User Configuration
(load #P"~/.lem/init/all.lisp")
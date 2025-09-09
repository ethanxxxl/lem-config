;;; AUTHOR: Ethan Smith
;;; DATE: 8 September 2025
;;; DESCRIPTION: Init file for my lem configuration

(in-package :lem-user)

(push #P"~/.lem/packages/lem-pareto/" asdf:*central-registry*)
(asdf:load-system :lem-pareto)
; Enable Paredit and Pareto along with Lisp mode
(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer)
                      'lem-lisp-mode:lisp-mode)
              (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t)
              (change-buffer-mode buffer 'lem-pareto-mode:pareto-mode ))))


;;; AUTHOR: Ethan Smith
;;; DATE: 8 September 2025
;;; DESCRIPTION: Init file for my lem configuration

(in-package :lem-user)

; (push #P"~/.lem/packages/lem-pareto/" asdf:*central-registry*)
; (asdf:load-system :lem-pareto)
; ; Enable Paredit and Pareto along with Lisp mode
; (add-hook *find-file-hook*
;           (lambda (buffer)
;             (when (eq (buffer-major-mode buffer)
;                       'lem-lisp-mode:lisp-mode)
;               (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t)
;               (change-buffer-mode buffer 'lem-pareto-mode:pareto-mode ))))

(load-theme "classic-dark")

(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer)
                      'lem-lisp-mode:lisp-mode)
              (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t))))

(undefine-key lem-paredit-mode:*paredit-mode-keymap* "C-h")

(define-keys *global-keymap*
  ("C-h f" 'lem-lisp-mode:lisp-describe-symbol)
  ("C-h k" 'describe-key)
  ("C-h m" 'describe-mode)
  ("C-h I" 'lem-lisp-mode:lisp-inspect)
  ("C-h b" 'describe-bindings))

(define-keys lem-paredit-mode:*paredit-mode-keymap*
  ("C-<" 'lem-paredit-mode:paredit-barf)
  ("C->" 'lem-paredit-mode:paredit-slurp))

(define-keys *global-keymap*
  ("C-/" 'undo)
  ("C-?" 'redo))

(lem/line-numbers:toggle-line-numbers)

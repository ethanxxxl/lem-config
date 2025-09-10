;;; AUTHOR: Ethan Smith
;;; DATE: 8 September 2025
;;; DESCRIPTION: Init file for my lem configuration

(in-package :lem-user)

;;; Any additional files that are a part of the configuration 
;;; should be loaded here
;
;
;

;;; Load Pareto 
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

;; enable paredit minor-mode in lisp mode
(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer)
                      'lem-lisp-mode:lisp-mode)
              (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t))))

(define-command apropos-symbol (symbol package) ((:string "Symbol Name: ") (:string "Package: "))
  (lem:show-message
   (format nil "~{~(~S~)~%~}" (apropos-list (string-upcase symbol)
                                            (string-upcase package)))))

(define-command open-config () ()
  (find-file #P"~/.lem/init.lisp"))

;; set convenience key-bindings

;; NOTE: these keybindings are meant for use in the graphical SDL2 version of LEM.
;;       They will not work in the ncurses mode.
(undefine-key lem-paredit-mode:*paredit-mode-keymap* "C-h")
(define-keys *global-keymap*
  ("C-h f" 'lem-lisp-mode:lisp-describe-symbol)
  ("C-h c" 'apropos-command)
  ("C-h a" 'apropos-symbol)
  ("C-h k" 'describe-key)
  ("C-h m" 'describe-mode)
  ("C-h I" 'lem-lisp-mode:lisp-inspect)
  ("C-h b" 'describe-bindings))

(define-keys *global-keymap*
  ("C-x C-Left" 'previous-buffer)
  ("C-x C-Right" 'next-buffer))

(define-key *global-keymap* "C-c p" 'open-config)

(define-keys lem-paredit-mode:*paredit-mode-keymap*
  ("C-<" 'lem-paredit-mode:paredit-barf)
  ("C->" 'lem-paredit-mode:paredit-slurp))

(define-keys *global-keymap*
  ("C-/" 'undo)
  ("C-?" 'redo))

;; Graphics stuff
(lem/line-numbers:toggle-line-numbers)
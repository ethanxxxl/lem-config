(defpackage :autofill-mode
  (:use :cl :lem))

(in-package :autofill-mode)

;(define-minor-mode :autofill-mode)

(defvar *fill-width* 80)

(defun get-autofill-text (start end)
  "Returns text between START and END.  If text is on multiple lines, 
any newlines and leading/trailing whitespace is removed.  The whitespace 
that is removed are #\\Space, #\\Tab, and #\\Page characters."
  (assert (equal (lem:point-buffer start) (lem:point-buffer end)))
  (assert (lem:point< start end))

  (lem:with-point ((p1 start)
                   (p2 start))
    ;; REDUCE combines all lines together, separated by spaces
    (reduce (lambda (a b) (concatenate 'string a " " b)) 
            (loop :while (and p1 (lem:point<= p1 end))
                  :collect (string-trim '(#\Space #\Tab #\Page)
                                        (progn
                                          (lem:line-end p2)
                                          (lem:points-to-string 
                                           p1 (lem:point-min p2 end))))
                  
                  :do (setf p1 (lem:line-offset p1 1)
                            p2 (lem:line-offset p2 1))))))

(defun wrap-line (text &key (fill-width *fill-width*))
  "Inserts newlines every FILL-WIDTH characters or less, based on
word boundaries.  TEXT should not have newline characters."
  (loop :for i :from (1- fill-width) :below (length text) :by fill-width
        :do (progn
              ;; ensure that i is at a space in between words.
              (unless (eq #\Space (elt text i))
                (loop :for n :from i :above 0
                      :until (eq #\Space (elt text n))
                      :finally (setf i n)))
              
              (setf (elt text i) #\Newline))
        :finally (return text)))

(defun autofill-paragraph (point)
  "Gets gets the paragraph at POINT.  Replace it with filled text."

  (lem:with-point ((start point)
                   (end point))
    ;; TODO check for empty lines, or lines that contain only whitespace
    (loop :until (let ((s (lem:line-string start))) 
                   (or (not s) (equal "" (lem:line-string start))))
          :do (lem:line-offset start -1))
    
    (loop :until (let ((s (lem:line-string end))) 
                   (or (not s) (equal "" (lem:line-string end))))
          :do (lem:line-offset end 1))
    
    (let ((text (get-autofill-text start end)))
      (lem:delete-between-points start end)
      (lem:insert-string start (wrap-line text)))))

(defvar *fill-function* 'autofill-paragraph)
(lem:define-command autofill () ()
  (funcall *fill-function* (lem:current-point)))


" This is some text
tha I will use here
dfsdf
 asdlkj lkjf
faskl
afaklj salkjf sadlkfj ad
f adfting!"


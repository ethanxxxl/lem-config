(defpackage :autofill-mode
  (:use :cl :lem))

(in-package :autofill-mode)

;(define-minor-mode :autofill-mode)

(defvar *fill-width* 80)

(defun get-autofill-text (start end &key (sentence-spaces 2))
  "Returns text between START and END.  If text is on multiple lines, any
newlines and leading/trailing whitespace is removed.  The whitespace that is
removed are #\\Space, #\\Tab, and #\\Page characters.

If a line ends with a non space character and punctuation mark (ie, end of a
sentence), SENTENCE-SPACES controls how many spaces to put between the two
lines"

  (assert (equal (lem:point-buffer start) (lem:point-buffer end)))
  (assert (lem:point< start end))
  (assert (> sentence-spaces 0))

  (lem:with-point ((p1 start)
                   (p2 start))
    ;; REDUCE combines all lines together, separated by spaces
    (reduce (lambda (a b) (concatenate 'string a " " b)) 
            (loop :while (and p1 (lem:point<= p1 end))
                  :collect 
                  ;; TODO this logic could be replaced with a regex
                     (let (s len punct last-letter)
                       (lem:line-end p2)

                       (setf s (lem:points-to-string p1 (lem:point-min p2 end)))
                       (setf s (string-trim '(#\Space #\Tab #\Page) s))

                       (setf len (length s))
                       (when (<= 2 len)
                         (setf punct (elt s (- len 1))
                               last-letter (elt s (- len 2)))
                       
                         (if (and (alpha-char-p last-letter)
                                  (or  (equal #\. punct)
                                       (equal #\! punct)
                                       (equal #\? punct)))
                             (setf s (concatenate
                                      'string s (make-string 
                                                 (1- sentence-spaces) :initial-element #\Space)))))
                       s)
                  
                  :do (setf p1 (lem:line-offset p1 1)
                            p2 (lem:line-offset p2 1))))))

(defun wrap-line (text &key (fill-width *fill-width*))
  "Inserts newlines every FILL-WIDTH characters or less, based on word
boundaries.  TEXT should not have newline characters."

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
                   (end point)
                   (tmp point))
    ;; TODO check for empty lines, or lines that contain only whitespace
    (loop :until (let ((p (lem:line-offset tmp -1))) 
                   (or (not p) (equal "" (lem:line-string p))))
          :do (lem:line-offset start -1))
    (lem:line-start start)

    (lem:move-point tmp point)
    (loop :until (let ((s (lem:line-offset tmp 1))) 
                   (or (not s) (equal "" (lem:line-string s))))
          :do (lem:line-offset end 1))
    (lem:line-end end)
    
    (let ((text (get-autofill-text start end)))
      (lem:delete-between-points start end)
      (lem:insert-string start (wrap-line text)))))

(defvar *fill-function* 'autofill-paragraph)
(lem:define-command autofill () ()
  "FIXME WIP: Generic autofill command.  Autofill method is chosen based on the
current mode."
  (funcall *fill-function* (lem:current-point)))

(lem:define-key lem:*global-keymap* "M-q" 'autofill)

"

This is a test of the autofill program that I have written.  This is filler
text that will serve as a test of the autofill program. I do not know if this
will work well.  I hope that it does.  I think that there are probably bugs in
this algorithm.  Hopefully this text can find any bugs that might exist in this
preliminary test.  Ok there!  This is exciting.  hmm.  I bet the double space
after sentences will not be preserved. 

This is a test of the autofill program that I have written.  This is filler text that will serve as a test
 of the autofill program. I do not know if this will work well.  I hope that it does.
  I think that there are probably bugs in this algorithm.
Hopefully this text can find any bugs that might exist in this
preliminary test.
Ok there!
This is exciting.
hmm. 
I bet the double space after sentences will not be preserved.
but just to test, for some reason if I ended with a,
comma, that would be rendered correctly.
If for some reason, I had a number x.n
that would be fine.
If I had done this .
then no big deal?
Hopefully.
"
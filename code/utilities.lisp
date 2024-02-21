(cl:in-package #:incrementalist)

(defun whitespacep (character) ; TODO move to spell-checking
  (member character '(#\Space #\Tab #\Newline #\Page)))

(defun punctuationp (character)
  (member character '(#\. #\? #\! #\: #\, #\;
                      #\( #\) #\< #\> #\[ #\] #\{ #\}
                      #\" #\' #\` #\/ #\_ #\- #\+ #\* #\% #\= #\#)))

;;; Positions

(defmacro %position< (line-number1 item-number1 line-number2 item-number2)
  (let ((n-line-number1 (gensym "LINE-NUMBER1"))
        (n-line-number2 (gensym "LINE-NUMBER2")))
    `(let ((,n-line-number1 ,line-number1)
           (,n-line-number2 ,line-number2))
       (or (< ,n-line-number1 ,n-line-number2)
           (and (= ,n-line-number1 ,n-line-number2)
                (< ,item-number1 ,item-number2))))))

;;; Return true if and only if the position indicated by the absolute
;;; line number LINE-NUMBER1 and the column number COLUMN-NUMBER1 is
;;; less than or equal to the position indicated by the absolute line
;;; number LINE-NUMBER2 and the column number COLUMN-NUMBER2.
(defmacro %position<= (line-number1 item-number1 line-number2 item-number2)
  (let ((n-line-number1 (gensym "LINE-NUMBER1"))
        (n-line-number2 (gensym "LINE-NUMBER2")))
    `(let ((,n-line-number1 ,line-number1)
           (,n-line-number2 ,line-number2))
       (or (< ,n-line-number1 ,n-line-number2)
           (and (= ,n-line-number1 ,n-line-number2)
                (<= ,item-number1 ,item-number2))))))

(defmacro %position> (line-number1 item-number1 line-number2 item-number2)
  `(not (%position<= ,line-number1 ,item-number1 ,line-number2 ,item-number2)))

(defmacro %position>= (line-number1 item-number1 line-number2 item-number2)
  `(not (%position< ,line-number1 ,item-number1 ,line-number2 ,item-number2)))

(defun position-is-inside-interval-p (position-line-number
                                      position-column-number
                                      interval-start-line-number
                                      interval-start-column-number
                                      interval-end-line-number
                                      interval-end-column-number)
  (and (%position> position-line-number position-column-number
                   interval-start-line-number interval-start-column-number)
       (%position< position-line-number position-column-number
                   interval-end-line-number interval-end-column-number)))

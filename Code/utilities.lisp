(cl:in-package #:incrementalist)

(defun whitespacep (character)
  (member character '(#\Space #\Newline)))

(defun punctuationp (character)
  (member character '(#\. #\? #\! #\: #\, #\;
                      #\( #\) #\< #\> #\[ #\] #\{ #\}
                      #\" #\' #\` #\/ #\- #\+ #\* #\% #\= #\#)))

;;; Return the line number and the column number of CURSOR as two
;;; values.
(defun cursor-positions (cursor)
  (values (cluffer:line-number cursor)
          (cluffer:cursor-position cursor)))

;;; Set the line number and the column number of CURSOR.
(defun set-cursor-positions (cursor line-number column-number)
  (let ((buffer (cluffer:buffer cursor)))
    (when (cluffer:cursor-attached-p cursor)
      (cluffer:detach-cursor cursor))
    (cluffer:attach-cursor
      cursor
      (cluffer:find-line buffer line-number)
      column-number)))

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

(cl:in-package #:incrementalist)

(defun whitespacep (character) ; TODO move to spell-checking
  (member character '(#\Space #\Tab #\Newline #\Page)))

(defun punctuationp (character)
  (member character '(#\. #\? #\! #\: #\, #\;
                      #\( #\) #\< #\> #\[ #\] #\{ #\}
                      #\" #\' #\` #\|
                      #\/ #\\ #\_ #\- #\+ #\* #\% #\= #\# #\~ #\@ #\&)))

(defun make-text-wads (stream source
                       &key (start-column-offset 0)
                            (end-column-offset   0 end-column-offset-p)
                            (min-length          1))
  (destructure-source (start-line start-column end-line end-column*) source
    (let* ((cache             (cache stream))
           (text              (make-array 0 :element-type 'character
                                            :adjustable   t
                                            :fill-pointer 0))
           (text-start-column (+ start-column start-column-offset))
           (wads              '()))
      (flet ((terminatingp (character)
               (let ((spacep       (whitespacep character))
                     (punctuationp (punctuationp character)))
                 (values (or spacep punctuationp) punctuationp)))
             (commit (line column checkp)
               (let ((length (length text))
                     (source (cons (cons line text-start-column)
                                   (cons line column))))
                 (cond ((zerop length)) ; do not collect empty wads
                       ((every #'punctuationp text)
                        (push (basic-wad 'punctuation-wad stream source)
                              wads))
                       ((and (>= length min-length)
                             (notany #'digit-char-p text))
                        (let ((misspelledp (and checkp
                                                (null (spell:english-lookup text)))))
                          (push (basic-wad 'word-wad stream source
                                           :misspelled misspelledp)
                                wads)))
                       (t
                        (push (basic-wad 'text-wad stream source)
                              wads))))
               (setf (fill-pointer text) 0
                     text-start-column   column)))
        (loop for line from start-line to (if (zerop end-column*)
                                              (1- end-line)
                                              end-line)
              for contents of-type simple-string = (line-contents cache line)
              do (loop with end-column = (if (= line end-line)
                                             (+ end-column*
                                                (if end-column-offset-p
                                                    end-column-offset
                                                    0))
                                             (length contents))
                       for column from text-start-column below end-column
                       for character = (aref contents column)
                       for (terminatingp punctuationp)
                          = (multiple-value-list (terminatingp character))
                       do (cond ((not terminatingp)
                                 (vector-push-extend character text))
                                (punctuationp
                                 (commit line column t)
                                 (vector-push-extend character text)
                                 (commit line (1+ column) nil))
                                (t
                                 (commit line column t)
                                 (incf text-start-column)))
                       finally (commit line column t))
                 (setf text-start-column 0))
        (nreverse wads)))))

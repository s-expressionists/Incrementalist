(cl:in-package #:incrementalist)

(defun whitespacep (character)
  (find character #(#\Space #\Tab #\Newline #\Page)))

(defun punctuationp (character)
  (find character #(#\. #\? #\! #\: #\, #\;
                    #\( #\) #\< #\> #\[ #\] #\{ #\}
                    #\" #\' #\` #\|
                    #\/ #\\ #\_ #\- #\+ #\* #\% #\= #\# #\~ #\@ #\&)))

(defun make-text-wad (text line start-column end-column line-length
                      min-length checkp)
  (declare (type (and string (not (or simple-array base-string))) text)
           (type alexandria:array-length min-length))
  (let ((length (length text)))
    (unless (zerop length) ; do not collect empty wads
      (cond ((every #'punctuationp text)
             (%basic-wad* 'punctuation-wad line start-column line end-column
                          line-length))
            ((and (>= length min-length)
                  (notany #'digit-char-p text))
             (let ((misspelledp (and checkp
                                     (null (spell:english-lookup text)))))
               (%basic-wad* 'word-wad line start-column line end-column
                            line-length
                            :misspelled misspelledp)))
            (t
             (%basic-wad* 'text-wad line start-column line end-column
                          line-length))))))

(defun make-text-wads (cache source
                       &key (start-column-offset 0)
                            (end-column-offset   0 end-column-offset-p)
                            (min-length          1))
  (destructure-source (start-line start-column end-line end-column*) source
    (let* ((text              (make-array 0 :element-type 'character
                                            :adjustable   t
                                            :fill-pointer 0))
           (text-start-column (+ start-column start-column-offset))
           (wads              '()))
      (flet ((terminatingp (character)
               (let ((spacep       (whitespacep character))
                     (punctuationp (punctuationp character)))
                 (values (or spacep punctuationp) punctuationp)))
             (commit (line column line-length checkp)
               (alexandria:when-let ((wad (make-text-wad
                                           text
                                           line text-start-column column
                                           line-length
                                           min-length checkp)))
                 (push wad wads))
               (setf (fill-pointer text) 0
                     text-start-column   column)))
        (loop for line from start-line to (if (zerop end-column*)
                                              (1- end-line)
                                              end-line)
              for contents of-type simple-string = (line-contents cache line)
              for line-length = (length contents)
              do (loop with end-column = (if (= line end-line)
                                             (+ end-column*
                                                (if end-column-offset-p
                                                    end-column-offset
                                                    0))
                                             line-length)
                       for column from text-start-column below end-column
                       for character = (aref contents column)
                       for (terminatingp punctuationp)
                          = (multiple-value-list (terminatingp character))
                       do (cond ((not terminatingp)
                                 (vector-push-extend character text))
                                (punctuationp
                                 (commit line column line-length t)
                                 (vector-push-extend character text)
                                 (commit line (1+ column) line-length nil))
                                (t
                                 (commit line column line-length t)
                                 (incf text-start-column)))
                       finally (commit line column line-length t))
                 (setf text-start-column 0))
        (nreverse wads)))))

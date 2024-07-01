(cl:in-package #:incrementalist)

;;; Position utilities

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

;;; Source utilities

(defmacro destructure-source
    ((start-line-var start-column-var
      &optional (end-line-var   (gensym "END-LINE")   end-line-supplied-p)
                (end-column-var (gensym "END-COLUMN") end-column-supplied-p))
     source &body body)
  `(destructuring-bind ((,start-line-var . ,start-column-var)
                        . (,end-line-var . ,end-column-var))
       ,source
     (declare (type alexandria:array-index
                    ,start-line-var ,start-column-var
                    ,end-line-var   ,end-column-var)
              ,@(unless end-line-supplied-p `((ignore ,end-line-var)))
              ,@(unless end-column-supplied-p `((ignore ,end-column-var))))
     ,@body))

;;; Result utilities

(defmacro %basic-wad* (class start-line start-column end-line end-column
                       max-line-width
                       &rest extra-initargs)
  `(make-instance ,class :cache               *cache*
                         :relative-p          nil
                         :absolute-start-line ,start-line
                         :start-line          ,start-line
                         :height              (- ,end-line ,start-line)
                         :start-column        ,start-column
                         :end-column          ,end-column
                         :max-line-width      ,max-line-width
                         ,@extra-initargs))

(defmacro basic-wad* (class stream start-line start-column end-line end-column
                      &rest extra-initargs)
  (alexandria:once-only (stream)
    (alexandria:with-unique-names (line-number max-line-width)
      `(let* ((,line-number    (line-number ,stream))
              (,max-line-width (compute-max-line-width
                                ,stream ,start-line ,line-number '())))
         (%basic-wad* ,class ,start-line ,start-column ,end-line ,end-column
                      ,max-line-width
                      ,@extra-initargs)))))

(defmacro basic-wad (class stream source &rest extra-initargs)
  (alexandria:with-unique-names (start-line start-column end-line end-column)
    `(destructure-source (,start-line ,start-column ,end-line ,end-column)
                         ,source
       (basic-wad* ,class ,stream
                   ,start-line ,start-column ,end-line ,end-column
                   ,@extra-initargs))))

(defmacro wad-with-children (class stream source children &rest extra-initargs)
  (alexandria:once-only (children)
    `(let ((result (basic-wad ,class ,stream ,source :children ,children
                              ,@extra-initargs)))
       (make-children-relative-and-set-family-relations result))))

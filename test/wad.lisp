(cl:in-package #:incrementalist.test)

(def-suite* :incrementalist.wad
  :in :incrementalist)

(test items.smoke
  "Smoke test for the `items' function."
  (let ((result (parse-result "1")))
    (let ((first (first result)))
      (is (string= "1" (inc:items first)))))

  (let ((result (parse-result #.(format nil "(a~%)(~%b)"))))
    (let ((first (first result)))
      (is (string= #.(format nil "(a~%)") (inc:items first))))
    (let ((second (second result)))
      (is (string= #.(format nil "(~%b)") (inc:items second))))))

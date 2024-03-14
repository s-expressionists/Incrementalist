(cl:in-package #:incrementalist)

;;; Symbol token protocol

(defgeneric package-marker-1 (symbol-token))

(defgeneric package-marker-2 (symbol-token))

(defgeneric package-name (symbol-token))

(defgeneric name (symbol-token))

;;; Children protocol

(defgeneric children (wad))

;;; Family relations protocol

(defgeneric parent (wad))

(defgeneric left-sibling (wad))

(defgeneric right-sibling (wad))

;;; Basic wad protocol

(defgeneric start-line (wad))

(defgeneric height (wad))

(defgeneric end-line (wad))

(defgeneric start-column (wad))

(defgeneric end-column (wad))

(defgeneric items (wad))

;;; Wad protocol
;;; extends basic wad protocol

(defgeneric errors (wad))

(defgeneric indentation (wad))

;;; Specialized wads

(defgeneric semicolon-count (semicolon-comment-wad))

(defgeneric expression (sharpsign-wad))

(defgeneric misspelled (word-wad))

(defgeneric condition (error-wad))

;;; Associated buffer protocol

;;; Return the Cluffer buffer associated with OBJECT.
(defgeneric buffer (object))

;;; Cache protocol
;;; Extends associated buffer protocol

(defgeneric time-stamp (cache))

;;; Given a cache, return the number of lines contained in the cache.
(defgeneric line-count (cache))

;;; Given a cache and a line number, return the number of items in the
;;; line with that line number.
(defgeneric line-length (cache line-number))

;;; Given a cache and a line number, return the contents of that line
;;; as a vector if items.
(defgeneric line-contents (cache line-number))

(defgeneric find-wad-beginning-line (cache line-number))

;;; Return a list of pairs of the form (N . W) where W is a wad, and N
;;; is the absolute start line number of W, of wads containing the
;;; position defined by LINE-NUMBER and COLUMN-NUMBER.  The list is
;;; ordered from the innermost wad containing the position to the
;;; top-level wad containing the position.  The empty list is returned
;;; if the position is inside no wad.
(defgeneric find-wads-containing-position (cache line-number column-number))

;;; Analyzer protocol
;;; Extends associated buffer protocol

(defgeneric cache (analyzer))

(defgeneric buffer (analyzer))

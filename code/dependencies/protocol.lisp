(cl:in-package #:incrementalist.dependencies)

;;; Validity protocol

(defgeneric validp (object))

;;; Cell protocol
;;; extends validity protocol

(defgeneric aspect (cell))

(defgeneric scope (cell))

(defgeneric value (cell))

(defgeneric map-users (function cell))

(defgeneric users (cell))

(defgeneric (setf validp) (new-value cell))

(defgeneric add-user (user cell))

(defgeneric remove-user (user cell))

;;; Cell user protocol
;;; extends validity protocol

(defgeneric valid-for-state-p (cell-user))

(defgeneric create-time (cell-user))

(defgeneric invalid (cell-user))

(defgeneric (setf invalid) (new-value cell-user))

(defgeneric used (cell-user))

(defgeneric (setf used) (new-value cell-user))

(defgeneric map-defined (function cell-user))

(defgeneric defined (cell-user))

(defgeneric add-defined (new-value cell-user))

(defgeneric map-escaped (function cell-user))

(defgeneric escaped (cell-user))

(defgeneric (setf escaped) (new-value cell-user))

(defgeneric map-external (function cell-user &key external-scopes))

(defgeneric inherited (cell-user))

(defgeneric (setf inherited) (new-value cell-user))

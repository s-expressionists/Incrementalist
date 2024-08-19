(cl:defpackage #:incrementalist.dependencies
  (:use
   #:cl)

  (:local-nicknames
   (#:a   #:alexandria)

   (#:dbg #:incrementalist.debug))

  ;; Types
  (:export
   #+incrementalist-debug #:list-of-cells
   #+incrementalist-debug #:list-of-cells-for-distinct-aspects)

  ;; Validity protocol
  (:export
   #:validp)

  ;; Cell protocol and classes
  (:export
   #:aspect
   #:scope
   #:value
   #:map-users
   #:users
   #:add-user
   #:remove-user

   #:cell
   #:globally-scoped-cell
   #:top-level-expression-scoped-cell
   #:lexically-scoped-cell

   #:make-cell)

  ;; Cell user protocol and mixin
  (:export
   #:valid-for-state-p
   #:create-time
   #:invalid   ; also `setf'
   #:used      ; also `setf'
   #:map-defined
   #:defined
   #:add-defined
   #:map-escaped
   #:escaped   ; also `setf'
   #:map-external
   #:inheritable
   #:inherited ; also `setf'

   #:add-dependencies

   #:update-escaping-for-toplevel-user

   #:cell-user-mixin)

  ;; Lexical state protocol
  (:export
   #:with-cells
   #:find-cell
   #:push-cell
   #:pop-cell
   #:pop-cell-until
   #:pop-local-cells

   #:install-state
   #:install-state-from-user)

  ;; Cell definition and use protocol
  (:export
   #:*defined-cells*
   #:*used-cells*
   #:*escaping-cells*

   #:register-definition
   #:make-and-register-cell
   #:register-use
   #:register-escaping

   #:with-cell-dependencies))

;;; shanks-model.el --- Object model

;;; Commentary:
;; 

;;; History:
;; 

(require 'cl)

;;; Code:

(defstruct shanks-model
  "The compiler/analyser environment."
  (packages (make-hash-table)) ;; symbol->shanks-package
  (packages-to-load)           ;; list of package-id symbols to be loaded
  (frames)                     ;; list of alists of local scopes
  (errors))                    ;; list of errors

(defstruct shanks-class
  "A record type for class information."
  (name)
  (parent)
  (interfaces)
  (mutable)
  (methods)
  (messages)
  (members))

(defstruct shanks-method
  "A record type for methods."
  (name)
  (class)
  (argument-types)
  (return-types))

(defstruct shanks-enum
  "A record type for enumerations."
  (id)
  (enumerators))

(defstruct shanks-package
  "A record type for packages."
  (enums (make-hash-table)) ;; symbol->shanks-enum
  (classes (make-hash-table)))

(defstruct shanks-error
  "A record type for compile-time errors."
  (line)
  (package-id)
  (message))

(provide 'shanks-model)

;;; shanks-model.el ends here

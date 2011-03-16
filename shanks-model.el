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
  (classes-to-load)            ;; list of (package-id class-id)
  (frames)                     ;; list of alists of local scopes
  (errors))                    ;; list of errors

(defstruct shanks-class
  "A record type for class information."
  (id)
  (base)
  (interfaces)
  (methods)
  (messages)
  (members))

(defstruct shanks-method
  "A record type for methods."
  (id)              ;; ID as a symbol
  (class)           ;; shanks-class
  (access)          ;; symbol, one of public, private, package
  (argument-types)  ;; list of (<type> <name>) [type-spec or shanks-type]
  (return-types))

(defstruct shanks-member
  "A record type for member variables."
  (id)              ;; ID as a symbol
  (class)           ;; shanks-class
  (access)          ;; symbol, one of public, private, package
  (type))           ;; type-spec or shanks-type

(defstruct shanks-enum
  "A record type for enumerations."
  (id)
  (enumerators))

(defstruct shanks-package
  "A record type for packages."
  (id)
  (enums (make-hash-table)) ;; symbol->shanks-enum
  (classes (make-hash-table)))

(defstruct shanks-reference
  "A record type for a reference (local variable or method parameter)."
  (type)
  (name))

(defstruct shanks-error
  "A record type for compile-time errors."
  (line)
  (package-id)
  (message))

(provide 'shanks-model)

;;; shanks-model.el ends here

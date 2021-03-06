;;; shanks-model.el --- Object model

;;; Commentary:
;; 

;;; History:
;; 

(require 'cl)

;;; Code:

(defstruct shanks-package
  "A record type for packages."
  (id)
  (packages (make-hash-table))   ;; symbol->shanks-package
  (enums (make-hash-table))      ;; symbol->shanks-enum
  (classes (make-hash-table))
  (names))                       ;; lowercase strings for clash detection

(defstruct shanks-model
  "The compiler/analyser environment."
  (root (make-shanks-package)) ;; root package
  (frames)                     ;; list of alists of local scopes
  (errors)                     ;; list of errors
  (to-load))                   ;; list of (package-spec class-id) to load

(defstruct shanks-class
  "A record type for class information."
  (id)
  (base)
  (interfaces)
  (methods)
  (messages)
  (members)
  (names))

(defstruct shanks-method
  "A record type for methods (and messages)."
  (id)              ;; ID as a symbol
  (class)           ;; shanks-class
  (access)          ;; symbol, one of public, private, package
  (arguments)       ;; list of (<type> <name>) [type-spec or shanks-type]
  (returns)
  (message)         ;; t, nil
  (source)          ;; AST for body
  (bytecode))

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

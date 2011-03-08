;;; shanks-types.el --- Type voodoo for an imaginary equine language

;;; Commentary:
;; 

;;; History:
;; 

(require 'cl)

;;; Code:

(defstruct shanks-cenv
  "The compiler/analyser environment."
  (modules (make-hash-table))
  (types (make-hash-table)))

(defstruct shanks-class
  "A record type for class information."
  (name)
  (parent)
  (interfaces)
  (methods)
  (messages)
  (members))

(defstruct shanks-method
  "A record type for methods."
  (name)
  (class)
  (arguments))

(provide 'shanks-types)

;;; shanks-types.el ends here

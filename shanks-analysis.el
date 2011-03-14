;;; shanks-types.el --- Type voodoo for an imaginary equine language

;;; Commentary:
;; 

;;; History:
;; 

(require 'cl)

;;; Code:

(defstruct shanks-cenv
  "The compiler/analyser environment."
  (packages (make-hash-table)) ;; symbol->shanks-package
  (packages-to-load)           ;; list of package-id symbols to be loaded
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

(defun shanks-pass-1! (cenv expression)
  (unless (and (consp expression)
               (eq (first expression) 'module))
    (error "Bad AST (expected module)"))
  (loop for item in (cdr expression) do
        (ecase (first item)
          ((import)
           'TODO)
          ((class)
           (shanks-scan-class! cenv item)))))

(defun shanks-scan-class! (cenv package-id expression)
  (let ((name (second expression))
        (parent (third expression))
        (interfaces (fourth expression))
        (mutable (fifth expression))
        (methods (sixth expression))
        (messages (seventh expression))
        (members (eighth expression)))
    ;; check if this name is already in use
    (let ((package (gethash package-id (shanks-cenv-packages cenv))))
      (if (gethash name (shanks-package-classes cenv))
          (push (format "Duplicate type ID %S" name)
                (shanks-cenv-errors cenv))
        TODO))))

(provide 'shanks-analysis)

;;; shanks-types.el ends here

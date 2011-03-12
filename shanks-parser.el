;;; shanks-parser.el --- Parsing routines for an imaginary equine language

;;; History:
;; 

;;; Commentary:
;; 

(require 'cl)

;;; STACKS

(defun shanks-list->stack (objects)
  "Make a mutable stack object using a list of OBJECTS."
  (cons objects nil))

(defun shanks-stack->list (stack)
  "Return a list of objects on STACK starting with the top."
  (car stack))

(defun shanks-peek (stack)
  "Return but do not consume the head item from STACK."
  (car (car stack)))

(defun shanks-pop! (stack)
  "Return and consume the top item from STACK destructively."
  (let* ((pair (car stack))
         (object (car pair)))
    (setf (car stack) (cdr pair))
    object))

(defun shanks-push! (object stack)
  "Push OBJECT onto STACK destructively."
  (setf (car stack) (cons object (car stack))))

;;; PARSER ENVIRONMENT

(defstruct shanks-pe
  "A record type for holding parser environment."
  (token-stack)
  (ast-stack)
  (current-line)
  (state-stack)
  (error-message))

(defun shanks-make-pe (tokens)
  "Make a new parser environment ready to consume a list of TOKENS."
  (make-shanks-pe :token-stack (shanks-list->stack tokens)
                  :ast-stack (shanks-list->stack '())
                  :current-line 1
                  :state-stack (shanks-list->stack '())))
  

(defun shanks-next! (pe)
  "Get the next token."
  ;; TODO consume special line number tokens and update line number
  (shanks-pop! (shanks-token-stack pe)))

(defun shanks-error! (pe message)
  "Set the error message in PE to MESSAGE."
  (setf (shanks-pe-error-message pe) message))

(defun shanks-begin! (pe)
  "Mark the beginning of an undoable transaction on PE."
  (shanks-push! (shanks-pe-current-line pe) (shanks-pe-state-stack pe))
  (shanks-push! (shanks-pe-token-stack pe)  (shanks-pe-state-stack pe))
  (shanks-push! (shanks-pe-ast-stack pe)    (shanks-pe-state-stack pe)))

(defun shanks-commit! (pe)
  "Commit a transaction on PE."
  (shanks-pop! (shanks-pe-state-stack pe))
  (shanks-pop! (shanks-pe-state-stack pe))
  (shanks-pop! (shanks-pe-state-stack pe)))

(defun shanks-rollback! (pe)
  "Undo everything done to PE since the last call to SHANKS-BEGIN! and return nil."
  (setf (shanks-pe-ast-stack pe)    (shanks-pop! (shanks-pe-state-stack pe))
        (shanks-pe-token-stack pe)  (shanks-pop! (shanks-pe-state-stack pe))
        (shanks-pe-current-line pe) (shanks-pop! (shanks-pe-state-stack pe)))
  nil)

;;; MISC

(defun shanks-title-case-symbol-p (symbol)
  (and (symbolp symbol)
       (let ((first-letter (elt (symbol-name symbol) 0)))
         (and (>= first-letter ?A)
              (<= first-letter ?Z)))))

(defun shanks-valid-package-name-p (symbol)
  (shanks-title-case-symbol-p symbol))

(defun shanks-valid-class-name-p (symbol)
  (shanks-title-case-symbol-p symbol))

;;; GRAMMAR

;;           <module> = <import-directive>* <declaration>* <EOF>
;; <import-directive> = "import" Name;
;;      <declaration> = <class-definition> | <immutable-definition>
;; <class-definition> = "class" Name <base-spec>? "{" <mmm-definition>* "}"
;; <immut-definition> = "immutable" Name <base-spec>? "{" <mmm-definition>* "}"
;;        <base-spec> = implements-spec? extends-spec?
;;  <implements-spec> = "implements" Name ("," Name)*
;;     <extends-spec> = "extends" NAME
;;   <mmm-definition> = <member-definition>
;;                    | <method-definition>
;;                    | <message-definition>
;;                    | <constructor-definition>
;;<member-definition> = Typename _name;
;;<method-definition> = <access-spec> name "(" <argument-list> ")" return-spec?
;;      <access-spec> = "public" | "private" | "package"
;;    <argument-list> =
;;                    | Typename name ("," <argument-list>)?
;;      <return-spec> = "->" "(" Typename name ")"

(defun shanks-parse-module! (pe)
  (shanks-begin! pe)
  (loop while (shanks-parse-import-directive! pe))
  (loop while (shanks-parse-declaration! pe))
  (if (shanks-end-of-stream? pe)
      (shanks-commit! pe)
    (shanks-error! "Unexpected token: %S" (shanks-peek pe))
    (shanks-rollback! pe)))

(defun shanks-parse-declaration! (pe)
  (or (shanks-parse-class-definition! pe)
      (shanks-parse-immutable-definition! pe)
      (shanks-parse-import-directive! pe)))

(defun shanks-parse-class-definition! (pe)
  (shanks-begin! pe)
  (if (eq (shanks-next! pe) 'T_CLASS)
      (let ((name (shanks-next! pe)))
        (if (shanks-valid-class-name-p next)
            (progn
              (shanks-parse-base-spec! pe)
              (if (eq (shanks-next! pe) 'T_LBRACE)
                  (if (shanks-parse-member-definitions pe)
                      (if (eq (shanks-peek pe) 'T_RBRACE)
                          (progn
                            (shanks-pop! pe)
                            (shanks-rollback! pe))
                        (shanks-rollback! pe))
                    (shanks-rollback! pe))))
          (shanks-rollback! pe)))
    (shanks-rollback! pe)))

(defun shanks-base-speck! (pe)
  (shanks-implements-spec! pe)
  (shanks-extends-spec! pe))

(defun shanks-implements-spec! (pe)
  (shanks-begin! pe)
  (if (eq (shanks-pop! pe) 'T_IMPLEMENTS)
      (progn
        (shanks-pop! pe)
        (let ((name (shanks-pop! pe)))
          (if (shanks-valid-package-name-p name)
              (p
    (shanks-rollback! pe)))

;; immutable-definition = immutable <A> [heirarchy-specification]
;;                        {
;;                            member-definitions
;;                        }

;; heirarchy-specification = [implements <A> [, <B> ...]] [extends <C>]

;; member-definitions = member-definition ...


(defun shanks-parse-import-directive! (pe)
  (shanks-begin! pe)
  (let ((keyword (shanks-next! pe)))
    (if (eq keyword 'T_IMPORT)
        (let ((package-name (shanks-next! pe)))
          (if (shanks-valid-package-name-p package-name)
              (let ((terminator (shanks-next! pe)))
                (if (eq 'T_TERMINATOR terminator)
                    (shanks-append! `(import ,package-name) pe)
                  (shanks-rollback! pe)))
            (shanks-rollback! pe)))
      (shanks-rollback! pe))))

(defun shanks-parse (tokens)
  "Parse TOKENS into an AST."
  (error "WRITE ME"))

(provide 'shanks-parser)

;;; shanks-parser.el ends here

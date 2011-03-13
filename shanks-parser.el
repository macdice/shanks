;;; shanks-parser.el --- Parsing routines for an imaginary equine language

;;; History:
;; 

;;; Commentary:
;; 
;; Here is an informal grammar for this language which I inferred from
;; some doodles I saw on some scraps of paper in a sushi restaurant.
;;
;; Before getting down to BNF, I understand that different kinds of
;; identifiers have different spelling restrictions (package names and
;; type names start with upper case, members start with underscore,
;; and other identifiers start with lower case).
;;
;;                <type-id> ~ /[A-Z][a-zA-Z0-9]*/
;;             <package-id> ~ /[A-Z][a-zA-Z0-9]*/
;;              <member-id> ~ /_[a-z][a-zA-Z0-9]*/
;;               <local-id> ~ /[a-z][a-zA-Z0-9]*/
;;              <method-id> ~ /[a-z][a-zA-Z0-9]*/
;;             <message-id> ~ /[a-z][a-zA-Z0-9]*/
;;
;; Here is an experimental pseudo-BNF grammar which may bear very
;; little resemblance to the grammar used by paid-up Pony Club
;; members:
;;
;;                 <module> = <import-directive>* 
;;                            <definition>* 
;;                            <EOF>
;;                          ;
;;       <import-directive> = "import" <package-id> ":" <type-id> ";"
;;                          | "import" <package-id> ";"
;;                          ;
;;             <definition> = <class-definition> 
;;                          | <enum-definition>
;;                          ;
;;       <class-definition> = <phylum> <type-id> <inheritence-spec>?
;;                            "{" 
;;                                <mmm-definition>* 
;;                            "}"
;;                          ;
;;                 <phylum> = "class" | "immutable" | "interface"
;;                          ;
;;       <inheritence-spec> = <implements-spec>? <extends-spec>?
;;                          ;
;;        <implements-spec> = "implements" <type-spec> ("," <type-spec>)*
;;                          ;
;;           <extends-spec> = "extends" <type-spec>
;;                          ;
;;         <mmm-definition> = <member-definition>
;;                          | <method-definition>
;;                          | <message-definition>
;;                          | <constructor-definition>
;;                          ;
;;      <member-definition> = <access-spec> <type-sec> <member-id> ";"
;;                          ;
;;              <type-spec> = <type-id> 
;;                          | <package-id> ":" <type-id>
;;                          ;
;;      <method-definition> = <access-spec> <method-id> "(" <argument-list> ")" return-spec?
;;                            "{"
;;                                <statement>*
;;                            "}"
;;                          ;
;;     <message-definition> = <access-spec> "message" <method-id> "(" <argument-list> ")"
;;                            "{"
;;                                <statement>*
;;                            "}"
;;                          ;
;; <constructor-definition> = <access-spec> "ambient"?
;;                            "new" <method-id> "(" <argument-list> ")" 
;;                            "{"
;;                                <statement>*
;;                            "}"
;;                          ;
;;            <access-spec> = "public" | "private" | "package"
;;                          ;
;;          <argument-list> =
;;                          | Typename name ("," <argument-list>)?
;;                          ;
;;            <return-spec> = "->" "(" <type-id> <local-id> ")"
;;                          ;
;;              <statement> = <simple-assignment> ";"
;;                          | <expression> ";"
;;                          | <jump-statement> ";"
;;                          | ";"
;;                          ;
;;      <simple-assignment> = <lvalue> <assignment-operator> <expression>
;;                          ;
;;                 <lvalue> = <local-id> | <member-id>
;;                          ;
;;         <jump-statement> = "return"
;;                          | "continue"
;;                          | "break"
;;                          ;
;;             <expression> = <call>
;;                          | <binary-expression>
;;                          ;
;;                   <call> = <local-id> "." name "(" <expression-list> ")"
;;                          ;
;;      <binary-expression> = <expression> <binary-operator> <expression>
;;                          ;
;;        <binary-operator> = "+" | "-" | "*" | "/" | "%"
;;                          ;

(require 'cl)

;;; STACKS

(defun shanks-list->stack (objects)
  "Make a mutable stack object using a list of OBJECTS."
  (cons objects nil))

(defun shanks-stack->list (stack)
  "Return a list of objects on STACK starting with the top."
  (car stack))

(defun shanks-stack-peek (stack)
  "Return but do not consume the head item from STACK."
  (car (car stack)))

(defun shanks-stack-pop! (stack)
  "Return and consume the top item from STACK destructively."
  (let* ((pair (car stack))
         (object (car pair)))
    (setf (car stack) (cdr pair))
    object))

(defun shanks-stack-push! (object stack)
  "Push OBJECT onto STACK destructively."
  (setf (car stack) (cons object (car stack))))

;;; PARSER ENVIRONMENT

(defstruct shanks-pe
  "A record type for holding parser environment."
  (token-stack)     ;; it's really more of a stream than a stack
  (work-stack)
  (current-line)
  (state-stack)
  (error-message))

(defun shanks-make-pe (tokens)
  "Make a new parser environment ready to consume a list of TOKENS."
  (make-shanks-pe :token-stack (shanks-list->stack tokens)
                  :work-stack (shanks-list->stack '())
                  :current-line 1
                  :state-stack (shanks-list->stack '())))
  

(defun shanks-next! (pe)
  "Get the next token."
  ;; TODO consume special line number tokens and update line number
  (shanks-stack-pop! (shanks-pe-token-stack pe)))

(defun shanks-peek (pe)
  (shanks-stack-peek (shanks-pe-token-stack pe)))

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

;;; IDENTIFIERS and their SPELLING RULES

(defun shanks-matching-symbol-p (regexp maybe-symbol)
  "Check if REGEXP matches MAYBE-SYMBOL, or return nil if it's not a symbol."
  (and (symbolp maybe-symbol)
       (let ((case-fold-search nil))
         (string-match-p regexp (symbol-name maybe-symbol)))))

(defun shanks-type-id-p (identifier)
  (shanks-matching-symbol-p "^[A-Z][a-zA-Z0-9]*$" identifier))

(defun shanks-package-id-p (identifier)
  (shanks-matching-symbol-p "^[A-Z][a-zA-Z0-9]*$" identifier))

(defun shanks-local-id-p (identifier)
  (shanks-matching-symbol-p "^[a-z][a-zA-Z0-9]*$" identifier))

(defun shanks-member-id-p (identifier)
  (shanks-matching-symbol-p "^_[a-zA-Z0-9]*$" identifier))

(defun shanks-method-id-p (identifier)
  (shanks-matching-symbol-p "^[a-z][a-zA-Z0-9]*$" identifier))

;;; RECURSIVE DESCENT

(defun shanks-parse-module! (pe)
  (shanks-begin! pe)
  (loop while (shanks-parse-import-directive! pe))
  (loop while (shanks-parse-definition! pe))
  (if (shanks-end-of-stream? pe)
      (shanks-commit! pe)
    (shanks-error! "Unexpected token: %S" (shanks-peek pe))
    (shanks-rollback! pe)))

(defun shanks-parse-definition! (pe)
  (or (shanks-parse-class-definition! pe)
      (shanks-parse-enum-definition! pe)))

(defun shanks-parse-class-definition! (pe)
  (error "TODO"))

(defun shanks-parse-call! (pe)
  nil)

(defun shanks-parse-literal! (pe)
  (let ((token (shanks-peek pe)))
    (if (or (numberp token)
            (stringp token))
        (progn
          (shanks-push! `(literal ,token) (shanks-pe-work-stack pe))
          (shanks-next! pe))
      nil)))

(defun shanks-parse-identifier! (pe)
    (let ((token (shanks-peek pe)))
      (if (or (shanks-local-id-p token)
              (shanks-member-id-p token))
          (progn
            (shanks-push! token (shanks-pe-work-stack pe))
            (shanks-next! pe))
        nil)))

(defun shanks-parse-binary-operator! (pe)
  (let ((token (shanks-peek pe)))
    (if (memq token '(+ - / *))
        (progn
          (shanks-push! token (shanks-pe-work-stack pe))
          (shanks-next! pe))
      nil)))

(defun shanks-parse-expression! (pe)
  ;; To avoid the problem of left recursion in a recursive descent
  ;; parser, we treat binary operators specially (and I don't know how
  ;; to represent this in the BNF above)
  (shanks-begin! pe)
  (if (or (shanks-parse-call! pe)
          (shanks-parse-identifier! pe)
          (shanks-parse-literal! pe))
      (let ((valid t))
        (while (and (shanks-parse-binary-operator! pe) valid)
          (if (or (shanks-parse-call! pe)
                  (shanks-parse-identifier! pe)
                  (shanks-parse-literal! pe))
              (let ((expr2 (shanks-pop! (shanks-pe-work-stack pe)))
                    (operator (shanks-pop! (shanks-pe-work-stack pe)))
                    (expr1 (shanks-pop! (shanks-pe-work-stack pe))))
                (shanks-push! `(,operator ,expr1 ,expr2)
                              (shanks-pe-work-stack pe)))
            (setf valid nil)))
        (if valid
            (shanks-commit! pe)
          (shanks-rollback! pe))
        valid)))

(defun shanks-parse (tokens)
  "Parse TOKENS representing one complete module producing an AST."
  (error "WRITE ME"))

(provide 'shanks-parser)

;;; shanks-parser.el ends here

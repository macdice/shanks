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

;;; PARSER ENVIRONMENT

(defstruct shanks-pe
  "A record type for holding parser environment."
  (tokens)
  (stack)
  (current-line)
  (state-stack)
  (error-message))

(defun shanks-next! (pe)
  "Get the next token."
  ;; TODO consume special line number tokens and update line number
  (pop (shanks-pe-tokens pe)))

(defun shanks-peek (pe)
  (car (shanks-pe-tokens pe)))

(defun shanks-error! (pe message)
  "Set the error message in PE to MESSAGE."
  (setf (shanks-pe-error-message pe) message))

;;; IDENTIFIERS and their SPELLING RULES

(defun shanks-matching-symbol-p (regexp maybe-symbol)
  "Check if REGEXP matches MAYBE-SYMBOL, or return nil if it's not a symbol."
  (and (symbolp maybe-symbol)
       (let ((case-fold-search nil))
         (string-match regexp (symbol-name maybe-symbol)))))

(defun shanks-type-id-p (identifier)
  (shanks-matching-symbol-p "^[A-Z][a-zA-Z0-9]*$" identifier))

(defun shanks-package-id-p (identifier)
  (shanks-matching-symbol-p "^[A-Z][a-zA-Z0-9]*$" identifier))

(defun shanks-package-or-type-id-p (identifier)
  (shanks-matching-symbol-p "^[A-Z][a-zA-Z0-9]*$" identifier))

(defun shanks-local-id-p (identifier)
  (shanks-matching-symbol-p "^[a-z][a-zA-Z0-9]*$" identifier))

(defun shanks-member-id-p (identifier)
  (shanks-matching-symbol-p "^_[a-zA-Z0-9]*$" identifier))

(defun shanks-method-id-p (identifier)
  (shanks-matching-symbol-p "^[a-z][a-zA-Z0-9]*$" identifier))

;;; RECURSIVE DESCENT

(defun shanks-parse-module! (pe)
  (loop while (shanks-parse-import-directive! pe))
  (loop while (shanks-parse-definition! pe))
  ;; TODO check that we are at the end of the file
  )

(defun shanks-parse-import-directive! (pe)
  (when (eq (shanks-peek pe) 'import)
    (error "TODO - import directive")))

(defun shanks-parse-definition! (pe)
  (or (shanks-parse-class-definition! pe)
      (shanks-parse-enum-definition! pe)))

(defun shanks-parse-enum-definition! (pe)
  (when (eq (shanks-peek pe) 'enum)
    (error "TODO - enum")))

(defun shanks-parse-class-definition! (pe)
  (let ((maybe-phylum (shanks-peek pe)))
    (when (memq maybe-phylum '(class immutable))
      ;; past this point, we consume from the stream, so we're always
      ;; going to return true but we may set errors in the parser
      ;; environment
      (shanks-next! pe)
      (let ((class-id (shanks-next! pe)))
        (shanks-parse-inheritence-spec! pe)
        (if (eq (shanks-peek pe) '{)
            (progn
              (shanks-next! pe)
              (loop while (shanks-parse-mmm-definition! pe))
              (if (eq (shanks-peek pe) '})
                  (progn
                    (shanks-next! pe)
                    t)
                (error "Expected statement or }")))
          (error "Expected {"))))))

(defun shanks-parse-inheritence-spec! (pe)
  ;; TODO
  nil)

(defun shanks-parse-mmm-definition! (pe)
  (or (shanks-parse-literal-definition! pe)
      (shanks-parse-member-definition! pe)
      (shanks-parse-method-or-message-definition! pe)))

(defun shanks-parse-literal-definition! (pe)
  (when (eq (shanks-peek pe) 'literal)
    (shanks-next! pe)
    (error "TODO literal")))

(defun shanks-parse-member-definition! (pe)
  (when (shanks-parse-type-spec! pe)
    (let ((member-id (shanks-next! pe)))
      (if (shanks-member-id-p member-id)
          (error "TODO -member")
        (error "Expected member name")))))

(defun shanks-parse-method-or-message-definition! (pe)
  (let ((access (shanks-peek pe)))
    (when (memq access '(public private package))
      (shanks-next! pe)
      ;; TODO look for message
      ;; TODO look for new
      ;; TODO look for ambiant
      (let ((id (shanks-next! pe)))
        (if (shanks-method-id-p id)
            (progn
              (error "TODO - method stuff"))
          (error "Expected ... TODO"))))))

(defun shanks-parse-type-spec! (pe)
  (when (shanks-package-or-type-id-p (shanks-peek pe))
    (let ((buffer nil)
          (more t))
      (while more
        (let ((id (shanks-next! pe)))
          (if (not (shanks-package-or-type-id-p id))
              (error "Expected package or type ID")
            (push id buffer)
            (let ((next (shanks-peek pe)))
              (if (eq next ':)
                  (shanks-next! pe)
                (setf more nil))))))
      (push (list (reverse (cdr buffer)) (car buffer))
            (shanks-pe-stack pe)))))

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
  "Parse TOKENS representing one complete source file producing an AST."
  (let ((pe (make-shanks-pe :tokens tokens)))
    (shanks-parse-module! pe)
    (error "TODO -- post-parse AST processing...")))

(provide 'shanks-parser)

;;; shanks-parser.el ends here

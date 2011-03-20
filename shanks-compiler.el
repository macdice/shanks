;; Compilation
;;
;; Here is what our ASTs look like:
;;
;; (package MyPackage
;;   (class MyClass
;;     (OtherPackage OtherClass)                    ;; base class
;;     ((OtherPackage InterfaceX))                  ;; interfaces
;;     (member public (OtherPackage FooClass)) _x)  ;; a member
;;     (member private U8 _y)                       ;; a member
;;     (method public                               ;; a method
;;       ((I32 r))                                  ;; return arguments
;;       (((OtherPackage FooClass) a)               ;; call arguments
;;        (I32 b))
;;       (block
;;         (= r (+ 1 b))))
;;
;; Some notes:
;;
;; * type specs look like (Package Class) for classes in a named
;;   package, Class for 'in this package', and U8 etc for certain
;;   built-in types that are not in any package you can name
;;
;; Overview of compilation process:
;;
;; * a root package and class are named by the user
;; * a new shanks-model is created with one type-specs-to-load
;; * parsing and analysis, while there is at least one type-specs-to-load
;; ** the class's source is found on the search path, and parsed
;; ** the parsed class is analysed with shanks-analyze
;; *** shanks-class, shanks-method, shanks-member etc objects are created
;; *** all unknown type-specs are added to type-specs-to-load
;; ** method and message bodies are scanned for type-spec references
;; * type-spec resolution
;; ** all types referenced in members, methods are resolved to shanks-class objects
;; * transformations
;; ** literal names replaced with their value
;; ** constant-folding
;; ** compile to bytecode

(defun shanks-error (model format &rest arguments)
  (push (apply #'format format arguments)
        (shanks-model-errors model)))

(defun shanks-find-or-create-package (model package-spec)
  "Return the package in MODEL which corresponds to PACKAGE-SPEC.
If no package exists matching the spec, then a new one is created.
A package spec is a list of package IDs.  A package ID is a symbol."
  (labels ((search (package package-spec)
    (if (null package-spec) 
        package
      (let* ((package-id (car package-spec))
             (child-package (gethash (car package-spec) 
                                     (shanks-package-packages package))))
        (if (null child-package)
            (let ((name-in-lower-case (downcase (symbol-name package-id)))
                  (child-package (make-shanks-package :id package-id)))
              (if (member name-in-lower-case (shanks-package-names package))
                  (shanks-error model 
                                "Can't define package %S due to name conflict"
                                package-id)
                (setf (gethash package-id (shanks-package-packages package))
                      child-package)
                (push name-in-lower-case (shanks-package-names package))
                (search child-package (cdr package-spec))))
          (search child-package (cdr package-spec)))))))
    (search (shanks-model-root model) package-spec)))

(defun shanks-analyze (model package-spec definition-expr)
  "Update MODEL with the results of analyzing EXPR.
This is the first phase of compilation, and builds our model of
classes and their members/methods/messages."
  (let ((package (shanks-find-or-create-package model package-spec)))
    (case (car definition-expr)
      ((class)
       (let ((class-id (second definition-expr))
             (base-class-spec (third definition-expr))
             (interfaces (fourth definition-expr)))
         (assert (symbolp class-id))
         (when (member (downcase (symbol-name class-id))
                       (shanks-package-names package))
           (error "Class %s already defined in package %s"
                  class-id
                  package-id))
         (let ((class (make-shanks-class :id class-id
                                         :base base-class-spec
                                         :interfaces interfaces)))
           (loop for subdef in (nthcdr 4 definition-expr) do
                 (case (car subdef)
                   ((member)
                    (push (make-shanks-member 
                           :access (second subdef)
                           :type (third subdef)
                           :id (forth subdef)
                           :class class)
                          (shanks-class-members class)))
                   ((method)
                    (push (make-shanks-method
                           :access (second subdef)
                           :arguments (third subdef)
                           :returns (fourth subdef)
                           :message nil
                           :class class)
                          (shanks-class-methods class)))
                   ((message)
                    (push (make-shanks-method
                           :access (second subdef)
                           :arguments (third subdef)
                           :returns nil
                           :message t
                           :class class)
                          (shanks-class-messages class)))))))))))

(defun shanks-resolve-types (model)
  "Update MODEL so that all argument types are resolved."
  TODO)

(defun shanks-range-check (model expr min max)
  (and (>= expr min)
       (<= expr max)))

(defun shanks-compile (model expr expected-type)
  (cond ((memq expr '(true false))
         (eq expected-type 'Bool))
        ((integerp expr)
         (case expected-type
           ((U8)
            (shanks-range-check model expr 0 255))
           ((U16)
            (shanks-range-check model expr 0 65535))
           (t (error "Unexpected integer literal"))))
        ((consp expr)
         (case (car expr)
           ((+ - * /)
            (and (shanks-type-check model (second expr) expected-type)
                 (shanks-type-check model (third expr) expected-type)))
           ((and or not)
            (and (eq expected-type 'Bool)
                 (loop for subexpr in (cdr expr)
                       always (shanks-type-check model subexpr 'Bool))))
           ((invoke)
            (let ((package-id (second expr))
                  (type-id (third expr)))

               (t (error "alsk"))))))))

(defun shanks-infer-type (model expr)
  (cond ((memq expr '(true false))
         'Bool)
        ((integerp expr)
         :undecided-integer)
        ((consp expr)
         (case (car expr)
           ((+ - * /)
            (error "TODO"))
           ((variable)
            (error "TODO"))
           ((invoke)
            (let ((object (second expr))
                  (method-id (third expr))
                  (arguments (cdddr expr)))
              (let ((object-type (shanks-infer-type model object)))
                (if (null object-type)
                    :unknown-type
                  (let ((return-types (shanks-class-return-types object-type)))
                    (if (= (length return-types) 1)
                        (first return-types)
                      (push (make-shanks-error :message "Expected single return value")
                            (shanks-model-errors model))))))
              (error "TODO")))))))

(defun shanks-find-source-path (search-paths package-spec class-id)
  "Search directories SEARCH-PATHS for a Pony file for PACKAGE-SPEC and CLASS-ID.
PACKAGE-SPEC is a list of package ID symbols, and CLASS-ID is a
symbol.  The return value is nil if the source file can't be
found, or the path as a string if it could."
  (let ((path (concat (mapconcat (lambda (package-id)
                                   (downcase (symbol-name package-id)))
                                 package-spec 
                                 "/")
                      "/"
                      (downcase (symbol-name class-id))
                      ".pony")))
    (loop for base in search-paths
          for maybe-path = (concat base "/" path)
          if (file-exists-p maybe-path)
          return maybe-path)))

(defun shanks-package-spec-and-class-id->string (package-spec class-id)
  "Produce a friendly string version of PACKAGE-SPEC and CLASS-ID
for messages."
  (concat (mapconcat #'symbol-name package-spec ":") ":" (symbol-name class-id)))

(defun shanks-compile-from-class (search-paths package-spec class-id)
  "Compile a named class, and everything it references, and return a model."
  (let ((model (make-shanks-model)))
    (push (list package-spec class-id) (shanks-model-to-load model))
    (while (and (null (shanks-model-errors model))
                (shanks-model-to-load model))
      (let* ((to-load (pop (shanks-model-to-load model)))
             (package-spec (first to-load))
             (class-id (second to-load))
             (path (shanks-find-source-path search-paths package-spec class-id)))
        (if (null path)
            (shanks-error model 
                          "Cannot find source for %s:%s" 
                          (shanks-package-spec-and-class-id->string package-spec
                                                                    class-id))
          (let ((tokens (shanks-lex-file path)))
            ;; TODO how to signal errors?
            (let ((definition-exprs (shanks-parse tokens)))
              ;; TODO how to signal errors? break loop on error?
              (loop for definition-expr in definition-exprs do 
                    (shanks-analyze model package-spec definition-expr)))))))))
;;; shanks-lexer.el --- Lexing routines for an imaginary equine language

;;; Commentary:
;; 
;; This is just a sketch...  pay no attention to it.

;;; History:
;; 

(require 'cl)

;;; Code:

(defun shanks-lex-string-literal (end)
  "Read a string literal up to END and return a Lisp string object."
  (forward-char)
  (let ((start (point))
        (result nil))
    ;; this is not very nice and not very Lispy and doesn't support
    ;; various important things
    (loop do
          (when (>= (point) end)
            (error "Past end of file while lexing string literal"))
          (let ((c (char-after)))
            (case c
              ((?\")
               (forward-char)
               (return (apply #'string (reverse result))))
              ((?\\)
               (forward-char)
               (push c result)
               (forward-char))
              (t
               (push c result)
               (forward-char)))))))
    
(defun shanks-looking-at-skip (pattern end)
  "If we are looking at PATTERN, skip it and return T."
  (when (looking-at pattern)
    (goto-char (match-end 0))
    t))

(defun shanks-lex-here (end)
  "Read one token at the current point, reading no further than END."
  ;; TODO TODO TODO -- unfinished!
  (let ((point (point)))
    (skip-chars-forward "[:space:]")
    ;; Here follows a totally inefficient way of lexing.  Gah...
    (cond ((>= point end)
           'end-of-file)
          ((looking-at "\"")
           (shanks-lex-string-literal end))
          ((looking-at "\\([0-9]+\\)\\>")
           (let ((result (string-to-int (match-string-no-properties 1))))
             (goto-char (match-end 0))
             result))
          ((looking-at "\\([0-9]+.[0-9]+\\)\\>")
           (let ((result (string-to-number (match-string-no-properties 1))))
             (goto-char (match-end 0))
             result))
          ((looking-at "import\\>")
           (goto-char (match-end 0))
           'T_IMPORT)
        
          ((looking-at "\\([a-zA-Z][a-zA-Z0-9]*\\)\\>")
           (let ((result (intern (match-string-no-properties 1))))
             (goto-char (match-end 0))
             result))
          ((shanks-looking-at-skip "!=" end) 'T_NE)
          ((shanks-looking-at-skip "import\\>" end) 'T_IMPORT)
          ((shanks-looking-at-skip "not\\>" end) 'T_NOT)
          ((shanks-looking-at-skip "and\\>" end) 'T_AND)
          ((shanks-looking-at-skip "/\\*" end)
           (search-forward "*/")
           (shanks-lex-here end))
          ((looking-at "//")
           (end-of-line))
          ;; TODO!
          (t
           (case (char-after)
             ((?\=) (forward-char) 'T_EQ)
             ((?\{) (forward-char) 'T_LBRACE)
             ((?\}) (forward-char) 'T_RBRACE)
             ((?\() (forward-char) 'T_LPAREN)
             ((?\)) (forward-char) 'T_RPAREN)
             ((?\[) (forward-char) 'T_LBRACKET)
             ((?\]) (forward-char) 'T_RBRACKET)
             ((?\<) (forward-char) 'T_LANGLE)
             ((?\>) (forward-char) 'T_RANGLE)
             ((?\.) (forward-char) 'T_DOT)
             ((?\;) (forward-char) 'T_TERMINATOR)
             (t (error "Cannot lex at line %d" (current-line))))))))

(defun shanks-lex-region (begin end)
  "Lex the region inside BEGIN, END and return a list of tokens."
  (save-excursion
    (goto-char begin)
    (loop for token = (shanks-lex-here end)
          until (eq token 'end-of-file)
          collect token)))

(provide 'shanks-lexer)

;;; shanks-lexer.el ends here

% File with definitions for exporting annotations to latex

#(use-modules (ice-9 regex))

% A list with characters that should be escaped when
% exporting to LaTeX. This works together with the below regexp
% and is used in the normalization function.
#(define latex-escape-pairs
   '(("\\" . "\\textbackslash ")
     ("&" . "\\&")
     ("{" . "\\{")
     ("}" . "\\}")
     ("[" . "{[}")
     ("]" . "{]}")
     ("#" . "\\#")
     ("\n\n" . "\\\\\n")))

latex-escape-regexpstring = "&|\\\\|\{|\}|\[|\]|#"
latex-escape-regexp = #(make-regexp latex-escape-regexpstring)

% LilyPond strings can contain stuff that is not accepted in LaTeX files,
% so we have to preprocess them)
#(define (escape-string-latex str)
   ;; Escape invalid LaTeX characters
   (set! str
         (regexp-substitute/global #f latex-escape-regexp str
           'pre (lambda (m)
                  (assoc-ref latex-escape-pairs (match:substring m)))
           'post))
   str)

#(define (indent-multiline-latex-string str)
   ;; make nice indentation
   (set! str
         (regexp-substitute/global #f "\n"
           (regexp-substitute/global #f "\n +" str
             'pre "\n" 'post)
           'pre "\n     " 'post))
   ;; "return" normalized string
   str)

% Return a string list for the "remaining" properties,
% formatted as a list of key=value arguments
#(define (format-latex-remaining-properties type props loc-props)
   (let ((cmd
          (assoc-ref annotation-type-latex-commands type))
         (props
          (map
           (lambda (p)
             (cons (car p)
               (if (ly:music? (cdr p))
                   "<LilyPond Music>"
                   (cdr p))))
           props))
         (result '()))
     ;; Start with LaTeX command
     (set! result
           (append-to-messages result
             (format "~a" cmd)))
     ;; First line of optional argument with opening bracket
     (set! result
           (append-to-messages result
             (format "   [~a={~a}," (car (first props)) (cdr (first props)))))
     (set! props (cdr props))
     ;; write all remaining properties as key=value pair
     (for-each
      (lambda (p)
        (set! result
              (append-to-messages result
                (format "    ~a={~a}," (car p) (cdr p)))))
      props)
     ;; properly close last entry
     (list-set! result (- (length result) 1)
       (format "~a]"
         (string-copy
          (last result)
          0
          (- (string-length (last result)) 1))))
     result))

% Generate and write annotations to a LaTeX file
#(define (export-annotations-latex)
   ;
   ; TODO::
   ; - implement configurable grouping options
   ; - output to separate files
   ; - using a function to compose the stringlist for each annotation
   ;   to enable different output targets
   ;   (eventually this should be template based).
   ;   Planned target formats are (potential order implementation):
   ;   - JSON
   ;   - plain text with textedit links ("Frescobaldi mode")
   ;   - markdown
   ;   - HTML
   ;   - PDF(??)
   ;

   ;; process annotations, adding lines to 'annotate-export-stringlist'
   (for-each
    ;
    ; TODO:
    ; This is the part that should be factored out
    ;
    (lambda (ann)
      (let*
       ((loc-props (annotation-location-properties ann))
        (rem-props (list-copy ann)))

       ;; Create a list rem-props with "remaining properties"
       ;; that are not used explicitly.
       (for-each
        (lambda (p)
          (set! rem-props (assoc-remove! rem-props p)))
        (list "type" "grob-type" "context-id"
          "message" "segment-name" "location" "basename"
          "measure-len" "beats-in-meter" "rhythmic-location"))

       ;; If there are remaining properties
       ;; output them to a key-value list as an optional argument
       ;; otherwise write a simple command
       (if (> (length rem-props) 0)
           (append-to-output-stringlist
            (format-latex-remaining-properties
             (assoc-ref ann "type") rem-props loc-props))
           ;; start entry with LaTeX command and rhythmic location
           (append-to-output-stringlist
            (format "~a"
              ;                     (format "~a{~a}{~a}"
              (assoc-ref annotation-type-latex-commands
                (assoc-ref ann "type")))))
       ;; output location arguments
       (append-to-output-stringlist
        (format "    {~a}{~a}"
          (assoc-ref loc-props "measure-no")
          ;
          ; TODO:
          ; beat-string isn't really suitable for the LaTeX export yet
          ;
          (assoc-ref loc-props "beat-string")))
       ;; Affected context
       (append-to-output-stringlist
        (format "    {~a}"
          (assoc-ref ann "context-id")))
       ;; Affected grob type
       (append-to-output-stringlist
        (format "    {~a}"
          (assoc-ref ann "grob-type")))
       ;; the actual message
       ;
       ; TODOsanitized-
       ; Format the message properly
       ;
       (append-to-output-stringlist
        (format "    {~a}"
          (indent-multiline-latex-string
           (escape-string-latex
            (assoc-ref ann "message")))))
       ;; add newline to annotation entry
       (append-to-output-stringlist " ")))
    annotations)

   ;; write to output file
   (write-output-file "inp"))


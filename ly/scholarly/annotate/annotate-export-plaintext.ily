%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Export annotations to plain text file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Generate and write annotations to one or more log files
#(define (do-log-annotations-plaintext)
   ;
   ; TODO::
   ; - implement configurable grouping options
   ; - output to separate files
   ; - using a function to compose the stringlist for each annotation
   ;   to enable different output targets
   ;   (eventually this should be template based).
   ;   Planned target formats are (potential order implementation):
   ;   - plain text (as already is possible)
   ;   - LaTeX (-> compatible with the to-be-written critical-report package)
   ;   - JSON
   ;   - plain text with textedit links ("Frescobaldi mode")
   ;   - markdown
   ;   - HTML
   ;   - PDF(??)
   ;

   ;; process annotations, adding lines to 'messages'
   (for-each
    ;
    ; TODO:
    ; This is the part that should be factored out
    ;
    (lambda (ann)
      (let* ((loc-props (annotation-location-properties ann)))
        ;; start entry with rhythmic-location
        (append-to-output-stringlist
         (format-location ann))
        ;; add annotation type
        (append-to-output-stringlist
         (assoc-ref annotation-type-labels
           (assoc-ref ann "type")))
        ;; print properties list
        (append-to-output-stringlist
         (format-property-messages ann
           (list "type" "context" "location""basename"
             "measure-len" "beats-in-meter" "rhythmic-location")))
        ;; add newline to annotation entry
        (append-to-output-stringlist " ")))
    annotations)

   ;; write to output file
   (write-output-file))


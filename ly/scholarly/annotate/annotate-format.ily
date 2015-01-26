
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% General routines for formatting output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Compose a message from the properties of an annotation
% The 'cmd' argument should be
% - ly:message or
% - append-message
% flt is a list of property names that should *not* be rendered
#(define (format-property-message prop)
   (let
    ((prop-key (car prop))
     (prop-value (cdr prop)))
    (format "    ~a: ~a"
      (or (assoc-ref annotation-property-labels prop-key)
          prop-key)
      ;; display a placeholder for music expressions
      ;; because these are cluttering the output.
      ;
      ; TODO
      ; maybe improve handling in the future
      ; and format messages for supported types like key signatures
      ;
      (if (ly:music? prop-value)
          "<LilyPond music>"
          prop-value))))

#(define (format-property-messages ann flt)
   (map (lambda (prop)
          (if (not (member (car prop) flt))
              (format-property-message prop)
              ""))
     ann))


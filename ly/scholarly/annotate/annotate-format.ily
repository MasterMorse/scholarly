
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% General routines for formatting output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Compose a message from the properties of an annotation
% The 'cmd' argument should be
% - ly:message or
% - append-message
% flt is a list of property names that should *not* be rendered
#(define (format-property-message prop)
   (format "    ~a: ~a"
     (or (assoc-ref annotation-property-labels (car prop))
         (car prop))
     (cdr prop)))

#(define (format-property-messages ann flt)
   (map (lambda (prop)
          (if (not (member (car prop) flt))
              (format-property-message prop)
              ""))
     ann))


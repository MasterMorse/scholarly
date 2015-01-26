
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% General routines for formatting output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define string representations for selected ly:music? data types.
% These are used for displaying custom properties.
#(define (format-ly-music music)
   (if (ly:music? music)
       (cond
        ((eq? 'TimeSignatureMusic (ly:music-property music 'name))
              (format "\\time ~a/~a"
                (ly:music-property music 'numerator)
                (ly:music-property music 'denominator)))
        ((eq? 'KeyChangeEvent (ly:music-property music 'name))
         (format "Key: ~a" (ly:music-property music 'tonic)))
         (else "<LilyPond Music>"))
         ;(else (format "~a" music)))
       "No music found"))


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
          (format-ly-music prop-value)
          prop-value))))

#(define (format-property-messages ann flt)
   (map (lambda (prop)
          (if (not (member (car prop) flt))
              (format-property-message prop)
              ""))
     (sort ann
       (lambda (a b)
         (string<? (car a) (car b))))))


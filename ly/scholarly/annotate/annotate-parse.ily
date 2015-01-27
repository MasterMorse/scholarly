%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions for the annotation engraver
% (provided by David Nalesnik)

% Return the musical/rhythmical position of a given grob
#(define (location grob)
   (if (ly:grob? grob)

   (let ((col (get-paper-column grob)))
     (if col
         (ly:grob-property col 'rhythmic-location)
         ;; get-paper-column returns #f if no column found
         ;; so we return an impossible value 'before the first beat'
         (cons 0 (ly:make-moment 0/4))))
   (ly:error "Requested rhythmic-location of a grob, but no grob provided")))


#(define (ly:moment<=? mom1 mom2)
   (or (ly:moment<? mom1 mom2)
       (equal? mom1 mom2)))

#(define (moment-floor num den)
   "Return the number of times den fits completely into num."
   (let loop ((result 1) (num num))
     (if (ly:moment<=? ZERO-MOMENT (ly:moment-sub num den))
         (loop (1+ result) (ly:moment-sub num den))
         result)))

% TODO: What's this?
#(define (compound? sig)
   "Determine if a meter is compound."
   (let ((num (car sig)))
     (cond
      ((= num 3) #f)
      ((= 0 (modulo num 3)) #t)
      (else #f))))

#(define (number-of-beats sig)
   (let ((num (car sig))
         (den (cdr sig)))
     (if (compound? sig)
         (/ num 3)
         num)))

% Return the length of one single "beat"
#(define (beat-length measure-length number-of-beats)
   (ly:moment-div measure-length (ly:make-moment number-of-beats)))

% Determine where a grob is in the horizontal course of the piece
#(define (get-paper-column grob)
   (cond
    ((not (ly:grob? grob)) #f)
    ((grob::has-interface grob 'paper-column-interface) grob)
    (else (get-paper-column
           ;; Can't use 'X' for axis because 'X' is also a music variable
           (ly:grob-parent grob 0)))))

% Calculate the rhythmic properties of an annotation
#(define (annotation-location-properties ann)
   (let* ((props '())
          (loc (assoc-ref ann "rhythmic-location"))
          (measure-pos (cdr loc))
          (beats-in-meter (assoc-ref ann "beats-in-meter"))
          (measure-len (assoc-ref ann "measure-len"))
          (beat-len (beat-length measure-len beats-in-meter))
          (our-beat (moment-floor measure-pos beat-len))
          (beat-part (ly:moment-sub
                      measure-pos
                      (ly:moment-mul
                       (ly:make-moment (1- our-beat))
                       beat-len)))
          (beat-fraction (moment->fraction
                          (ly:moment-div beat-part beat-len)))
          (beat-fraction (/ (car beat-fraction) (cdr beat-fraction)))
          (beat-string (number->string our-beat))
          (beat-string
           (if (= 0 beat-fraction)
               beat-string
               (string-append
                beat-string
                " "
                (number->string beat-fraction)))))
     (if (= 0 (car loc))
         (ly:input-warning
          (assoc-ref ann "location")
          "Sorry, location could not be determined."))
     ;
     ; TODO
     ; Do we need a property for the input-location and rhythmic location in here?
     ;
     (set! props (assoc-set! props "grob-type" (assoc-ref ann "grob-type")))
     (set! props (assoc-set! props "measure-no" (car loc)))
     (set! props (assoc-set! props "measure-pos" measure-pos))
     (set! props (assoc-set! props "beats-in-meter" beats-in-meter))
     (set! props (assoc-set! props "measure-len" measure-len))
     (set! props (assoc-set! props "beat-len" beat-len))
     (set! props (assoc-set! props "our-beat" our-beat))
     (set! props (assoc-set! props "beat-part" beat-part))
     (set! props (assoc-set! props "beat-fraction" beat-fraction))
     (set! props (assoc-set! props "beat-string" beat-string))
     ;; "return" props
     props)
   )


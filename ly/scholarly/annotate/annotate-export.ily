
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% General export helper routines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Provide a global string-list that output functions append their lines to.
% This is finally used to write to log files.
#(cond ((not (defined? 'annotate-export-stringlist))
        (define annotate-export-stringlist '())))

% Append a single string or a stringlist
% to the stringlist that is used for export
#(define (append-to-output-stringlist msg)
   (set! annotate-export-stringlist
         (append annotate-export-stringlist
           (if (list? msg)
               msg
               (list msg)))))

% Same as append-to-output-stringlist but
% operate on a given 'messages' argument.
#(define (append-to-messages messages msg)
   ;; append a single string or a string list
   (append messages
     (if (list? msg)
         msg
         (list msg))))

% As writing to a logfile uses (display)
% this is a nice shorthand (also necessary to be compatible with (write-lines)
#(define (display-line line)
   (display line)
   (newline))

% Take a string list and write it to an output using 'cmd'
% cmd could be 'display' or 'ly:message',
% basically any procedure that takes one string argument
#(define (write-lines msgs cmd)
   (for-each
    (lambda (m)
      ;; filtered properties are represented by empty strings
      ;; so we filter them out here.
      (if (not (string= m ""))
          (cmd m)))
    msgs))

% Take the stringlist 'annotate-export-stringlist
% and write it out to a file
#(define (write-output-file)
   ;
   ; TODO
   ; remove "messages" here and directly use the global object
   ;
   ; TODO
   ; Make the file name configurable and let it respect the target format
   ;
   (let* ((logfile (format "~A.annotations.log" annotation-out-basename)))
     (ly:message "writing '~A' ..." logfile)
     (with-output-to-file logfile
       (lambda ()
         (write-lines annotate-export-stringlist display-line)))))

% Format the rhythmic location of an annotation to a string
% used when printing to the console or exporting to plain text
#(define (format-location ann)
   (let ((props (annotation-location-properties ann))
         (loc (assoc-ref ann "rhythmic-location")))
     (if (= 0 (car loc))
         ;; workaround for a problem that sometimes the paperColumn gets
         ;; lost along the way. In that case the location is manually
         ;; set to measure zero - which is impossible.
         (format "Sorry, rhythmic position could not be determined.\nInput location at ~a"
           (assoc-ref ann "location"))
         (format "Measure ~a, beat ~a"
           (assoc-ref props "measure-no")
           (assoc-ref props "beat-string")))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Print annotations to console
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Print annotations to the console
#(define (do-print-annotations)
   (for-each
    (lambda (ann)
      (begin
       (ly:input-message (assoc-ref ann "location") "\nAnnotation:")
       (ly:message (format "    ~a" (format-location ann)))
       (write-lines
        (format-property-messages ann
          (list "type" "context" "location"
            "segment-name" "basename" "measure-len"
            "beats-in-meter" "rhythmic-location"))
        ly:message)
       (ly:message "")))
    annotations))



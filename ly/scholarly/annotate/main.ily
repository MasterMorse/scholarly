% Develop the interface to \annotate
% See https://github.com/openlilylib/lilypond-doc/wiki/Documenting-musical-content for discussion

%{
  \annotate
  Enter annotations directly in the music input
  Depending on configuration variables the engraver
  - colors affected objects depending on the type of annotation
  - prints a message on the console
  TODO:
  - generate clickable links when writing to file
  - enable the music function to apply editorial functions
    to the affected grob (e.g. dashing slurs, parenthesizing etc.).
    This has to be controlled by extra annotation properties
    and be configurable to a high degree (this is a major task).
  - provide an infrastructure for custom annotation types

  Credits:
  - \annotate etc.: Urs Liska, with help from lilypond-user
  - annotationEngraver: David Nalesnik, adapted by Urs Liska
  - custom grob properties: Paul Morris
%}

\version "2.17.18"

% Global object storing all annotations
#(define annotations '())

% Include factored out functionality
\include "annotate-configuration.ily"
\include "parse-rhythmic-location.ily"
\include "annotate-sort.ily"
\include "annotate-format.ily"
\include "annotate-export.ily"
\include "annotate-export-latex.ily"
\include "annotate-export-plaintext.ily"

% Define a lookup list for existing export procedures.
% While this might be expected to be defined in the configuration
% file it has to be inserted *after* the procedures have been defined
#(define export-routines
   `(("latex" . ,export-annotations-latex)
     ("plaintext" . ,export-annotations-plaintext)))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Helper functions to manage the annotation objects

% Predicate: an annotation is an alist that at least contains a number of
% default keys (which should usually be generated by the \annotate music function)
#(define (input-annotation? obj)
   (and
    (list? obj)
    (every pair? obj)
    (assoc-ref obj "message")
    (assoc-ref obj "type")
    (assoc-ref obj "location")))

% Retrieve the grob name from the annotation (provided by Harm)
#(define grob-name
   (lambda (x)
     (if (ly:grob? x)
         (assq-ref (ly:grob-property x 'meta) 'name)
         (ly:error "~a is not a grob" x))))

% Create custom grob properties
% Modified function from "scm/define-grob-properties.scm" (provided by Paul Morris)
#(define (cn-define-grob-property symbol type?)
   (set-object-property! symbol 'backend-type? type?)
   (set-object-property! symbol 'backend-doc "custom grob property")
   symbol)

% Create custom property 'annotation
% to pass information from the music function to the engraver
#(cn-define-grob-property 'input-annotation input-annotation?)



%%%%%%%%%%%%%%%%%%%%%
% Annotation engraver
% - Originally provided by David Nalesnik
% - Adapted to the existing \annotation function by Urs Liska

% Collector acknowledges annotations and appends them
% to the global annotations object
annotationCollector =
#(lambda (context)
   (let* ((grobs '()))
     (make-engraver
      ;; receive grobs with annotations, set a few more properties
      ;; and append annotation objects to the global annotations list
      (acknowledgers
       ((grob-interface engraver grob source-engraver)
        (let ((annotation (ly:grob-property grob 'input-annotation)))
          ;; A grob is to be accepted when 'annotation *does* have some content
          (if (and (not (null-list? annotation))
                   (not (member
                         (assoc-ref annotation "type")
                         ignored-annotation-types)))
              ;; add more properties that are only now available
              (let ((meter (ly:context-property context 'timeSignatureFraction))
                    (measure-length (ly:context-property context 'measureLength)))
                (begin
                 (if color-annotations
                     (set! (ly:grob-property grob 'color)
                           (assoc-ref annotation-colors
                             (assoc-ref annotation "type"))))
                 (if (or print-annotations export-annotations)
                     ;; only add to the list of grobs in the engraver
                     ;; when we actually process them afterwards
                     (set! grobs (cons (list grob meter measure-length annotation)
                                   grobs)))))))))
      ;; Iterate over collected grobs and produce a list of annotations
      ;; (when annotations are neither printed nor logged the list is empty).
      ((finalize trans)
       (begin
        (for-each
         (lambda (g)
           (let* ((annotation (last g))
                  (ctx-id
                   ;; Determine if there's
                   ;; a) an explicit context name defined or
                   ;; b) an implicit context name through the named Staff context
                   (or (assoc-ref annotation "context")
                       (annotation-context-label context))))
             ;; Here we add more properties that can only now be determined.
             ;; Even more detailed informations (properties) will later be
             ;; determined from these fields.
             (set! annotation (assoc-set! annotation "rhythmic-location" (location (first g))))
             (set! annotation (assoc-set! annotation "grob-type" (grob-name (car g))))
             (set! annotation (assoc-set! annotation "beats-in-meter" (number-of-beats (second g))))
             (set! annotation (assoc-set! annotation "measure-len" (third g)))
             ;; If there's a better label for the context overwrite the context-id property
             ;; which has originally been set to the directory name the input file is in
             ;; (because in some set-ups this is an indicator of the voice/part context).
             (if (or ctx-id (string=? ctx-id ""))
                 (set! annotation (assoc-set! annotation "context-id" ctx-id)))

             ;; add current annotation to the list of annotations
             (set! annotations (append annotations (list annotation)))))
         (reverse grobs)))))))


% When the score is finalized this engraver
% processes the list of annotations and produces
% appropriate output.
annotationProcessor =
#(lambda (context)
   (make-engraver
    ((finalize trans)
     ;; Sort annotations by the given criteria
     (for-each
      (lambda (s)
        (set! annotations
              (sort-annotations annotations
                (assoc-ref annotation-comparison-predicates s))))
      (reverse annotation-sort-criteria))

     ;; Optionally print annotations
     (if print-annotations
         (do-print-annotations))
     ;; Export iterating over all entries in the
     ;; annotation-export-targets configuration list
     (for-each
      (lambda (t)
        (let
         ((er (assoc-ref export-routines t)))
         ;; skip invalid entries
         (if er
             (er)
             (ly:warning (format "Invalid annotation export target: ~a" t)))))
      annotation-export-targets))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


annotate =
#(define-music-function (parser location name properties type item)
   ((symbol?) ly:context-mod? markup? symbol-list-or-music?)
   ;; annotates a musical object for use with lilypond-doc

   (let*
    ( ;; create empty alist to hold the annotation
      (props '())
      ;; retrieve a pair with containing directory and input file
      (input-file (string-split (car (ly:input-file-line-char-column location)) #\/ ))
      (ctx (list-tail input-file (- (length input-file) 2)))
      ;; extract directory name (-> part/voice name)
      (input-directory (car ctx))
      ;; extract segment name
      ; currently this is still *with* the extension
      (segment-name (cdr ctx)))

    ;; The "type" is passed as an argument from the wrapper functions
    ;; An empty string refers to the generic \annotation function. In this case
    ;; we don't set a type at all to ensure proper predicate checking
    ;; (the annotation must then have an explicit 'type' property)
    (if (not (string=? type ""))
        (set! props (assoc-set! props "type" type)))

    ;; Add or replace props entries taken from the properties argument
    (for-each
     (lambda (mod)
       (set! props
             (assoc-set! props
               (symbol->string (cadr mod)) (caddr mod))))
     (ly:get-context-mods properties))

    ;; pass along the input location to the engraver
    (set! props (assoc-set! props "location" location))

    ; also pass along our project-specific annotation properties
    ;; the 'context-id' property is the name of the musical context
    ;; the annotation refers to. As our fallthrough solution we
    ;; set this to the name of the enclosing directory
    (set! props
          (assoc-set! props
            "context-id"
            input-directory))
    ;
    ; TODO:
    ; this is a remnant from the Oskar Fried project.
    ; I'm not sure yet if that should be kept or not.
    ;
    (set! props
          (assoc-set! props
            "segment-name"
            segment-name))

    ;; Check if we do have a valid annotation,
    ;; then process it.
    (if (input-annotation? props)
        ;; Apply the annotation object as an override, depending on the input syntax
        (cond
         ((and (ly:music? item) (symbol? name))
          ;; item is music and name directs to a specific grob
          ;; annotate the named grob
          #{
            \tweak #`(,name input-annotation) #props #item
          #})
         ((ly:music? item)
          ;; item is music
          ;; -> annotate the music item (usually the NoteHead)
          #{
            \tweak #'input-annotation #props #item
          #})
         (else
          ;; item is a symbol list (i.e. grob name)
          ;; -> annotate the next item of the given grob name
          #{
            \once \override #item #'input-annotation = #props
          #}))
        (begin
         (ly:input-warning location "Improper annotation. Maybe there are mandatory properties missing?")
         #{ #}))))



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Public interface
%%%% Define one generic command \annotation
%%%% and a number of wrapper functions for different annotation types
%
% Annotations may have an arbitrary number of key=value properties,
% some of them being recognized by the system.
% A 'message' property is mandatory for all annotation types.

annotation =
% Generic annotation, can be used to "create" custom annotation types
% Note: a 'type' property is mandatory for this command
#(define-music-function (parser location name properties item)
   ((symbol?) ly:context-mod? symbol-list-or-music?)
   (if (symbol? name)
       #{ \annotate
          #name
          #properties
          ""
          #item #}
       #{ \annotate
          #properties
          ""
          #item #}))

criticalRemark =
% Final annotation about an editorial decision
#(define-music-function (parser location name properties item)
   ((symbol?) ly:context-mod? symbol-list-or-music?)
   (if (symbol? name)
       #{ \annotate
          #name
          #properties
          "critical-remark"
          #item #}
       #{ \annotate
          #properties
          "critical-remark"
          #item #}))

lilypondIssue =
% Annotate a LilyPond issue that hasn't been resolved yet
#(define-music-function (parser location name properties item)
   ((symbol?) ly:context-mod? symbol-list-or-music?)
   (if (symbol? name)
       #{ \annotate
          #name
          #properties
          "lilypond-issue"
          #item #}
       #{ \annotate
          #properties
          "lilypond-issue"
          #item #}))

musicalIssue =
% Annotate a musical issue that hasn't been resolved yet
#(define-music-function (parser location name properties item)
   ((symbol?) ly:context-mod? symbol-list-or-music?)
   (if (symbol? name)
       #{ \annotate
          #name
          #properties
          "musical-issue"
          #item #}
       #{ \annotate
          #properties
          "musical-issue"
          #item #}))

question =
% Annotation about a general question
#(define-music-function (parser location name properties item)
   ((symbol?) ly:context-mod? symbol-list-or-music?)
   (if (symbol? name)
       #{ \annotate
          #name
          #properties
          "question"
          #item #}
       #{ \annotate
          #properties
          "question"
          #item #}))

todo =
% Annotate a task that *has* to be finished
#(define-music-function (parser location name properties item)
   ((symbol?) ly:context-mod? symbol-list-or-music?)
   (if (symbol? name)
       #{ \annotate
          #name
          #properties
          "todo"
          #item #}
       #{ \annotate
          #properties
          "todo"
          #item #}))



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Set default integration in the layout contexts.
%%%% All settings can be overridden in individual scores.

\layout {
  % In each Staff-like context an annotation collector
  % parses annotations and appends them to the global
  % annotations object.
  \context {
    \Staff
    \consists \annotationCollector
  }
  \context {
    \DrumStaff
    \consists \annotationCollector
  }
  \context {
    \RhythmicStaff
    \consists \annotationCollector
  }
  \context {
    \TabStaff
    \consists \annotationCollector
  }
  \context {
    \GregorianTranscriptionStaff
    \consists \annotationCollector
  }
  \context {
    \MensuralStaff
    \consists \annotationCollector
  }
  \context {
    \VaticanaStaff
    \consists \annotationCollector
  }
  \context {
    \Dynamics
    \consists \annotationCollector
  }
  \context {
    \Lyrics
    \consists \annotationCollector
  }
  \context {
    \Score
    % The annotation processor living in the Score context
    % processes the annotations and outputs them to different
    % targets.
    \consists \annotationProcessor
  }
}


%{
  Define configuration variables and convenience functions for \annotate
%}

%%%%%%%%%%%%%%%%%%%%%%%
% fundamental behaviour
%%%%%%%%%%%%%%%%%%%%%%%

% String list predicate
#(define (stringlist? obj)
   (and (list? obj)
        (every string? obj)))

%%%%%%%%%%%%%%%%
% Console output

% initialize empty configuration variable
% By default annotations are printed to the console
#(cond ((not (defined? 'print-annotations))
        (define print-annotations #t)))

% Convenience function for switching annotation printing
% Specify ##t or ##f to switch on/off
printAnnotations =
#(define-scheme-function (parser location active)
   (boolean?)
   (set! print-annotations active))

%%%%%%%%%%%%%
% File export

% initialize empty configuration variable
#(cond ((not (defined? 'annotation-export-targets))
        (define annotation-export-targets '())))
#(define export-annotations #f)

% Convenience function to select output targets
% Provide a list with strings. These have to match
% an item in the export-routines alist defined in the main file.
setAnnotationExportTargets =
#(define-void-function (parser location targets)
   (stringlist?)
   (set! annotation-export-targets targets)
   (set! export-annotations #t))

%%%%%%%%%%%%%%%%%
% Limiting output

% Filter list to ignore annotation types before they are even created.
#(define ignored-annotation-types '())

% Convenience function to set the list of ignored annotation types.
% Strings passed in the stringlist argument should match existing
% annotation types, otherwise they simply don't have any effect.
ignoreAnnotationTypes =
#(define-void-function (parser location types)(stringlist?)
   (set! ignored-annotation-types types))


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Coloring annotations
%%%%%%%%%%%%%%%%%%%%%%%%%

% Switch coloring of annotations on/off.
% By default coloring is active
#(cond ((not (defined? 'color-annotations))
        (define color-annotations #t)))

% Convenience function for switching coloring annotations
% Specify ##t or ##f to switch on/off
colorAnnotations =
#(define-scheme-function (parser location active)
   (boolean?)
   (set! color-annotations active))

% default colors for annotations types
#(define annotation-color-defaults
   ;; Colors to be used for different types of annotation
   `(("critical-remark" . ,darkgreen)
     ("musical-issue" . ,green)
     ("lilypond-issue" . ,red)
     ("question" . ,blue)
     ("todo" . ,magenta)))

% initialize an empty alist for the annotation colors
#(cond ((not (defined? 'annotation-colors))
        (define annotation-colors '())))

% look up default annotation colors and set them
% if they aren't present yet
#(for-each
  (lambda (type)
    (if (not (assoc-ref annotation-colors (car type)))
        (set! annotation-colors
              (assoc-set! annotation-colors
                (car type) (cdr type)))))
  annotation-color-defaults)

% Convenience function to modify the colors for any annotation type.
% Expects:
% - string with annotation type (should match one of the key from
%   annotation-color-defaults above)
% - Scheme color
% TODO:
% This *should* work to seet the color of custom annotation types
% but this should be checked carefully.
% BTW what happens if a custom annotation type is used and *no*
% color is added? This should have a fallback without errors.
setAnnotationTypeColor =
#(define-void-function (parser location type color)
   (markup? color?)
   (set! annotation-colors
         (assoc-set! annotation-colors type color)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of annotation types for plain text output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for each known annotation type
% Used for plain text export
#(define annotation-type-labels-defaults
   ;; default labels to be used for output in logfiles
   `(("critical-remark" . "Critical Remark:")
     ("musical-issue" . "Musical Issue:")
     ("lilypond-issue" . "LilyPond Issue:")
     ("question" . "Question:")
     ("todo" . "TODO:")))

% initialize an empty alist for the annotation type labels
#(cond ((not (defined? 'annotation-type-labels))
        (define annotation-type-labels '())))

% look up default type labels and set them
% if they aren't present yet
#(for-each
  (lambda (type)
    (if (not (assoc-ref annotation-type-labels (car type)))
        (set! annotation-type-labels
              (assoc-set! annotation-type-labels
                (car type) (cdr type)))))
  annotation-type-labels-defaults)

% Convenience function to modify the labels for any annotation type.
% Expects:
% - string with annotation type (should match one of the key from
%   annotation-type-labels above)
% - string with new label
% TODO:
% This *should* work to seet the labels of custom annotation types
% but this should be checked carefully.
% BTW what happens if a custom annotation type is used and *no*
% label is added? This should have a fallback without errors.
setAnnotationTypeLabel =
#(define-void-function (parser location type label)
   (markup? markup?)
   (set! annotation-type-labels
         (assoc-set! annotation-type-labels type label)))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of annotation types for LaTeX output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for each known annotation type
% Used for LaTeX export
#(define annotation-type-latex-commands
   `(("critical-remark" . "\\criticalRemark")
     ("musical-issue" . "\\musicalIssue")
     ("lilypond-issue" . "\\lilypondIssue")
     ("question" . "\\annotateQuestion")
     ("todo" . "\\annotateTodo")))

% There is no implementation of convenience commands because this should
% not actually be necessary. The LaTeX implementation is designed to work
% together with the LaTeX package, so it should note be configured on user level.
% If the functionality has to be adapted to a given project the above alist
% can be modified directly.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of labels for property fields
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for annotation properties
% Used for console printing and plain text  export
#(define annotation-property-labels-defaults
   `(("message" . "Message")
     ("author" . "Author(s)")
     ("context-id" . "Context")
     ("source" . "Affected Source")
     ("voice-name" . "Voice")
     ("segment-name" . "File")
     ("grob-type" . "Affected Item")))

% initialize an empty alist for the annotation type labels
#(cond ((not (defined? 'annotation-property-labels))
        (define annotation-property-labels '())))

% look up default type labels and set them
% if they aren't present yet
#(for-each
  (lambda (type)
    (if (not (assoc-ref annotation-property-labels (car type)))
        (set! annotation-property-labels
              (assoc-set! annotation-property-labels
                (car type) (cdr type)))))
  annotation-property-labels-defaults)

% Convenience function to modify the labels for any annotation type.
% Expects:
% - string with property type (should match one of the key from
%   annotation-property-label-defaults above)
% - string with new label
% For custom properties that don't have a defined label
% the plain property name is used.
setAnnotationPropertyLabel =
#(define-void-function (parser location prop label)
   (markup? markup?)
   (set! annotation-property-labels
         (assoc-set! annotation-property-labels prop label)))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of labels for voice/context names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% \annotate tries to determine a proper name for the
% originating context. It does so by retrieving the
% context-id property of the Staff context enclosing
% the Voice context the annotation is entered in.
%
% TODO:
% This is not sufficiently reliable yet, I think.
%
% If this Staff is not named explicitly the directory
% name the input files is located in is used instead
% because in some set-ups this may also be an indicator
% for the musical part.
% However, for pretty-printing or localizing \annotate
% supports re-labelling of these context names.

% Initialize empty alist.
#(cond ((not (defined? 'annotation-context-labels))
        (define annotation-context-labels '())))

% Convenience function to add labels for context names.
% Supply an alist with one pair for each instrument,
% e.g. #'(("piano" . "Klavier"))
addAnnotationContextLabels =
#(define-void-function (parser location labels)
   (list?)
   (for-each
    (lambda (l)
      (set! annotation-context-labels
            (assoc-set! annotation-context-labels
              (car l) (cdr l))))
    labels))

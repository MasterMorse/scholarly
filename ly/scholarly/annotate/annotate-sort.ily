%%%%%%%%%%
% comparison operators for sorting annotations by different properties

#(define (get-rhythmic-location ann)
   "Retrieve 'rhythmic-location' from 'grob-location'"
   (assoc-ref
    (assoc-ref ann "grob-location")
    "rhythmic-location"))

% compare by rhythmic location
#(define (annotation-earlier? ann-a ann-b)
   (let*
    ((loc-a (get-rhythmic-location ann-a))
     (ma (car loc-a))
     (pa (cdr loc-a))
     (loc-b (get-rhythmic-location ann-b))
     (mb (car loc-b))
     (pb (cdr loc-b)))
    (cond
     ((< ma mb) #t)
     ((> ma mb) #f)
     (else (ly:moment<? pa pb)))))

% compare by author
#(define (annotation-less-than-by-author? ann-a ann-b)
   (string<?
    (assoc-ref ann-a "author")
    (assoc-ref ann-b "author")))

% compare by annotation type
#(define (annotation-less-than-by-type? ann-a ann-b)
   (string<?
    (assoc-ref ann-a "type")
    (assoc-ref ann-b "type")))

% Lookup list from which the predicate procedures are retrieved
#(define annotation-comparison-predicates
   `(("rhythmic-location" . ,annotation-earlier?)
     ("author" . ,annotation-less-than-by-author?)
     ("type" . ,annotation-less-than-by-type?)))

% Return a sorted list of annotations
%
% TODO:
% It has to be decided whether this will actually be needed.
% Currently it works quite well to iterate directly within the engraver.
% This function is only necessary when there will be more code inside.
%
#(define (sort-annotations annotations predicate)
   (stable-sort annotations predicate))


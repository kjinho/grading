#lang info
(define collection "grading")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/grading.scrbl" ())))
(define pkg-desc "tool to create PDF grade sheets")
(define version "0.0")
(define pkg-authors '(jinho))
(define license '(CC0))

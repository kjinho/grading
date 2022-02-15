#lang racket/base

(require (only-in yaml
                  string->yaml
                  file->yaml)
         (only-in racket/date
                  date->string
                  date-display-format)
         (only-in threading
                  ~>
                  ~>>)
         (only-in racket/match
                  match)
         (only-in racket/string
                  string-join
                  string-replace)
         (only-in racket/file
                  display-to-file
                  write-to-file)
         (only-in racket/system
                  system*))

(provide overwrite-flag
         pdflatex-command
         accumulate-scores
         calculate-grades
         latexify-student
         latexify-to-file
         render-grade-list
         save-total-scores!
         get-student-names
         process-name!)         

(define (mean xs)
  (let loop ((xs xs)
             (acc 0)
             (len 0))
    (cond ((and (null? xs)
                (= 0 len))
           +nan.0)
          ((null? xs)
           (/ acc len))
          (else
           (loop (cdr xs)
                 (+ acc (car xs))
                 (+ len 1))))))

(define (stddev xs)
  (let ((avg (mean xs)))
    (call-with-values
     (lambda ()
       (let loop ((xs xs)
                  (acc 0)
                  (len 0))
         (if (null? xs)
             (values acc len)
             (loop (cdr xs)
                   (+ (expt (- (car xs) avg) 2) acc)
                   (+ len 1)))))
     (lambda (acc len)
       (sqrt (/ acc len))))))
     
(module+ test
  (require (only-in math/statistics
                    (stddev stddev-m)
                    (mean mean-m))
           rackunit)
  (define ε 1e-10)
  (let ((test-data '(234 234 23  12 19 423 34834 9239)))
    (check-= (mean test-data)
             (mean-m test-data)
             ε)
    (check-= (stddev test-data)
             (stddev-m test-data)
             ε)))
  

(define overwrite-flag (make-parameter 'error))

(define (accumulate-scores yaml)
  (let* ((grades (hash-ref yaml "grades"))
         (grade-list (hash-map grades (lambda (k v) `(,k ,@v)))))
    (map (lambda (x)
           (cons
            (car x)
            (~>>
             (cdr x)
             (map hash-values)
             (apply append)
             (filter list?)
             (apply append)
             (map (lambda (y) (car (hash-values y))))
             (filter number?)
             (apply +))))
         grade-list)))

(define (calculate-stddev acc-scores)
  (stddev (map (lambda (x) (cdr x)) acc-scores)))

(define (deviations-from-mean->grade num)
  (cond ((< num -6)
         'C-)
        ((< num -5)
         'C)
        ((< num -4)
         'C+)
        ((< num -3)
         'B-)
        ((< num -2)
         'B)
        ((< num -1)
         'B+)
        ((< num 1)
         'A-)
        ((< num 2)
         'A)
        (else 'A+)))
        
;; returns a hash table of names to hash tables of
;; - 'score
;; - 'grade
;; - 'deviations
(define (calculate-grades acc-scores)
  (let ((stddevs (calculate-stddev acc-scores))
        (avg-score (mean (map (lambda (x) (cdr x)) acc-scores)))
        )
    (make-hash
     (map (lambda (x)
            (let* ((name (car x))
                   (score (cdr x))
                   (deviations-from-mean (/ (- score avg-score)
                                            stddevs))
                   (grade (deviations-from-mean->grade
                           deviations-from-mean)))
              (cons name
                    (hasheq 'score score
                            'deviations deviations-from-mean
                            'grade grade))))
          acc-scores))))

(define (latexify-category category)
  (match category
    ((hash-table (category-name list-of-criteria))                        
     (string-append 
      "\\noindent\\textbf{" category-name "}\n"
      "\\begin{itemize}\n"
      (string-join
       (map (lambda (h)
              (match h
                ((hash-table (k v))
                 (if (number? v)
                     (string-append "\\item " k "\\hfill " (number->string v) "\n")
                     ""))                 
                (_ (error "improperly formed category"))))
            list-of-criteria)
       "")
      "\\end{itemize}\n"
      "\\noindent\\textit{Comments}: "
      (let ((comments
             (filter (lambda (h) (hash-ref h "Comments" #f)) list-of-criteria)))
        (if (null? comments)
            "\n"
            (hash-ref (car comments) "Comments")))
      "\n\n"
      "\\noindent\\hrulefill"))
    (_ (error "argument is not a category"))))

(define (latexify-student yaml name)
  (let* ((assignment (hash-ref yaml "assignment"))
         (date (hash-ref yaml "date"))
         (student (hash-ref (hash-ref yaml "grades") name))
         (general-comments (hash-ref
                            (car
                             (filter (lambda (h)
                                       (match h
                                         ((hash-table (k v))
                                          #:when (string? v)
                                          #t)
                                         (_ #f)))
                                     student))
                            "Comments"))
         (other-categories (filter (lambda (h)
                                     (match h
                                       ((hash-table (k v))
                                        #:when (not (string? v))
                                        #t)
                                       (_ #f)))
                                   student))
         (grades (hash-ref (calculate-grades (accumulate-scores yaml))
                           name)))
    (string-append
     "\\documentclass[12pt,letterpaper]{article}\n"
     "\\usepackage{csquotes}\n"
     "\\title{" assignment " Grade Sheet}\n"
     "\\author{" name "}\n"
     "\\date{" (parameterize
                   ((date-display-format 'iso-8601))
                 (date->string date)) "}\n"
     "\\begin{document}\n"
     "\\maketitle\n\n"
     "\\begin{center}\\framebox{All criteria scored from 0 to 5.}\\end{center}\n\n"
     (string-join
      (map latexify-category other-categories)
      "\n\n")
     "\n\n\\noindent\\textbf{General Comments:}\n\n"
     general-comments
     "\n\n~~\\\\\n"
     "\\centering\\framebox{\n"
     "\\begin{tabular}{l l}\n"
     "Score: & " (number->string (hash-ref grades 'score)) "\\\\
StdDevs: & " (number->string (hash-ref grades 'deviations)) "\\\\
Grade: & " (symbol->string (hash-ref grades 'grade)) "\\\\
\\end{tabular}
}

\\end{document}"
     )))

(define (latexify-to-file yaml
                          name
                          #:exists (exists-flag (overwrite-flag))
                          #:filename (filename (path-add-extension name ".tex")))
  (let ((latex-contents (latexify-student yaml name)))
    (display-to-file latex-contents
                     filename
                     #:exists exists-flag)))

(define pdflatex-command (make-parameter "/Library/TeX/texbin/pdflatex"))

;; given a yaml and name, generates the PDF and cleans up
(define (process-name! yaml name)
  (let* ((pname (string-replace name " " "_"))
         (texname (path-add-extension pname ".tex"))
         (auxfiles (map (lambda (x) (path-add-extension pname x))
                        '(".log" ".aux"))))
    (begin
      (latexify-to-file yaml name #:filename texname)
      (system* (pdflatex-command) texname)
      (for ((del-file-names (cons texname auxfiles)))
        (delete-file del-file-names)))))

(define (get-student-names yaml)
  (~> (hash-ref yaml "grades")
      (hash-keys)))

(define (save-total-scores! yaml filename #:exists (exists-flag (overwrite-flag)))
  (write-to-file (calculate-grades (accumulate-scores yaml))
                   filename
                   #:exists exists-flag))

(define (render-grade-list yaml)
  (let ((assignment (hash-ref yaml "assignment"))
        (date (hash-ref yaml "date"))
        (grades (calculate-grades (accumulate-scores yaml))))
    (string-append "\\documentclass[12pt,letterpaper]{article}
\\usepackage{dcolumn}
\\title{" assignment " Grade Sheet (Totals)}
\\date{" (parameterize
             ((date-display-format 'iso-8601))
           (date->string date)) "}
\\author{Jin-Ho King}
\\begin{document}
\\maketitle

\\begin{tabular}{l r D{.}{.}{20} l}
\\textbf{Student} & \\textbf{Score} & \\multicolumn{1}{l}{\\textbf{Std.\\ Deviations}} & \\textbf{Grade} \\\\ \\hline
"
         (~>>
          (hash->list grades)
          ((lambda (x)
             (sort x string<? #:key car)))
          (map (lambda (x)
                (string-join
                 `(,(car x)
                   ,(number->string (hash-ref (cdr x) 'score))
                   ,(number->string (hash-ref (cdr x) 'deviations))
                   ,(symbol->string (hash-ref (cdr x) 'grade)))
                 " & ")))
          ((lambda (x)
             (string-join x "\\\\\n"))))       
         "
\\end{tabular}
\\end{document}")))


(module+ main
  (require racket/cmdline)

  (define total-path (make-parameter "all-grades.tex"))
  (define individual-grades (make-parameter #t))
  
  (define yaml-file
    (command-line
     #:program "grading"
     #:once-each
     (("-p" "--path-to-latex") path "Path to LaTeX" (pdflatex-command path))
     (("-f" "--force-overwrite") "Overwrite preexisting files"
                                 (overwrite-flag 'replace))
     #:once-any
     (("-t" "--totals") output-path "output totals"
                        (begin
                          (individual-grades #f)
                          (total-path output-path)))
     (("-i" "--individual-grades")
      "generate individual grades (default)"
      (begin
        (individual-grades #t)
        (total-path #f)))
     #:args (filename)
     filename))

  (if (and (total-path) (not (individual-grades)))
      (with-output-to-file 
          (total-path)
        (lambda () (display (render-grade-list (file->yaml yaml-file))))
        #:exists (overwrite-flag))
      (let* ((yaml (file->yaml yaml-file))
             (student-list (get-student-names yaml)))
        (for ((student student-list))
          (process-name! yaml student)))))
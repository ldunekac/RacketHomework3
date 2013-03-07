#lang racket

(define articles '(
     ((Test-Driven Learning: Intrinsic Integration of Testing into the CS/SE Curriculum)
      ((David Jansen)(Hossein Saiedian))
      ("Test-driven learning" "test-driven development" "extreme programming" "pedagogy" "CS1"))
     ((Process Improvement of Peer Code Review and Behavior Analysis of its Participants)
      ((WANG Yan-qing) (LI Yi-jun) (Michael Collins) (LIU Pei-jie))
      ("peer code review" "behavior analysis" "software quality assurance" 
        "computer science education" "software engineering"))
     ((Computer Games as Motivation for Design Patterns)
      ((Paul V. Gestwicki))
      ("Design Patterns" "Games" "Pedagogy" "Java"))
     ((Killer "Killer Examples" for Design Patterns)
      ((Carl Alphonce) (Michael Caspersen) (Adrienne Decker))
      ("Object-orientation" "Design Patterns"))
     ((Test-First Java Concurrency for the Classroom)
      ((Mathias Ricken)(Robert Cartwright))
      ("CS education" "Java" "JUnit" "unit testing" "concurrent programming"
       "tools" "software engineering"))
     ((Teaching Design Patterns in CS1: a Closed Laboratory Sequence
                based on the Game of Life)
      ((Michael Wick))
      ("Design Patterns" "Game of Life" "CS1" "Laboratory"))
   ))

(define (getTitle paper)
  (car paper))
(define (getAuthors paper)
  (cadr paper))
(define (getKeywords paper)
  (caddr paper))

(define (keywordSearch keyword papers)
 (cond  [(empty? papers) '()]
        [(member keyword (getKeywords (car papers))) 
           (cons (getTitle (car papers)) (keywordSearch keyword (cdr  papers)))]
        [else (keywordSearch keyword (cdr papers))]))

(define (makeMapFunction keyword)
  (lambda (paper) (if (member keyword (getKeywords paper))
                      (getTitle paper)
                      '())))
(define (containsKeyword keyword papers)
  (define (helper endList) 
    (cond [(empty? endList) '()]
          [(not (empty? (car endList))) (cons (car endList) (helper (cdr endList)))]
          [else (helper (cdr endList))]
          ))
  (helper (map (makeMapFunction keyword) papers)))

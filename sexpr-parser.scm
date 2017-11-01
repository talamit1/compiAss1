(load "pc.scm") 
 
(define <sexpr> 
  ;; fill in the s-expression parser details here 
  ) 


(define <digit0-9>
	(range #\0 #\9))

(define <digit1-9>
	(range #\1 #\9))

(define <nat>
  (new (*parser <digit1-9>)
       (*parser <digit0-9>) *star
       (*caten 2)

       (*parser (char #\0))
       (*parser (range #\0 #\9))
       *not-followed-by
       (*disj 2)
        (*pack 
       	(lambda (resList) (string->number (list->string (cons (car resList) (cadr resList)))) )
       )
       done))
(define <plus>
	(pack
		 (char #\+)
		 (lambda (res) ( string res))
			))
(define <minus>
	(pack
		 (char #\-)
		 (lambda (res) ( string res))
			))


(define <integer>
	(new 
		 (*parser <plus>)
		  (*parser <nat>) 
		  (*caten 2)

		  (*parser <minus>)
		  (*parser <nat>) 
		  (*caten 2)

		  (*parser <nat>)

		  (*disj 3)
		  (*pack toPrint)

		done)
	)

(define toPrint 
	(lambda (e) 
		(if (pair? e) (list->string (cons (car e) (cadr e)))
			e
		)
		))


(define a 5)



 (test-string <integer> "-4")

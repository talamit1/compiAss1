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


(define intToRet
	(lambda (sign int)
		(string->number (string-append sign (number->string int)))
		)
)

(define <integer>
	(new 
		 (*parser <plus>)
		  (*parser <nat>) 
		  (*caten 2)
		  (*pack-with intToRet)

		  (*parser <minus>)
		  (*parser <nat>) 
		  (*caten 2)
		  (*pack-with intToRet)

		  (*parser <nat>)

		  (*disj 3)
		   done
	)
)

(define <fraction>
	(new
		(*parser <integer>)
		(*parser (char #\/))
		(*pack 
			(lambda (ch)
				(string ch)))
		
		;(pack
		;	(char #\/)
		;	(lambda (ch) ( string ch))
		;	   )

		(*parser <nat>)
		
		(*caten 3)
		
		(*pack-with
			(lambda (int ch nat)
				(string->number (string-append (string-append (number->string int) ch) (number->string nat)))
				
				))    

		done))


(define <HexChar>
		(disj <digit0-9> (range #\a #\f))
)

(define <HexUnicodeChar>
	(caten (cahr '#\x') (star <HexChar>)  )
)

(define <visibleSimpleChar>
	(const (lambda (vh)  
		(char<=? 32 ch)
		)
	)
)
	








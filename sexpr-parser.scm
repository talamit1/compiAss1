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


(define append2
	(lambda (st1 st2 )
		(string-append (string st1) (string st2)))
	)


(define <boolean>
	(new
		(*parser (char-ci #\#))
		(*parser (char-ci #\t))
		(*caten 2)
		(*pack-with append2)
		
		(*parser (char-ci #\#))
		(*parser (char-ci #\f))
		(*caten 2)
		(*pack-with append2)
	

		(*disj 2)
	done)
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
		(*parser <nat>)
		(*caten 3)
		
		(*pack-with
			(lambda (int ch nat)
				( / int nat))
		)    

		done)
)


(define <HexChar>
		(disj <digit0-9> (range-ci #\a #\f) )
)

(define <HexUnicodeChar>
	(pack-with
	(caten (char-ci #\x) (plus <HexChar> ) )
	(lambda (x num)
		(integer->char (string->number (list->string num) 16) )
		)
	)
	
)

(define <VisibleSimpleChar>
	(const (lambda (ch)  
		(char<=? #\  ch)
		)
	)
)

(define <CharPrefix>
	(word "#\\")
)

(define buildNamed 
	(lambda (str ch)
		(pack
			(word-ci str) 
			(lambda (_) ch)
		)	
	)
)

(define <NamedChar>
	(new
		(*parser (buildNamed "lambda" (integer->char 955)))
		(*parser (buildNamed "nul" #\nul ))
		(*parser (buildNamed "tab" #\tab ))
		(*parser (buildNamed "newline" #\newline ))
		(*parser (buildNamed "return" #\return ))
		(*parser (buildNamed "page" #\page))
		(*parser (buildNamed "space" #\space))
		(*disj 7)
		done)	
)



(define <Char>
	(new 
		(*parser <CharPrefix>)
		(*parser <HexUnicodeChar>)
		(*parser <NamedChar>)
		(*parser <VisibleSimpleChar>)
		(*disj 3)
		(*caten 2)
		(*pack-with
			(lambda(charPref ch) ch))
		done
   	)
)

(define <InfixPrefixExtensionPrefix>
	(new 
		(*parser (word "##"))
		(*parser (word "#%"))
		(*disj 2)
	done)	
)

(define InfixSymbol
	(new 
		(*parser (char "#\+"))
		(*parser (char "#\-"))
		(*parser (char "#\*"))
		(*parser (char "#\/"))
		(*parser (char "#\^"))
		(*parser (word "**"))
		(*disj 6)
	done)
)

	
;(test-string <boolean> "#t")

(test-string <Char> "#t");
(test-string <Char> "#\\t");
(test-string <Char> "#\\x64")






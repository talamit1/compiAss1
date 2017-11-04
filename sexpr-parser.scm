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

;(test-string <Char> "#\\t");

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



(define <SymbolChar>
	(new
		(*parser <digit0-9>)
		(*parser (range-ci #\a #\z))
		(*parser (char #\!))
		(*parser (char #\$))
		(*parser (char #\^))
		(*parser (char #\*))
		(*parser (char #\-))
		(*parser (char #\_))
		(*parser (char #\=))
		(*parser (char #\+))
		(*parser (char #\>))
		(*parser (char #\<))
		(*parser (char #\?))
		(*parser (char #\/))
		(*disj 14)

	done)
	)


(define <symbol>	
	(new
		(*parser <SymbolChar>) *star
	
		(*pack
			(lambda (cha)
				(string->symbol (list->string cha )))
		)
	done
	)
)


(define <stringHexChar>
	(new
		(*parser (char #\\))
		(*parser <HexUnicodeChar>)
		(*parser (char #\;))
		(*caten 3)

		(*pack-with
			(lambda(strHex str semi) str))
	done
	)
)

(test-string <stringHexChar> "\\x61;") ;((match #\a) (remaining ""))


(define <StringMetaChar>

	(new
	(*parser (word-ci "\\\\"))
		(*pack (lambda (_) #\\))
	(*parser (word-ci "\\\""))
		(*pack (lambda (_) #\"))
	(*parser (word-ci "\\\t"))
		(*pack (lambda (_) #\tab))
	(*parser (word-ci "\\\f"))
		(*pack (lambda (_) #\page))
	(*parser (word-ci "\\\n"))
		(*pack (lambda (_) #\newline))
	(*parser (word-ci "\\\r"))
		(*pack (lambda (_) #\return))
	(*disj 6)
	done
	)
)

;(test-string <StringMetaChar> "\\\\")

(define <StringLiteralChar>
	(new (*parser <any>)
     	 (*parser (char #\\))
      	 (*parser (char #\"))
      	 (*disj 2)
      	*diff
	done)
)
;(test-string <StringLiteralChar> "hfh")

(define <StringChar>
(new 
	(*parser <StringLiteralChar>)
	(*parser <StringMetaChar>)
	(*parser <stringHexChar>)
	(*disj 3)

done)

)

(define <string>
	(new
		(*parser (char #\"))
		(*parser <StringChar>) *star
		(*parser (char #\"))

		(*caten 3)

		(*pack-with
			(lambda(opem string close) 
				(list->string string)))

	done)

)

(test-string <string> "\"\\x61;\\x63;\"")

;(test-string <SymbolChar> "1234abcd")
;(test-string <SymbolChar> "%33356")
;(test-string <symbol> "1234abcd")	
;(test-string <boolean> "#t")
;(test-string <Char> "#t");
;(test-string <stringHexChar> "#\\x23")

;(test-string <Char> "#\\x64")






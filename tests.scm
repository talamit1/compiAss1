(load "sexpr-parser.scm")

(define run-test
	(lambda (test expected)
		(cond
			((equal? test expected)
				(display "Success, Test result is: ")
				(display test)
				(newline)
				(display "The expected result was: ")
				(display expected))
			(else 
				(display "Failure, Test result is: ")
				(display test)
				(newline)
				(display "The expected result was: ")
				(display expected)))))

; (failed with report:)

;; TESTS HEADLINE
(display "=========================================================================")
(newline)
(display "========================== WELCOME TO TESTS!!! ==========================")
(newline)
(display "=========================== RAHMANA LIZLAN!!! ===========================")
(newline)
(display "==================== Programmer: Avraham Natan, 2017 ====================")
(newline)
(display "=========================================================================")
(newline)

#|
;; <Boolean> tests
(newline)
(display "=========================================================================")
(newline)
(display "==================== Tests for <Boolean> expressions: ===================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"#f\")")
(newline)
(run-test (test-string <sexpr> "#f") '((match #f) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#F\"))")
(newline)
(run-test (test-string <sexpr> "#F") '((match #f) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#t\")")
(newline)
(run-test (test-string <sexpr> "#t") '((match #t) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#T\"))")
(newline)
(run-test (test-string <sexpr> "#T") '((match #t) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"# f\")")
(newline)
(run-test (test-string <sexpr> "# f") '(failed with report:))
(newline)
(newline)

(display "> (test-string <sexpr> \" #F\"))")
(newline)
(run-test (test-string <sexpr> " #F") '((match #f) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#t \")")
(newline)
(run-test (test-string <sexpr> "#t ") '((match #t) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#Tg\"))")
(newline)
(run-test (test-string <sexpr> "#Tg") '((match #t) (remaining "g")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#t)\")")
(newline)
(run-test (test-string <sexpr> "#t)") '((match #t) (remaining ")")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#T}\"))")
(newline)
(run-test (test-string <sexpr> "#T}") '((match #t) (remaining "}")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#T.\"))")
(newline)
(run-test (test-string <sexpr> "#T.") '((match #t) (remaining ".")))
(newline)
(newline)
|#

#|
;; <Char> tests
(newline)
(display "=========================================================================")
(newline)
(display "===================== Tests for <Char> expressions: =====================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"#\\a\")")
(newline)
(run-test (test-string <sexpr> "#\\a") '((match #\a) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\r\"))")
(newline)
(run-test (test-string <sexpr> "#\\r") '((match #\r) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\ab\")")
(newline)
(run-test (test-string <sexpr> "#\\ab") '(failed with report:))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\\"))")
(newline)
(run-test (test-string <sexpr> "#\\") '(failed with report:))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\lambda\")")
(newline)
(run-test (test-string <sexpr> "#\\lambda") '((match #\λ) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\newline\")")
(newline)
(run-test (test-string <sexpr> "#\\newline") '((match #\newline) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\nul\")")
(newline)
(run-test (test-string <sexpr> "#\\nul") '((match #\nul) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\page\")")
(newline)
(run-test (test-string <sexpr> "#\\page") '((match #\page) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\RETURN\")")
(newline)
(run-test (test-string <sexpr> "#\\RETURN") '((match #\return) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\space\")")
(newline)
(run-test (test-string <sexpr> "#\\space") '((match #\space) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\tAb\")")
(newline)
(run-test (test-string <sexpr> "#\\tab") '((match #\tab) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\tabs\")")
(newline)
(run-test (test-string <sexpr> "#\\tabs") '(failed with report:))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\x61\")")
(newline)
(run-test (test-string <sexpr> "#\\x61") '((match #\a) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#\\x5d0\")")
(newline)
(run-test (test-string <sexpr> "#\\x5d0") '((match #\א) (remaining "")))
(newline)
(newline)
|#

#|
;; <Number> tests
(newline)
(display "=========================================================================")
(newline)
(display "==================== Tests for <Number> expressions: ====================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"13\")")
(newline)
(run-test (test-string <sexpr> "13") '((match 13) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"13t\")")
(newline)
(run-test (test-string <sexpr> "13t") '((match 13t) (remaining ""))) ;; Actual printout: ((match \x31;3t) (remaining ""))
(newline)
(newline)

(display "> (test-string <sexpr> \" 13\")")
(newline)
(run-test (test-string <sexpr> " 13") '((match 13) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \" 13   \")")
(newline)
(run-test (test-string <sexpr> " 13   ") '((match 13) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \" 13.\")")
(newline)
(run-test (test-string <sexpr> " 13.") '((match 13) (remaining ".")))
(newline)
(newline)

(display "> (test-string <sexpr> \" 13)\")")
(newline)
(run-test (test-string <sexpr> " 13)") '((match 13) (remaining ")")))
(newline)
(newline)

(display "> (test-string <sexpr> \" 13}\")")
(newline)
(run-test (test-string <sexpr> " 13}") '((match 13) (remaining "}")))
(newline)
(newline)

(display "> (test-string <sexpr> \"13?\")")
(newline)
(run-test (test-string <sexpr> "13?") '((match 13?) (remaining ""))) ;; Actual printout: ((match \x31;3?) (remaining ""))
(newline)
(newline)

(display "> (test-string <sexpr> \" 13/\")")
(newline)
(run-test (test-string <sexpr> " 13/") '((match 13/) (remaining ""))) ;; Actual printout: ((match \x31;3/) (remaining ""))
(newline)
(newline)

(display "> (test-string <sexpr> \" 13/ \")")
(newline)
(run-test (test-string <sexpr> " 13/ ") '((match 13/) (remaining ""))) ;; Actual printout: ((match \x31;3/) (remaining ""))
(newline)
(newline)

(display "> (test-string <sexpr> \"-13\")")
(newline)
(run-test (test-string <sexpr> "-13") '((match -13) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"+13\")")
(newline)
(run-test (test-string <sexpr> "+13") '((match 13) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"013\")")
(newline)
(run-test (test-string <sexpr> "013") '((match 13) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"13/39\")")
(newline)
(run-test (test-string <sexpr> "13/39") '((match 1/3) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"13/0\")")
(newline)
(write (test-string <sexpr> "13/0")) ;; should be: ((match \x31;3/0) (remaining ""))
(newline)
(newline)

(display "> (test-string <sexpr> \"+3/5\")")
(newline)
(run-test (test-string <sexpr> "+3/5") '((match 3/5) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"-10/5\")")
(newline)
(run-test (test-string <sexpr> "-10/5") '((match -2) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"13/2\")")
(newline)
(run-test (test-string <sexpr> "13/2") '((match 13/2) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"13/-39\")")
(newline)
(run-test (test-string <sexpr> "13/-39") '((match 13/-39) (remaining ""))) ;; Actual printout: ((match \x31;3/-39) (remaining ""))
(newline)
(newline)

(display "> (test-string <sexpr> \"1se3\")")
(newline)
(run-test (test-string <sexpr> "1se3") '((match 1se3) (remaining ""))) ;; Actual printout: ((match \x31;se3) (remaining ""))
(newline)
(newline)

(display "> (test-string <sexpr> \"1f3/45\")")
(newline)
(run-test (test-string <sexpr> "1f3/45") '((match 1f3/45) (remaining ""))) ;; Actual printout: ((match \x31;f3/45) (remaining ""))
(newline)
(newline)

(display "> (test-string <sexpr> \"13/4g5\")")
(newline)
(run-test (test-string <sexpr> "13/4g5") '((match 13/4g5) (remaining ""))) ;; Actual printout: ((match \x31;3/4g5) (remaining ""))
(newline)
(newline)
|#

#|
;; <String> tests
(newline)
(display "=========================================================================")
(newline)
(display "==================== Tests for <String> expressions: ====================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"\"a\"\")")
(newline)
(run-test (test-string <sexpr> "\"a\"") '((match "a") (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"\"\\x61;\"\")")
(newline)
(run-test (test-string <sexpr> "\"\\x61;\"") '((match "a") (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"\"\\x61;\\x63;\"\")")
(newline)
(run-test (test-string <sexpr> "\"\\x61;\\x63;\"") '((match "ac") (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"\\\\\")")
(newline)
(run-test (test-string <sexpr> "\"\\\\\"") '((match "\\") (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"\"\\\"\"\")")
(newline)
(run-test (test-string <sexpr> "\"\\\"\"") '((match "\"") (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"\"\t\"\")")
(newline)
(run-test (test-string <sexpr> "\"\t\"") '((match "\t") (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"\"\f\"\")")
(newline)
(run-test (test-string <sexpr> "\"\f\"") '((match "\f") (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"\"\n\"\")")
(newline)
(run-test (test-string <sexpr> "\"\n\"") '((match "\n") (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"\"\r\"\")")
(newline)
(run-test (test-string <sexpr> "\"\r\"") '(test-string <sexpr> "\"\r\""))
(newline)
(newline)

(display "> (test-string <sexpr> \"\"cyka\"\")")
(newline)
(run-test (test-string <sexpr> "\"cyka\"") '((match "cyka") (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"\"boris s\\x61;ys: \\\"OPA BLYAT!\\\"\"\")")
(newline)
(run-test (test-string <sexpr> "\"boris s\\x61;ys: \\\"OPA BLYAT!\\\"\"") '((match "boris says: \"OPA BLYAT!\"") (remaining "")))
(newline)
(newline)
|#

#|
;; <Symbol> tests
(newline)
(display "=========================================================================")
(newline)
(display "==================== Tests for <Symbol> expressions: ====================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"a\")")
(newline)
(run-test (test-string <sexpr> "a") '((match a) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"A\")")
(newline)
(run-test (test-string <sexpr> "A") '((match a) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"8\")")
(newline)
(run-test (test-string <sexpr> "8") '((match 8) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"!\")")
(newline)
(run-test (test-string <sexpr> "!") '((match !) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"$\")")
(newline)
(run-test (test-string <sexpr> "$") '((match $) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"^\")")
(newline)
(run-test (test-string <sexpr> "^") '((match ^) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"*\")")
(newline)
(run-test (test-string <sexpr> "*") '((match *) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"-\")")
(newline)
(run-test (test-string <sexpr> "-") '((match -) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"_\")")
(newline)
(run-test (test-string <sexpr> "_") '((match _) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"=\")")
(newline)
(run-test (test-string <sexpr> "=") '((match =) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"+\")")
(newline)
(run-test (test-string <sexpr> "+") '((match +) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"<\")")
(newline)
(run-test (test-string <sexpr> "<") '((match <) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \">\")")
(newline)
(run-test (test-string <sexpr> ">") '((match >) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"?\")")
(newline)
(run-test (test-string <sexpr> "?") '((match ?) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"/\")")
(newline)
(run-test (test-string <sexpr> "/") '((match /) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"num\")")
(newline)
(run-test (test-string <sexpr> "num") '((match num) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"nAn1\")")
(newline)
(run-test (test-string <sexpr> "nAn1") '((match nan1) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"2fast2furious\")")
(newline)
(run-test (test-string <sexpr> "2fast2furious") '((match 2fast2furious) (remaining ""))) ;; Actual printout: ((match \x32;fast2furious (remaining ""))
(newline)
(newline)

(display "> (test-string <sexpr> \"!<anseiD0rifto\")")
(newline)
(run-test (test-string <sexpr> "!<anseiD0rifto") '((match !<anseid0rifto) (remaining "")))
(newline)
(newline)
|#

#|
;; <ProperList> tests
(newline)
(display "=========================================================================")
(newline)
(display "================== Tests for <ProperList> expressions: ==================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"()\")")
(newline)
(run-test (test-string <sexpr> "()") '((match ()) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(#f #t #t)\")")
(newline)
(run-test (test-string <sexpr> "(#f #t #t)") '((match (#f #t #t)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 2 3)\")")
(newline)
(run-test (test-string <sexpr> "(1 2 3)") '((match (1 2 3)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 (2 3))\")")
(newline)
(run-test (test-string <sexpr> "(1 (2 3))") '((match (1 (2 3))) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 (2 3) \"hi\")\")")
(newline)
(run-test (test-string <sexpr> "(1 (2 3) \"hi\")") '((match (1 (2 3) "hi")) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 (2) 3)\")")
(newline)
(run-test (test-string <sexpr> "(1 (2) 3)") '((match (1 (2) 3)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 (2 (5 5 \"hi\")) 3)\")")
(newline)
(run-test (test-string <sexpr> "(1 (2 (5 5 \"hi\")) 3)\")") '((match (1 (2 (5 5 "hi")) 3)) (remaining "\")")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 (2) !)\")")
(newline)
(run-test (test-string <sexpr> "(1 (2) !)") '((match (1 (2) !)) (remaining "")))
(newline)
(newline)
|#

#|
;; <ImproperList> tests
(newline)
(display "=========================================================================")
(newline)
(display "================= Tests for <ImproperList> expressions: =================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"(1 . 2)\")")
(newline)
(run-test (test-string <sexpr> "(1 . 2)") '((match (1 . 2)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 2 . 5)\")")
(newline)
(run-test (test-string <sexpr> "(1 2 . 5)") '((match (1 2 . 5)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 .     2)\")")
(newline)
(run-test (test-string <sexpr> "(1 .     2)") '((match (1 . 2)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"((1 2) . 3)\")")
(newline)
(run-test (test-string <sexpr> "((1 2) . 3)") '((match ((1 2) . 3)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 . (2 3))\")")
(newline)
(run-test (test-string <sexpr> "(1 . (2 3))") '((match (1 2 3)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 .((2 3) \"hi\"))\")")
(newline)
(run-test (test-string <sexpr> "(1 .((2 3) \"hi\"))") '((match (1 (2 3) "hi")) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 . (2) 3)\")")
(newline)
(run-test (test-string <sexpr> "(1 . (2) 3)") '(failed with report:))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 .((2) 3))\")")
(newline)
(run-test (test-string <sexpr> "(1 .((2) 3))") '((match (1 (2) 3)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1     .   (2 (5 5 \"hi\")) 3)\")")
(newline)
(run-test (test-string <sexpr> "(1     .   (2 (5 5 \"hi\")) 3)\")") '(failed with report:))
(newline)
(newline)

(display "> (test-string <sexpr> \"(1 .((2) (!)))\")")
(newline)
(run-test (test-string <sexpr> "(1 .((2) (!)))") '((match (1 (2) (!))) (remaining "")))
(newline)
(newline)
|#

#|
;; <Vector> tests
(newline)
(display "=========================================================================")
(newline)
(display "==================== Tests for <Vector> expressions: ====================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"#()\")")
(newline)
(run-test (test-string <sexpr> "#()") '((match #()) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#(#f #t #t)\")")
(newline)
(run-test (test-string <sexpr> "#(#f #t #t)") '((match #(#f #t #t)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#(1 2 3)\")")
(newline)
(run-test (test-string <sexpr> "#(1 2 3)") '((match #(1 2 3)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#(1 (2 3))\")")
(newline)
(run-test (test-string <sexpr> "#(1 (2 3))") '((match #(1 (2 3))) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#(1 (2 3) \"hi\")\")")
(newline)
(run-test (test-string <sexpr> "#(1 (2 3) \"hi\")") '((match #(1 (2 3) "hi")) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#(1 (2) 3)\")")
(newline)
(run-test (test-string <sexpr> "#(1 (2) 3)") '((match #(1 (2) 3)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#(1 (2)    3)\")")
(newline)
(run-test (test-string <sexpr> "#(1 (2)    3)") '((match #(1 (2) 3)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#(1 (2 (5 5 \"hi\")) 3)\")")
(newline)
(run-test (test-string <sexpr> "#(1 (2 (5 5 \"hi\")) 3)\")") '((match #(1 (2 (5 5 "hi")) 3)) (remaining "\")")))
(newline)
(newline)

(display "> (test-string <sexpr> \"#(1 (2) !)\")")
(newline)
(run-test (test-string <sexpr> "#(1 (2) !)") '((match #(1 (2) !)) (remaining "")))
(newline)
(newline)
|#

#|
;; <Quoted> tests
(newline)
(display "=========================================================================")
(newline)
(display "==================== Tests for <Quoted> expressions: ====================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"'\")")
(newline)
(run-test (test-string <sexpr> "'") '(failed with report:))
(newline)
(newline)

(display "> (test-string <sexpr> \"'3\")")
(newline)
(run-test (test-string <sexpr> "'3") '((match '3) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"'(1 2 3)\")")
(newline)
(run-test (test-string <sexpr> "'(1 2 3)") '((match '(1 2 3)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"'(a b c)t\")")
(newline)
(run-test (test-string <sexpr> "'(a b c)t") '((match '(a b c)) (remaining "t")))
(newline)
(newline)

(display "> (test-string <sexpr> \"'#f\")")
(newline)
(run-test (test-string <sexpr> "'#f") '((match '#f) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"'(4 #f (+ 1 3))\")")
(newline)
(run-test (test-string <sexpr> "'(4 #f (+ 1 3))") '((match '(4 #f (+ 1 3))) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"'(4 #f ,(+ 1 3))\")")
(newline)
(run-test (test-string <sexpr> "'(4 #f ,(+ 1 3))") '((match '(4 #f ,(+ 1 3))) (remaining "")))
(newline)
(newline)
|#

#|
;; <QuasiQuoted> tests
(newline)
(display "=========================================================================")
(newline)
(display "================== Tests for <QuasiQuoted> expressions: =================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"`a\")")
(newline)
(run-test (test-string <sexpr> "`a") '((match `a) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"`(a b c)\")")
(newline)
(run-test (test-string <sexpr> "`(a b c)") '((match `(a b c)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"`(a b c)t\")")
(newline)
(run-test (test-string <sexpr> "`(a b c)t") '((match `(a b c)) (remaining "t")))
(newline)
(newline)

(display "> (test-string <sexpr> \"`#f\")")
(newline)
(run-test (test-string <sexpr> "`#f") '((match `#f) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"`(4 #f (+ 1 3))\")")
(newline)
(run-test (test-string <sexpr> "`(4 #f (+ 1 3))") '((match `(4 #f (+ 1 3))) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"`(4 #f ,(+ 1 3))\")") ;; to get back to this onse unquote works
(newline)
(run-test (test-string <sexpr> "`(4 #f ,(+ 1 3))") '((match `(4 #f ,(+ 1 3))) (remaining "")))
(newline)
(newline)
|#

#|
;; <Unquoted> tests
(newline)
(display "=========================================================================")
(newline)
(display "=================== Tests for <Unquoted> expressions: ===================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \",\")")
(newline)
(run-test (test-string <sexpr> ",") '(failed with report:))
(newline)
(newline)

(display "> (test-string <sexpr> \",3\")")
(newline)
(run-test (test-string <sexpr> ",3") '((match ,3) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \",(a b c)\")")
(newline)
(run-test (test-string <sexpr> ",(a b c)") '((match ,(a b c)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \",(a b c)t\")")
(newline)
(run-test (test-string <sexpr> ",(a b c)t") '((match ,(a b c)) (remaining "t")))
(newline)
(newline)

(display "> (test-string <sexpr> \",#f\")")
(newline)
(run-test (test-string <sexpr> ",#f") '((match ,#f) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \",(4 #f (cons 1 3))\")")
(newline)
(run-test (test-string <sexpr> ",(4 #f (cons 1 3))") '((match ,(4 #f (cons 1 3))) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \",(4 #f ,(cons 1 3))\")") ;; to get back to this onse unquote works
(newline)
(run-test (test-string <sexpr> ",(4 #f ,(cons 1 3))") '((match ,(4 #f ,(cons 1 3))) (remaining "")))
(newline)
(newline)
|#

#|
;; <CBName> tests
(newline)
(display "=========================================================================")
(newline)
(display "=================== Tests for <CBName> expressions: ===================")
(newline)
(display "=========================================================================")
(newline)

(display "> (test-string <sexpr> \"@a\")")
(newline)
(run-test (test-string <sexpr> "@a") '((match (cbname a)) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"@(* a b)\")")
(newline)
(run-test (test-string <sexpr> "@(* a b)") '((match (cbname (* a b))) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"{(* a b)}\")")
(newline)
(run-test (test-string <sexpr> "{(* a b)}") '((match (cbname (* a b))) (remaining "")))
(newline)
(newline)

(display "> (test-string <sexpr> \"{  (* a b)  }\")")
(newline)
(run-test (test-string <sexpr> "{  (* a b)  }") '((match (cbname (* a b))) (remaining "")))
(newline)
(newline)
|#
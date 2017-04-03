;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;;;::;:::
;;;     Searching the Web  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;;;:::;;;
;;;  	  	     	 
;;; cuyuk@ku.edu.tr    Mon Nov 17 09:38:53 2014
;;;  	  	     	  	  	     	 
;;; Code for this project:  	  	     	 
;     * code files to read: search.scm
;     * code file simply to load: generate.scm, include.scm
(load "include.scm")  	  	     	 
(load "search.scm")  	  	     	 
(load "generate.scm")  	  	     	 
  	  	     	 
;;; The following is defined so that the file loads without errors:
(define your-answer-here -1)
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;;;:::;;:
;;; Warmup Exercise: Index Implementation
;;;  	  	     	 
;;; Draw a box and pointer diagram (you do not need to turn this in), and
;;; show the corresponding printed representation, to illustrate the
;;; implementation of an Index as defined in Section 3.  Think about how
;;; you want the following expressions to create and then mutate your data
;;; structure:  	  	     	 
;;;  	  	     	 
;;; (define test-index (make-index))
;;; (add-to-index! test-index 'key1 'value1)
;;; (add-to-index! test-index 'key2 'value2)
;;; (add-to-index! test-index 'key1 'another-value1)
  	  	     	 
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;::;;;;
;;; Exercise 1: The Web as a General Graph
;;; 3 pts  	  	     	 
;;;  	  	     	 
;;; Explain why our depth first strategy (using DFS-simple) will
;;; fail on the-web-initial graph.

;ANSWER:
;The reason that the-web-initial graph won't work is that, the-web-initial graph has "http://ais.ku.edu.tr/course/7963/Default.html" as the top node. Than its child's child     	 
;"http://ais.ku.edu.tr/course/7963/syl.html" has an outgoing edge to the top node. In other words, there is a cycle inside this tree and DFS-simple cannot work for trees that	     	 
;include cycles. Similarly, "http://ais.ku.edu.tr/course/7963/des.html" and "http://ais.ku.edu.tr/course/7963/CWMenu.html" are siblings but they have an edge between each other.     	 
;If we try to execute DFS-simple for the-web-initial then it would go into an infinite loop. Because at each iteration of inner-search, it will call the top node recursively.

;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;::;;;:
;;;;;;  	  	     	 
;;; Exercise 2:  Breadth-first search
;;; 4 pts  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.
  	  	     	 
(define (BFS-simple start goal? graph)  
  (search start  	  	     	 
	  goal?  	  	     	 
	  find-node-children 	  	     	 
	  (lambda (new old) (append old new))
	  graph))
;EXPLANATION:The reason why it works: It works because the cooperation of append and find-node-children. find-node-children basically gives append elements from left to right.
;In other words, in the beginning we have "a" as a list, then find-node-children gives the lower nodes from left to right and "append" adds them to the end of the list. Then we
;we have a,b,i,m and it keeps adding and checking in the meantime like this.
  	  	     	 
;;;;;;;;;;;;;;
(write-line (list 'BFS-SIMPLE 'TEST 'CASES:))
;;; Test Cases  	  	     	 
(BFS-simple 'a (lambda (node) (eq? node 'g)) test-graph)
;;;;;;  	  	     	 
  	  	     	 
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;::;;:;
;;;;;;  	  	     	 
;;; Exercise 3:  search-with-cycles
;;;  	  	     	 
;;; Part (a): you will need to write a similar search procedure that
;;; handles cycles  	  	     	 
;;; 8 pts  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project3 handout, do not forget
;;; the documentation and test cases.

  	  	     	 
(define (search-with-cycles initial-state goal? successors merge graph) ;EXPLANATION: search-with-cycles basically does the same thing with search. Only difference is that
  (define (search-inner still-to-do graph)                              ;search-with-cycles erases the node that it has already visited. When we give a graph to search-with-cycles
    (if (null? still-to-do)  	  	     	                        ;it starts from initial-state and checks if it's the one we are looking for, if not it erases that node.
	#f  	  	     	                                        ;Hence, even though there is a cycle it will be broken.
	(let ((current (car still-to-do)))
          (if *search-debug*  	  	     	 
	      (write-line (list 'now-at current)))
	  (if (goal? current)  	  	     	 
	      #t
              (search-inner (merge (successors graph current) (cdr still-to-do)) (remove (find-graph-element graph current) graph)))
              )))
  (search-inner (list initial-state) graph))
  	  	     	 

  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;::;;::
;;; Part (b): Use search-with-cycles to define a new procedure called DFS that
;;;           implements full depth first search.
;;; 2 pts  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project3 handout, do not forget
;;; the documentation and test cases.
  	  	     	 
(define (DFS start goal? graph)  ;EXPLANATION: DFS takes search-with-cycles unlike DFS-simple. So, by this way it can work for circles.
  (search-with-cycles start  	  	     	 
	  goal?  	  	     	 
	  find-node-children  	  	     	 
	  (lambda (new old) (append new old))
	  graph))  	     	 
  	  	     	 
;;;;;;;;;;;;;; Test Cases  
(write-line (list 'DFS 'TEST 'CASES:))
;;;	  	     	 
(DFS 'x (lambda (node) (eq? node 'z)) test-cycle2)
(DFS 'x (lambda (node) (eq? node 'v)) test-cycle2)
;;;;;;  	  	     	 
  	  	     	  	  	     	 
  	  	     	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;::;:;;
;;; Part (c): Use search-with-cycles to define a new procedure called BFS that
;;;           implements full breadth first search.
;;; 2 pts  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.
  	  	     	 
(define (BFS start goal? graph) ;EXPLANATION: BFS takes search-with-cycles unlike BFS-simple. So, by this way it can work for circles.
  (search-with-cycles start  	  	     	 
	  goal?  	  	     	 
	  find-node-children  	  	     	 
	  (lambda (new old) (append old new))
	  graph))  	  	     	 
  	  	     	 
;;;;;;;;;;;;;; Test Cases  
(write-line (list 'BFS 'TEST 'CASES:))
;;;	  	     	 
(BFS 'x (lambda (node) (eq? node 'v)) test-cycle2)
(BFS 'x (lambda (node) (eq? node 'u)) test-cycle2)
;;;;;;  	  	     	 
  	  	     	 
  	  	     	 
  	  	     	 
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;::;:;:
;;; Part (d): Give the order in which the nodes are visited for DFS and BFS
;;;           search of the-web.
;;; 4 pts 

;;;Test Cases
(write-line (list 'BFS 'AND 'DFS 'TEST 'CASES 'WITH 'the-web))
;;;
(BFS 'http://ais.ku.edu.tr/course/7963/default.html (lambda (node) (eq? node 'a)) the-web)
(DFS 'http://ais.ku.edu.tr/course/7963/default.html (lambda (node) (eq? node 'a)) the-web)

;the order is linear for both of them. The nodes are visited the same number of times with the number of elements in the-web. So, O(n). This is what we
;are looking for anyways. To visit every element 1 time.
  	  	     	 
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;::;::;
;;;;;;;;;;;;;;;  	  	     	 
;;; Exercise 4: The Index Abstraction
;;; 9 pts  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.

(define (exists? list value)                ;EXPLANATION: I wrote this in order to check (when we use add-to-index!) if the key we are trying to enter already exists in the index.
  (cond ((null? list) #f)
        ((eq? (car list) value) #t)
        (else (exists? (cdr list) value))))
  	  	     	 
 	  	     	 
; Adds the value under the given key in the index
(define (add-to-index! index key value) ; Index,Key,Val -> Index  	      ;EXPLANATION: First, we check if the index is null. If it is, then we directly add the key and the
  (let ((index-entry (find-entry-in-index index key)))                        ;value. If it it is not null then we first check (with the help of exists?) if the key we want to 
    (if (null? index-entry)  	  	     	                              ;enter exists. If it doesnt, we create a new pair. If it exists then we directly add the value
	;; no entry -- create and insert a new one...                         ;to the existing key.
        (set-cdr! index (append (cdr index) (list (list key (list value)))))
  	  	     	 
	;; entry exists -- insert value if not already there...
        (if (exists? (car (cdr index-entry)) value)
            index
            (set-car! (cdr index-entry) (cons value (car (cdr index-entry))))))
    index))

  	  	     	 
;;;;;;;;;;;;;; Test Cases 
(write-line (list 'ADD-TO-INDEX 'TEST 'CASES:))
;;; 	  	     	 
 (define test-index (make-index))
 (add-to-index! test-index 'key1 'value1)
 (add-to-index! test-index 'key1 'another-value1)
 (add-to-index! test-index 'key1 'another-value1)
 (add-to-index! test-index 'key2 'value2)
 (add-to-index! test-index 'key2 'another-value2)
 (add-to-index! test-index 'key1 'value1)
 (add-to-index! test-index 'k2 null)

;;;;;;  	  	     	 
  	  	     	 
;
(write-line (list 'FIND-IN-INDEX 'TEST 'CASES:))
(find-in-index test-index 'key1)
(find-in-index test-index 'key2)
  	  	     	 
; Remove a key from a given index
(define (remove-key-from-index index key)                        ;EXPLANATION: With this procedure, we first check if the index-entry the we want to erase exists in the index
  (let ((index-entry (find-entry-in-index index key)))           ;If it does not exist then we are not to do anything. If it does, we basically remove that index-entry form the
    (if (null? index-entry)                                      ;index.
        "The key you have entered does not exist in this index."
        (remove index-entry index))))  	  	     	 
  	  	     	 
;;;;;;;;;;;;;;Test Cases
(write-line (list 'remove-key-from-index 'TEST 'CASES:))
;;;  	  	     	 
(remove-key-from-index test-index 'key2)
(remove-key-from-index test-index 'key3)
;;;;;;  	  	     	 
  	  	     	 
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;::;:::
;;; Exercise 5: Indexing the Web
;;; 6 pts  	  	     	 
;;;  	  	     	 
;;; Our purpose in creating an index of a web is to
;;; later support the ability to find any pages that contain
;;; a given word.  Thus, a Key in our index will be a Word,
;;; and the values in the index will be the URLs of pages
;;; that contain that word.  	  	     	 
;;;  	  	     	 
;;; A procedure to help  with indexing web pages
;;; using the Index abstraction.  The idea is to
;;; get the text associated with the URL from the
;;; web, and then key each of the words in the
;;; text into the index.  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.
  	  	     	 
;;;add-document-to-index!: Index, Web, URL
;(define (add-document-to-index! index web url)
;  )
(define (add-document-to-index! index web url)              ;EXPLANATION:In this proc, I defined another procedure inside it. It only takes content of the url in the web
  ;;Finds the contents of the url                           ;that is given by the user. First, we check if the web is null, if not we find the content of the corresponding url
  (define (work-with-content! content)                      ;then we check if the content is null, if not, we add the content recursively(second expression of begin).
    (if (null? web) 
        "The web you have entered is empty!!"
        (if (null? content)
            index
            (begin (add-to-index! index (car content) url)
                   (work-with-content! (cdr content))))))
  (work-with-content! (find-node-contents web url)))	  	     	 
  	  	     	 
;;;;;;;;;;;;;;Test Cases
(write-line (list 'add-document-to-index! 'TEST 'CASES:))
;;; Test Cases and Example use  	  	     	 
;;;;;;  	  	     	 
;  	  	     	 
(define the-web-index (make-index))
;  	  	     	 
(add-document-to-index! the-web-index
			the-web
			'http://ais.ku.edu.tr/course/7963/des.html)
;;;;;;Test Cases
(write-line (list 'find-in-index 'TEST 'CASES:))
;;;;;;

(find-node-contents the-web 'http://ais.ku.edu.tr/course/7963/des.html)
;  	  	     	 
(find-in-index the-web-index 'DESCRIPTION)
; ;Value: (http://ais.ku.edu.tr/course/7963/des.html)
;  	  	     	 
(find-in-index the-web-index '*magic*)
; ;Value: #f  	  	     	 
  	  	     	 
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;:::;;;
;; Exercise 6: Crawling the web to build an index
;;;;  	  	     	 
;;; (a) Final search with procedure application at each node we visit
;;; 10 pts  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.

(define (search-with-cycles-with-procedure initial-state goal? proc successors merge graph) ;EXPLANATION: This procedure is exactly the same with search-with-cycles. Only
  (define (search-inner still-to-do graph index)                                    ;difference is that when this one is checking a node if it is the one we are looking for
    (if (null? still-to-do)  	  	     	                                    ;it also applies a procedure to the node we check. This helps us the create an index.
	index  	  	     	 
	(let ((current (car still-to-do)))
          ;(if *search-debug*  	  	     	 
	  ;    (write-line (list 'now-at current)))
	  (if (goal? current)  	  	     	 
	      #t
              (search-inner (merge (successors graph current) (cdr still-to-do)) (remove (find-graph-element graph current) graph) (proc index graph current)))
              )))
  (search-inner (list initial-state) graph (make-index)))
  	  	     	 	  	     	 
  	  	     	 
;;; BFS with procedure  	  	     	 
(define (BFS-with-procedure start goal? proc graph)
  (search-with-cycles-with-procedure start
                                     goal?
                                     proc
                                     find-node-children
                                     (lambda (new old) (append old new))
                                     graph)) 
  
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;:;:;:::;;:
;;; (b) Write a procedure, make-web-index, that creates a new index,
;;; find all the URLs that can be reached from a given web and and
;;; initial URL, indexes them, and returns a procedure that can be
;;; used to look up all the URLs of documents containing a given word
;;; (key). Use BFS to traverse the-web. You will also want to make use
;;; of the index manipulating procedures from above.
;;; 6 pts  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.

(define (make-web-index web url)          ;EXPLANATION: With BFS-with-procedure we give an index to make-web-index and then we check the word "x" in that index by find-in-index.
  (lambda (x)                             ;Every URL that contaions the word "x" will be returned by find-in-index.
    (if (pair? (find-in-index (BFS-with-procedure url (lambda (node) (eq? node 'AADDSSBBμφλSBBμφλXXAADDSSBBμφλSBBμφλXX)) add-document-to-index! web) x))
  (append (make-index) (list (list x (find-in-index (BFS-with-procedure url (lambda (node) (eq? node 'AADDSSBBμφλSBBμφλXXAADDSSBBμφλSBBμφλXX)) add-document-to-index! web) x))))
  (append (make-index) (list (list x (list(find-in-index (BFS-with-procedure url (lambda (node) (eq? node 'AADDSSBBμφλSBBμφλXXAADDSSBBμφλSBBμφλXX)) add-document-to-index! web) x))))))))  	  	     	 
  	  	     	 
;;;;;;;;;;;;;;Test Case
(write-line (list 'make-web-index 'TEST 'CASES:))
;;;	  	     	 
;;;;;;  	  	     	 
;  	  	     	 
(define find-documents (make-web-index the-web 'http://ais.ku.edu.tr/course/7963/default.html))
;;  	  	     	 
(find-documents 'DESCRIPTION)
;  	  	     	 
(find-documents 'comp101)  	  	     	 
;  	  	     	 
(find-documents '|1|)  	  	     	 
  	  	     	 
  	      	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;::;;::;;;;
;;; Exercise 7: A dynamic web search
;;;  	  	     	 
;;; 8 pts  	  	     	 
;;;  	  	     	 
;;;  	  	     	 
;;; To investigate crawling, write two procedures: 1. (search-any web
;;; start-node word): searches the indicated web using a BFS strategy
;;; and returns the FIRST document that it finds that contains the
;;; given word. It should stop searching as soon as it finds such a
;;; document.  2. (search-all web start-node word): searches the
;;; entire web (using a BFS strategy) and returns all documents that
;;; contain a given word.  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.
  	  	     	 
(car (cadr ((make-web-index the-web 'http://ais.ku.edu.tr/course/7963/default.html) 'DESCRIPTION)))

(define (search-any web start-node word)
  (let ((index ((make-web-index web start-node) word)))
    (if (not (pair? index)) #f
        (list (car index) (list (car (cadr index)) (list (caar (cdr (cadr index))))))))) 	  	     	 
  	  	     	 
(define (search-all web start-node word)
  (let ((index ((make-web-index web start-node) word)))
    (if (pair? index) index
         #f)))	  	     	 
  	  	     	 	  	     	 	 
;;;;;;;;;;;;;;
(write-line (list 'search-any 'AND 'search-all 'TEST 'CASES:))
;;; Test Cases  	  	     	 
;;;;;;  	  	     	 
(search-any the-web 'http://ais.ku.edu.tr/course/7963/default.html 'DESCRIPTION)
  	  	     	 
(search-all the-web 'http://ais.ku.edu.tr/course/7963/default.html 'DESCRIPTION)

(search-any the-web 'http://ais.ku.edu.tr/course/7963/default.html 'comp101)

(search-all the-web 'http://ais.ku.edu.tr/course/7963/default.html 'comp101)

(search-any the-web 'http://ais.ku.edu.tr/course/7963/default.html '|1|)

(search-all the-web 'http://ais.ku.edu.tr/course/7963/default.html '|1|)
  	  	     	 
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;::;;::;;;:
;;; Exercise 8: Comparison - Web Index vs. Dynamic Search
;;;;  	  	     	 
;;; 6 pts  	  	     	 
;;;------------------------------------------------------------
;;; utility for timing procedure calls.
;;; returns the time in milliseconds
  	  	     	 
(define (timed f . args)  	  	     	 
  (let ((start (current-milliseconds)))
    (let ((val (apply f args)))
      (newline)  	  	     	 
      (display "time expended: ")
      (display (- (current-milliseconds) start))
      val)))  	  	     	 
  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.
;;;  

;;; LIST THE TIMED TEST CASES HERE:
;  	  	     	 
;(timed factorial 25) 

;;;;;;;;;;;!!!!Following test cases are the cases that we are asked to do for each random-web, AFTER THE TEST CASES I WROTE several lines of ANSWER regarding the question:

;;;Test Case
"RANDOM WEBS:"
;;;
(define random-web1 (generate-random-web 5))
(define random-web2 (generate-random-web 20))
(define random-web3 (generate-random-web 40))

;;;Test Case
"timed search-any TEST CASES FOR ALL RANDOM WEBS:"
;;;
(timed search-any random-web1 '*start* 'description)
(timed search-any random-web1 '*start* 'susanhockfield)

(timed search-any random-web2 '*start* 'description)
(timed search-any random-web2 '*start* 'susanhockfield)

(timed search-any random-web3 '*start* 'description)
(timed search-any random-web3 '*start* 'susanhockfield)

;;;Test case
"timed search-all TEST CASES FOR ALL RANDOM WEBS:"
;;;
(timed search-all random-web1 '*start* 'description)

(timed search-all random-web2 '*start* 'description)

(timed search-all random-web3 '*start* 'description)

;;;Test Case
"timed make-web-index TEST CASES FOR ALL RANDOM WEBS:"
;;;
(timed make-web-index random-web1 '*start*)
(timed make-web-index random-web2 '*start*)
(timed make-web-index random-web3 '*start*)

;;;Test Case
"timed find-documents TEST CASES:"
;;;
(define find-documents1 (make-web-index random-web1 ''*start*))
(define find-documents2 (make-web-index random-web2 ''*start*))
(define find-documents3 (make-web-index random-web3 ''*start*))

(timed find-documents1 'description)
(timed find-documents2 'description)
(timed find-documents3 'description)

(timed find-documents1 'susanhockfield)
(timed find-documents2 'susanhockfield)
(timed find-documents3 'susanhockfield)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ANSWER TO THE QUESTION:

;The conclusion I draw: In the beginning, when the program does the search-any, the time taken is small because it checkes the first content only. So, it's not valid to
;make any conclusion from that. 

;After search-any, I applied search-all to 3 random webs I created. The time it takes to search all the web index is accordingly to the size of the web. If web is larger
;then the time it takes to search all documents is higher too.

;After these, the biggest conclusion I can make is between search-all and find-documents. Both procedures do the same job when they are applied
;to same web-index and start node. When I applied timed to both search-all and find-documents it was always find-documents which is a lot faster than search-all.
;As I explained above, search-all's time consumption increases with respect to web size, whereas find-documents procedure almost spends no time to do that.

;I think the reason for that, the search-all procedure does both indexing and looking for content in the same time, whereas the procedure find-documents creates index first,
;then looks for documents. So, if I was building a web for users I would try to take care of this issue.
 	  	     	 
  	  	     	 
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;::;;::;;:;
;;;;;;;;;;;;;;;;;;;;;;;;  	  	     	 
;;; Exercise 9: Using a better indexing scheme
;;;;  	  	     	 
;;; 10 pts  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;!!!!In this part, I created a new proc called "create-key-sorted-index". This proc takes an index, and sorts it with respect to its keys.

(define (find-entry-in-list lst key)
  (let ((entry (assv key (cdr lst))))
        (if entry entry '())))

(define (find-in-list lst key)
  (let ((index-entry (find-entry-in-list lst key)))
    (if (not (null? index-entry))
        (cadr index-entry)  	  	     	 
        #f)))


(define (create-list-of-keys index)                                ;EXPLANATION: This takes a list index and creates a list of its keys.
  (if (null? index) null
      (cons (car (car index)) (create-list-of-keys (cdr index)))))


(define (sort-key-list lst)                   ;EXPLANATION: This takes a list of keys and sorts them.
  (sort lst symbol<?))


(define (create-key-sorted index sorted-key-list)  ;EXPLANATION:this takes a sorted-key-list then appends them with its values and creates a list of keys and values.
      (if (null? sorted-key-list) 
          null
          (cons (list (car sorted-key-list) (find-in-list index (car sorted-key-list))) (create-key-sorted index (cdr sorted-key-list)))))



(define (create-key-sorted-index index) ;EXPLANATION: After we create a list that contains sorted keys with their values. This procedure turns that list into an index.
  (if (not (index? index)) #f
  (append (make-index) (create-key-sorted index (sort-key-list (create-list-of-keys (cdr index)))))))


(write-line (list 'TEST 'CASES 'FOR 'MY 'OWN 'KEY 'SORTED 'INDEX 'PROCEDURE:))
(add-to-index! test-index 'isildur 'ignorant)
(add-to-index! test-index 'isildur 'weak)            ;EXPLANATION: I created an index with many arguments to check my procedures, if they sort the values and keys.
(add-to-index! test-index 'isildur 'mourning)
(add-to-index! test-index 'elendil 'powerful)
(add-to-index! test-index 'elendil 'wise)
(add-to-index! test-index 'aragorn 'heir)
(add-to-index! test-index 'aragorn 'merciless)
(add-to-index! test-index 'aragorn 'killer)
(add-to-index! test-index 'legolas 'elf)
(add-to-index! test-index 'legolas 'arrogant)
(add-to-index! test-index 'isildur 'zillion)
(add-to-index! test-index 'elendil 'strong)
(add-to-index! test-index 'aragorn 'brave)
(add-to-index! test-index 'legolas 'fighter)
(add-to-index! test-index 'key1 'no)

;;;
(write-line (list 'THE 'LAST 'VERSION 'OF 'UNSORTED 'TEST-INDEX 'THAT 'I 'WILL 'USE 'FOR 'OPTIMIZE-INDEX:))
(add-to-index! test-index 'key2 'no)

;;;;;;;;;;;;!!!!TEST CASE THAT SHOWS create-key-sorted-index-works:
(create-key-sorted-index test-index)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF MY OWN SORT PROC!


(define (set-cth-element index-vector c)
  (if (pair? (car (cdr (vector-ref index-vector c))))
  (vector-set! index-vector c (list (car (vector-ref index-vector c)) (sort (car (cdr (vector-ref index-vector c))) symbol<?))) ;EXPLANATION: This proc is a helper proc to sort
  (vector-set! index-vector c (list (car (vector-ref index-vector c)) (car (cdr (vector-ref index-vector c))))))                ;the values of the keys in a vector.
  index-vector)  


(define (sort-values index-vector c)      
  (let ((ct (vector-length index-vector)))          ;EXPLANATION: This proc finally sorts the values of the keys in a vector.
    (let ((vctr (set-cth-element index-vector c)))   
    (if (= c (- ct 1)) index-vector
        (if (pair? (vector-ref index-vector c))
          (sort-values vctr (+ c 1))
          (sort-values index-vector (+ c 1)))))))

        
	  	     	 
(define (optimize-index index)
  (if (not(index? index)) #f
  (sort-values (list->vector (create-key-sorted-index index)) 1)))

(write-line (list 'TEST ' CASE 'FOR 'optimize-index 'THAT 'TOOK 'UNSORTED '(this is my test index)'INDEX '(LIST) 'AND 'TURNED 'INTO 'A 'KEY 'AND 'VALUE 'SORTED 'VECTOR:))

(define test-vector (list->vector (create-key-sorted-index test-index)))

(optimize-index test-index) ;EXPLANATION: Before, test-index is unsorted. After this it gets vector and its keys and values sorted.
  	  	     	 
  	  	     	 
;;;;;;;::;;;:::::;:;:::::;;::::;:;:::;:;::;::;::;;::;::;;::;;::
;;;;;;;;;;;;;;;;;;;;;;;;  	  	     	 
;;; Exercise 10: Binary search of sorted elements
;;;;  	  	     	 
;;; 20 pts  	  	     	 
;;;  	  	     	 
;;; Follow the instructions in the Project 3 handout, do not forget
;;; the documentation and test cases.

 ;; type: Optimized-Index, Key -> List<Val>
  ;; key is a symbol representing the key we are looking for
  ;; this procedure does a binary search, so it takes O(log n)
  ;; time where n is the number of entries in optind
  	  	     	  	  	     	 	  	     	 
(define (find-entry-in-optimized-index optind key)
  (if (vector? optind)
  (let ((c (/ (vector-length optind) 2)))
    (define (vector-search vector ctr key)
      (if (> (/ (* 3 ctr) 2) (vector-length vector)) "The key you are looking for is not here!!";EXPLANATION: This is a basic binary search. First I take the length of the vector.
      (if (< ctr 1) "I could not find what you are looking for!!!"                          ;Then applies the vector-search proc that I defined inside find-entry-in-optimized.
          (if (eq? key (car (vector-ref vector (round ctr))))                               ;After finding the length, it checkes the first value if it is greater or less than                                                                                  
              (car (cdr (vector-ref vector (round ctr))))                                   ;the one we are looking for. If it is less than moves to left, if it is greater then 
              (if (symbol<? key (car (vector-ref vector (round ctr))))                      ;it moves to the right. While doing that I have added several pre-cautions that if the 
                  (vector-search vector (/ (* 3 ctr) 2) key))))))                           ;counter is higher than the length it returns false, also if it is less than 1 it 
    (vector-search optind c key)) "Please enter an unempty index!!!"))                      ;returns false too. Also, if the index is empty.
  	  	     	 
  	  	     	 
;;;;;;;;;;;;;;  	  	     	 
;;; Test Cases  	  	     	 
;;;;;;

(write-line (list 'my 'index 'test 'for 'find-entry-in-optimized-index:))
(define test-optind (optimize-index test-index))
(timed find-entry-in-optimized-index test-optind 'aragorn)

(define random-web4 (generate-random-web 100))

(define test-optimized-index1 (optimize-index ((make-web-index random-web4 '*start*) 'DESCRIPTION)))

(define test-optimized-index2 (optimize-index ((make-web-index random-web4 'http://ais.ku.edu.tr/course/7963/default.html) 'DESCRIPTION)))

(define test-optimized-index3 (optimize-index ((make-web-index random-web4 '*start*) '|1|)))

(define test-optimized-index4 (optimize-index ((make-web-index random-web4 'http://ais.ku.edu.tr/course/7963/default.html) '|1|)))

(define test-optimized-index5 (optimize-index ((make-web-index the-web '*start*) '|1|)))

(define test-optimized-index6 (optimize-index ((make-web-index the-web 'http://ais.ku.edu.tr/course/7963/default.html) '|1|)))

(timed find-entry-in-optimized-index test-optimized-index1 'DESCRIPTION)

(timed find-entry-in-optimized-index test-optimized-index2 'DESCRIPTION)

(timed find-entry-in-optimized-index test-optimized-index3 '|1|)

(timed find-entry-in-optimized-index test-optimized-index4 '|1|)

(timed find-entry-in-optimized-index test-optimized-index5 '|1|)

(timed find-entry-in-optimized-index test-optimized-index6 '|1|)

;EXPLANATION: I did 4 different timing tests for find-entry-in-optimized-index. In the exercise 8, we did the same thing for random webs. This time I created a web that is
;way larger than the ones I created in the Exercise 8. In Exercise 8, we did the same thing with find-entry-in-optimized-index. But here we do the same thing for vector indexes.
;There we did it for list indexes. After calculating the time for finding the document in a list index was always greater even though the web we make index is smaller.
;For lists the time results were distinct, such as 49, 68, 88, 106... Whereas for find-entry-in-optimized-index the time results are almos always 0, 1, 4, 6. It never exceeded 10.
;So the conclusion I found is that searching in vector is way faster than lists. Designing web with vectors instead of lists is more useful.



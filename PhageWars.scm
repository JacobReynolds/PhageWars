;;;; CSci 1901 Project - Fall 2013
;;;; PhageWars++ Player AI

;;;======================;;;
;;;  SUBMISSION DETAILS  ;;;
;;;======================;;;
(define (reload) (load "reyno511.scm"))
;; List both partners' information below.
;; Leave the second list blank if you worked alone.
(define authors 
  '((
     "Jacob Reynolds"  ;; Author 1 Name
     "reyno511"        ;; Author 1 X500
     "4728076"         ;; Author 1 ID
     "002"             ;; Author 1 Section
     )
    (
     ""   ;; Author 2 Name
     ""   ;; Author 2 X500
     ""   ;; Author 2 ID
     ""   ;; Author 2 Section
     )))

;; CSELabs Machine Tested On: 
;;


;;;====================;;;
;;;  Player-Procedure  ;;;
;;;====================;;;



(define player-procedure
  (let () 
    
    ;;===================;;
    ;; Helper Procedures ;;
    ;;===============================================================;;
    ;; Include procedures used by get-move that are not available in ;;
    ;;  the util.scm or help.scm file.                               ;;
    ;; Note: PhageWars++.scm loads util.scm and help.scm , so you do ;;
    ;;  not need to load it from this file.                          ;;
    ;; You also have access to the constants defined inside of       ;;
    ;;  PhageWars++.scm - Constants are wrapped in asterisks.        ;;
    ;;===============================================================;;
    
    ;; Returns a random-element from a list.
    (define (random-element lst)
      (list-ref lst (random (length lst))))
    

    ;; Makes a random move
    (define (make-random-move player board)
      (let ((my-cells (get-cell-list player board)))
	(list (make-move (car (random-element my-cells)) 
			 (car (random-element board))))))

    
    ;; Makes a random move to closest cell
    (define (close-random player board)
      (let ((my-cells (get-cell-list player board)))
	(let ((cell (random-element my-cells)))
	  (list (make-move (car cell)
			   (closest-cell (car cell) player board))))))
    
    
    ;;All cells will defend themselves
    (define (defend player board)
      '())

    
    ;;Returns the cell value with the largest virus amount
    (define (get-largest-cell player board)
      (define (helper player board cell-list largest)
	(if (= (length cell-list) 1);tests the final value against the original value
	    (get-cell-value player board (max (car (get-virus-value player board largest)) (car (cdr (cddddr (car cell-list))))))
	    (if (> (car (cdr (cddddr (car cell-list)))) (car (cdr (cddddr (cadr cell-list)))));tests which value is larger
		(helper player board (cdr cell-list) (caar cell-list))
		(helper player board (cdr cell-list) largest))))
      (helper player board (get-cell-list player board) (caar (get-cell-list player board))))

    
    ;;Returns the virus count of a given cell
    (define (get-virus-value player board cell)
      (define (helper player board cell cell-list);modifies the cell-list to get the virus count dedicated to the cell
	(if (= (caar cell-list) cell)
	    (cdr (cddddr (car cell-list)))
	    (helper player board cell (cdr cell-list))))
      (helper player board cell board))

    
    ;;Returns the cell number when given a virus amount, possibility for error when two or more cells have the same amount.  Did not use in code.
    (define (get-cell-value player board value)
      (define (helper player board value cell-list)
	(if (= (car (cdr (cddddr (car cell-list)))) value)
	    (caar cell-list)
	    (helper player board value (cdr cell-list))))
      (helper player board value (get-cell-list player board)))

    
    ;;If you are losing (have less cells than other player) returns #t
    (define (losing? player board)
      (if (> (length (get-cell-number-list (other-player player) board)) (length (get-cell-number-list player board)))
	  #t
	  #f))

    
    ;;Returns a cell that is not the largest cell.  Possibility for error when only one cell, did not use in code.
    (define (new-random-move player board)
      (define (helper player board cell)
	(if (= cell (get-largest-cell player board));if that cell is the largest cell, cdr the list.
	    (helper player board (new-random (get-cell-number-list player board)))
	    cell))
      (helper player board (new-random (get-cell-number-list player board))))

    
    ;;Picks a random number out of an inputted list of numbers
    (define (new-random list)
      (define (helper list count)
	(if (= count 0)
	    (car list)
	    (helper (cdr list) (- count 1))))
      (helper list (random (length list))))
    
    
    ;;Gets the virus count of the first cell in a list.  Should be called with (car list) instead of list.
    (define (get-virus-count list)
      (caddr (cdddar list)))
        
    
    ;; Counts instances of x in lst with eq?
    (define (count x lst)
      (length (filter (lambda (el) (eq? el x)) lst)))
    
    
    ;;The next 5 procedures are copied from a previous homework, and are procedures used in merge-sort, to sort a random list of integers into lowest-to-highest order.
    
    ;;Combines a list from A integer to B integer.  Starts count at 0.
    (define (sub-list lst a b)
      (if (null? lst)
	  lst
	  (if (> b (length lst))
	      ()
	      (if (> a (length lst))
		  ()
		  (if (and (<= a 0) (>= b 0))
		      (cons (car lst) (sub-list (cdr lst) (- a 1) (- b 1))) ;if at or beyond integer A, cons (car lst) to the call of (sub-list (cdr lst)), subtract one from a and b
		      (sub-list (cdr lst) (- a 1) (- b 1))))))) ;if the count is not at A yet, call the cdr of the list, and subtract 1 from a and b.
    
    
    ;;Reverses a list by adding first point onto end of new list.
    (define (reverse lst)
      (define (helper lst temp-list)
	(if (null? lst)
	    temp-list
	    (helper (cdr lst) (cons (car lst) temp-list))))
      (helper lst ()))
    

    ;;Merges two lists that are numerically sorted
    (define (merge-lst lst1 lst2) 
      (define (helper lst1 lst2 temp-list)
	(if (null? lst1)
	    (append (reverse temp-list) lst2)
	    (if (null? lst2)
		(append (reverse temp-list) lst1)
		(if (and (list? lst1) (list? lst2))
		    (if (= (min (car lst1) (car lst2)) (car lst1)) ;if the minimum point is in the first list
			(helper (cdr lst1) lst2 (cons (car lst1) temp-list)) ;cons that point to temp-list
			(helper lst1 (cdr lst2) (cons (car lst2) temp-list)));else cons the car of lst2 to templist
		    ()))))
      (helper lst1 lst2 ()))

    
    ;;Breaks a list into two closely even lists
    (define (sub-list-split lst) 
      (if (null? lst)
	  ()
	  (if (list? lst)
	      (if (odd? (length lst))
		  (list (sub-list lst 0 (/ (- (length lst) 2) 2)) (sub-list lst (/ (- (length lst) 2) 2) (- (length lst) 1))) ;makes a list of the left half and right half, using sub-list and length
		  (list (sub-list lst 0 (/ (- (length lst) 1) 2)) (sub-list lst (/ (- (length lst) 1) 2) (- (length lst) 1))))
	      ())))

    
    ;;Sorts an unordered list numerically.
    (define (merge-sort lst)
      (if (or (null? lst) (null? (cdr lst)))
	  lst
	  (if (list? lst)
	      (merge-lst (merge-sort (car (sub-list-split lst))) (merge-sort (cadr (sub-list-split lst)))) ;splits the list into two by calling sub-list-split, and taking the car and cadr, then sends both of those back into merge-sort, after merging the lists.
	      ())))

    
    ;;Gets the sorted list of all virus amounts
    (define (virus-list player board list)
      (if (null? list)
	  ()
	  (merge-sort (cons (get-virus-count list) (virus-list player board (cdr list))))))
    
        
    ;;Gets the 2nd largest cell
    (define (get-2nd player board)
      (get-cell-value player board (list-ref (merge-sort (virus-list player board (get-cell-list player board))) (- (length (virus-list player board (get-cell-list player board))) 2))))
    
    
    ;;Returns a list of all un-owned cells
    (define (get-open-cell player board)
      (define (helper player board list)
	(if (null? board)
	    list
	    (if (equal? (car (cdddar board)) *NEUTRAL-SYMBOL*);If the cell has the symbol 'N', then it is a neutral cell, and is added to the list.
		(helper player (cdr board) (cons (caar board) list))
		(helper player (cdr board) list))))
      (helper player board '()))
    

    ;;Returns the term in the cell-list with the owner.  Should be called with (car list).
    (define (get-owner list)
      (car (cdddar list)))    
    
    
    ;;Returns the closest cell to a given cell
    (define (closest-cell cell player board)
      (define (helper cell player board otpcelllist closest original)
	(if (= (length otpcelllist) 1);when one term is left, it tests it against the closest current cell, to account for it being the smallest.
	    (if (= closest original)
		(car otpcelllist)
	        (if (> (distance cell (car otpcelllist) board) (distance cell closest board))
		    closest
		    (car otpcelllist)))
	    (if (> (distance cell (car otpcelllist) board) (distance cell closest board));Tests the distance between two cells. 
		(helper cell player board (cdr otpcelllist) closest original)
		(helper cell player board (cdr otpcelllist) (car otpcelllist) original))))
      (helper cell player board (get-cell-number-list (other-player player) board) (car (get-cell-number-list (other-player player) board)) (car (get-cell-number-list (other-player player) board))))
    

    ;;Returns the closest open cell to a given cell, if none open, returns closest cell to that cell
    (define (closest-open-cell cell player board queue)
      (define (helper cell player board openlist closest original queue)
	(if (null? openlist)
	    (closest-cell cell player board)
	    (if (= (length openlist) 1)
		(if (= closest (car original))
		    (car original)
		    (if (>= (distance cell (car openlist) board) (distance cell closest board));
			closest
			(car openlist)))
		(if (allattacked? (car openlist) player board queue);if I am already attacking that cell, don't attack
		    (helper cell player board (cdr openlist) closest original queue)
		    (if (>= (distance cell (car openlist) board) (distance cell closest board));testing the distances
			(helper cell player board (cdr openlist) closest original queue)
			(helper cell player board (cdr openlist) (car openlist) original queue))))))
	(helper cell player board (get-open-cell player board) (car (get-open-cell player board)) (get-open-cell player board) queue))
      
      
      ;;Returns the closest of my cells to an open cell
      (define (closest-my-cell cell player board)
	(define (helper cell player board otpcelllist closest original)
	  (if (= (length otpcelllist) 1)
	      (if (= closest original)
		  (car otpcelllist)
		  (if (> (distance cell (car otpcelllist) board) (distance cell closest board))
		      closest
		      (car otpcelllist)))
	      (if (> (distance cell (car otpcelllist) board) (distance cell closest board)) 
		  (helper cell player board (cdr otpcelllist) closest original)
		  (helper cell player board (cdr otpcelllist) (car otpcelllist) original))))
	(helper cell player board (get-cell-number-list player board) (car (get-cell-number-list player board)) (car (get-cell-number-list player board))))
      
      
      ;;A procedure that will shoot from the largest cell to any cell it can overtake, otherwise attack at random
      (define (whisper player board)
	(define (helper player board temp-list)
	  (if (null? temp-list)
	      (list (make-move (get-largest-cell player board)
			       (closest-open-cell (get-largest-cell player board) player board)))
	      (if (>= (get-attack-count (get-largest-cell player board) board) (get-virus-count temp-list))
		  (list (make-move (get-largest-cell player board) (caar temp-list)))
		  (helper player board (cdr temp-list)))))
	(helper player board (get-cell-list (other-player player) board)))

      
      ;;Returns true if my largest cell can overtake any opposing cells.
      (define (whisper-base player board)
	(define (helper player board temp-list)
	  (if (null? temp-list)
	      #f
	      (if (>= (get-attack-count (get-largest-cell player board) board) (get-virus-count temp-list))
		  #t
		  (helper player board (cdr temp-list)))))
	(helper player board (get-cell-list (other-player player) board)))
      
      ;;Tells if one cell can overtake the closest cell to it
      (define (whisper-cell cell player board)
	(if (> (get-attack-count cell board) (get-attack-count (closest-cell cell player board) board))
	    #t
	    #f))

      
      ;;Returns #t if there are any open cells on the map
      (define (opencells? player board)
	(if (> (length (get-open-cell player board)) 0)
	    #t
	    #f))

      
      ;;Returns true if that cell is being attacked
      (define (attacked? cell player board queue)
	(if (null? (cdr queue))
	    #f
	    (if (eq? (cadddr (cddadr queue)) (player-string player))
		(attacked? cell player board (cdr queue))
		(if (and (> (cadadr queue) 0) (= (cadddr (cadr queue)) cell))
		    #t
		    (attacked? cell player board (cdr queue))))))

      
      ;;Returns true if I am attacking that cell already
      (define (allattacked? cell player board queue)
	(if (null? (cdr queue))
	    #f
	    (if (eq? (cadddr (cddadr queue)) (player-string (other-player player)));if the other player is attacking, check next attack.
		(attacked? cell player board (cdr queue))
		(if (and (> (cadadr queue) 0) (= (cadddr (cadr queue)) cell));Any call that is here is a call made from me, if it is the cell and has an attack value, return #t.
		    #t
		    (attacked? cell player board (cdr queue))))))
      

      ;;Returns the enemy cell with the lowest virus count
      (define (lowest-cell player board)
	(define (helper player board cell-list cell virus)
	  (if (null? cell-list)
	      cell
	      (if (> (car (get-virus-value player board (car cell-list))) (car virus))
		  (helper player board (cdr cell-list) cell virus)
		  (helper player board (cdr cell-list) (car cell-list) (get-virus-value player board (car cell-list))))))
	(helper player board (get-cell-number-list (other-player player) board) 0 (get-virus-value player board (car (get-cell-number-list (other-player player) board)))))

      
      ;;Checks if an integer is in a list of integers.
      (define (element? x list)
	(if (null? list)
	    #f
	    (if (= x (car list))
		#t
		(element? x (cdr list)))))
      
      
      ;;If that cell can be subject to a one move attack returns true
      (define (onemove player board cell-list)
	(if (and (<= (distance (closest-cell (car cell-list) player board) (car cell-list) board) 4) (>= (car (get-virus-value player board (closest-cell (car cell-list) player board))) (/ (car (get-virus-value player board (car cell-list))) 2)))
	    #t
	    #f))


      ;;If that cell can attack the closest cell to it in one move return true.  Not used in code, because all cells are already attacking closest cells.
      (define (oneattack player board cell-list)
	(if (and (<= (distance (closest-cell (car cell-list) player board) (car cell-list) board) 4) (<= (car (get-virus-value player board (closest-cell (car cell-list) player board))) (get-attack-count (car cell-list) board)))
	    #t
	    #f))

      
      ;;If my only cell is being attacked and there is an open cell, it returns true
      (define (stalemate player board)
	(if (and (= (length (get-cell-number-list player board)) 1) (>= (length (get-open-cell player board)) 1))
	    #t
	    #f))

      
      ;;If all cells are stuck defending returns true.
      (define (allstale player board queue cell-list)
	(if (null? cell-list)
	    #t
	    (if (or (attacked? (car cell-list) player board queue) (onemove player board cell-list))
		(allstale player board queue (cdr cell-list))
		#f)))

      
      ;;If the nearest open cell is not attackable with current virus count, return true
      (define (attackable player board queue cell-list)
	(if (> (car (get-virus-value player board (closest-open-cell (car cell-list) player board queue))) (get-attack-count (car cell-list) board))
	    #t
	    #f))
      
      ;;Main game-playing procedure.  Uses a pretty simple strategy.  See individual base-cases above for what they do.  But if there is an open-cell, it attacks.  When there are no open cells all cells will attack their nearest cell.  A few built-in safe guards for defensive purposes as well.
      (define (p3 player board queue)
	(define (helper player board cell-list call queue open)
	  (if (and (null? cell-list) (null? call))
	      (defend player board)
	      (if (null? cell-list)
		  call
		  (if (onemove player board cell-list)
		      (helper player board (cdr cell-list) call queue open)       
		      (if (attacked? (car cell-list) player board queue)
			  (if (stalemate player board)
			      (helper player board (cdr cell-list) (cons (make-move (car cell-list) (closest-open-cell (car cell-list) player board queue)) call) queue open)
			      (helper player board (cdr cell-list) call queue open))
			  (if (null? (get-open-cell player board))
			      (helper player board (cdr cell-list) (cons (make-move (car cell-list) (closest-cell (car cell-list) player board)) call) queue open)
			      (if (attackable player board queue cell-list)
				  (helper player board (cdr cell-list) (cons (make-move (car cell-list) (closest-cell (car cell-list) player board)) call) queue open)
				  (if (element? (closest-open-cell (car cell-list) player board queue) open)
				      (helper player board (cdr cell-list) (cons (make-move (car cell-list) (closest-open-cell (car cell-list) player board queue)) call) queue (remove (lambda (x) (eq? x (closest-open-cell (car cell-list) player board queue))) open))
				      (helper player board (cdr cell-list) (cons (make-move (car cell-list) (closest-cell (car cell-list) player board)) call) queue open)))))))))
	(helper player board (get-cell-number-list player board) () queue (get-open-cell player board)))
      

      
      ;;====================;;
      ;; Get-Move Procedure ;;
      ;;===============================================================;;
      ;; This is the procedure called by dots++.scm to get your move.  ;;
      ;; Returns a line-position object.
      ;;===============================================================;;
      
      ;; Main procedure
      (define (get-move player queue board)
	(p3 player board queue))
      
      
      
      ;; Return get-move procedure
      get-move
      
      )) ;; End of player-procedure
  

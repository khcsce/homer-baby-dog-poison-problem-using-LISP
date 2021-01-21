;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;Homework 2
;NAME: Khoa Quach
;ID: 105123806

; TODO: comment code
(defun BFS (FRINGE)
	(cond ((null FRINGE) nil) ; return nil, if empty list (null)
		; extra step to validate input is a list (not needed but to be safe)
		((atom FRINGE) '(FRINGE));if it is just an atom, we can just return the atom to be safe
		; Now check the current node of the tree
		; Case 1: atom
		;((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE)))) ; recusively call on the rest
		; Case 2: list
		;(t (BFS (append (cdr FRINGE) (car FRINGE))))
		; Case 1: list
		((listp (car FRINGE)) (BFS (append (cdr FRINGE) (car FRINGE)))) ; if it is a list, we have
		; to append the cdr to to the car, we keep doing this so we find an atom
		; since we need to deal with the atoms before expanding these lists
		; Idea: expand node and add the children
		; Case 2: atom
		(t (cons (car FRINGE) (BFS (cdr FRINGE)))) ; recusively call on the rest
	)
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (equal S `(T T T T))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
	;(let (first S) (first S)) ; for readability
	;(let (second S) (second S))
	;(let (third S) (third S))
	;(let (fourth S) (fourth S))
	; This gave me the error: ERROR: CAR called on non-list argument,  so I used the long syntax to access the position of the list
 	; h for (first S) only, b for (first S) with (second S), d for hoomer with (third S), p for (first S) with (fourth S)
 	(cond 
 		; Case 1 : homer
 		((equal A 'h) ; entity h
	 		; invalid state (1) if the baby is alone with poison  or (2) if the baby is alone with the dog
	 		; (3) action is impossiblle  => homer is not on the same side as the entity
 			(cond ((and (equal (first S) (second S)) (or (equal (second S) (third S)) (equal (second S) (fourth S)))) nil) ; return nil if invalid state
 			; Otherwise, move (so we have to negate (first S))
 				(t (list (cons (not (car S)) (cdr S)))) ; return a list
 			)
 			; I don't know if clisp does things by reference so I used NOT (car S) instead of the local variable
 		) ; closing equal
 		; Case 2: homer with baby
 		((equal A 'b)
 		 	; Homer has to be with baby
 			(cond ((not (equal (first S) (second S))) nil) ; return nil if invalid state
 				(t (list (list (not (car S)) (not (second S)) (third S) (fourth S))))
 		  	)
 		); closing equal
 		; Case 3: home with dog
 		((equal A 'd)
 			(cond ((not (equal (first S) (third S))) nil) ; invalid state: homer (first S) and dog (third S) on different sides
 				((equal (second S) (fourth S)) nil) ; invalid state: baby (second S) alone with poison(fourth S)
 				(t (list (list (not (car S)) (second S) (not (third S)) (fourth S))))
 			)
 		) ; closing 
 		; Case 3: homer with poison
 		((equal A 'p) 
 			(cond ((not (equal (first S) (fourth S))) nil) ;invalid state: homer (first S) and  poison(fourth S) on different sides
 				((equal (second S) (third S)) nil) ; invalid state: baby (second S) alone with dog (third S)
 				(t (list (list (not (car S)) (second S) (third S) (not (fourth S)))))
 			)
 		)
 		(t nil)
 	) ; outer cond
 	; return: list containing the state that results from that move.
 )


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
    ; h for homer only, b for homer with baby, d for hoomer with dog, p for homer with poison
    ; use append since the function returns a list containing the state 
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
	(if (null STATES)
		nil
		(if (equal S (first STATES))
			T
			(ON-PATH S (cdr STATES))
		)
	)
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
	(cond ((null STATES) nil)
		; could also done visited already logic here instead of DFS function
		(t
			(cond ((null (DFS (car STATES) PATH)) ;(DFS (car STATES) PATH) is the "argument"
				(MULT-DFS (cdr STATES) PATH)) ;if ; if no path exists, test other states at the same L
				(t (DFS (car STATES) PATH)) ; else
			)
		)
	)
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH) 
	(if (FINAL-STATE S)  	; final state reached
		(append PATH (list S)) ; goal statefound so append to path
		(if (ON-PATH S PATH)
			nil        		; visited already
			(MULT-DFS (SUCC-FN S) (append PATH (list S))) ; otherwise, use MULT-DFS with 
			; SUCC-FN (which expands the node) and appending the path to the list S
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GENERATION FUNCTIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a set of replacement rules RULES, applies them N times to an intial sequence STARTING-CHAIN.
;; Cannot account for contextual rulesets!
(defun apply-rules (rules starting-chain n &key (context nil))
  (let ((chain starting-chain))
    (if (not context)
	(dotimes (i n)
	  (setf chain (apply #'append
			     (map 'list rules chain))))
      (dotimes (i n)
	(labels ((contextual-apply (pre x remainder)
				   (append (funcall rules pre x (car remainder))
					   (when remainder
					     (contextual-apply x (car remainder) (cdr remainder))))))
		(setf chain (contextual-apply nil (car chain) (cdr chain))))))
    chain))


;; Given a name NAME, applies the set of replacement rules NAME-RULES N times to an initial sequence *NAME-AXIOM*.
(defun apply-rules-from-name (name n &key (context nil))
  (let ((rules (eval (read-from-string (concatenate 'string "#'" name "-rules"))))
	(chain (eval (read-from-string (concatenate 'string "*" name "-axiom*")))))
    (apply-rules rules chain n :context context)))

;; Converts a list of user-defined replacement rules into a function. It is assumed that the replacement rules are given
;; as a list, where each element of that list is of the form (VARIABLE REPLACEMENT).
(defun make-rules (rules)
  (lambda (x)
    (block fake-cond
	   (dolist (rule rules)
	     (when (equal x (car rule))
	       (return-from fake-cond (cadr rule))))
	   (list x))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PRE-DEFINED L-SYSTEMS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A list of the pre-defined (and thus protected) system names
(defparameter *system-list* (list "algae" "cantor" "hilbert2" "sierpinski" "thue-morse" "gosper" "simple-stochastic"))

;; Lindenmayer's original algae model
;; variables: a,b
;; constants: none
(defun algae-rules (x)
  (cond ((equal x #\a) (list #\a #\b))
	((equal x #\b) (list #\a))
	(t (list x))))
(defparameter *algae-axiom* (list #\a))
(defparameter *algae-variables* (list #\a #\b))

;; Sierpinski triangle
;; variables: f,g (both meaning "draw forwards" by the same amount)
;; constants: +,- (meaning "turn left (by pi/3)" and "turn right (by pi/3)")
(defun sierpinski-rules (x)
  (cond ((equal x #\f) (list #\f #\- #\g #\+ #\f #\+ #\g #\- #\f))
	((equal x #\g) (list #\g #\g))
	(t (list x))))
(defparameter *sierpinski-axiom* (list #\f #\- #\g #\- #\g))
(defparameter *sierpinski-variables* (list #\f #\g))

;; Cantor set
;; variables: f,e (meaning "draw forward" and "move forward", both by the same amount)
;; constants: none
(defun cantor-rules (x)
  (cond ((equal x #\f) (list #\f #\e #\f))
	((equal x #\e) (list #\e #\e #\e))
	(t (list x))))
(defparameter *cantor-axiom* (list #\f))
(defparameter *cantor-variables* (list #\f #\e))

;; Hilbert curve in two dimensions
;; variables: a,b (no meaning)
;; constants: f,+,- (meaning "draw forward", "turn left (pi/2)", "turn right (pi/2)")
(defun hilbert2-rules (x)
  (cond ((equal x #\a) (list #\+ #\b #\f #\- #\a #\f #\a #\- #\f #\b #\+))
	((equal x #\b) (list #\- #\a #\f #\+ #\b #\f #\b #\+ #\f #\a #\-))
	(t (list x))))
(defparameter *hilbert2-axiom* (list #\a))
(defparameter *hilbert2-variables* (list #\a #\b))

;; Thue-Morse sequence
;; variables: 0,1
;; constants: none
(defun thue-morse-rules (x)
  (cond ((equal x #\0) (list #\0 #\1))
	((equal x #\1) (list #\1 #\0))
	(t (list x))))
(defparameter *thue-morse-axiom* (list #\0))
(defparameter *thue-morse-variables* (list #\0 #\1))

;; Gosper curve
;; variables: f,g (both meaning "draw forwards" by the same amount)
;; constants: +,- (meaning "turn left (by pi/6)", "turn right (by pi/6)")
(defun gosper-rules (x)
  (cond ((equal x #\f) (list #\f #\- #\g #\- #\- #\g #\+ #\f #\+ #\+ #\f #\f #\+ #\g #\-))
	((equal x #\g) (list #\+ #\f #\- #\g #\g #\- #\- #\g #\- #\f #\+ #\+ #\f #\+ #\g))
	(t (list x))))
(defparameter *gosper-axiom* (list #\a))
(defparameter *gosper-variables* (list #\f #\g))

;; A simple stochastic process I made up to test stochastic grammars.
;; variables: a,b
;; constants: none
(defun simple-stochastic-rules (x)
  (cond ((equal x #\a) (if (< (random 1.0) 0.5) (list #\b) (list #\a #\a)))
	((equal x #\b) (if (< (random 1.0) 0.5) (list #\b) (list #\a)))
	(t (list x))))
(defparameter *simple-stochastic-axiom* (list #\a))
(defparameter *simple-stochastic-variables* (list #\a #\b))

;; A simple contextual process I made up to test contextual grammars.
;; PRE means the element before X (the "predecessor")
;; SUC means the element after X (the "successor")
;; X has the same meaning as is implied in context-free languages, i.e., it is the element of the list that we're
;; applying the ruleset to.
;; variables: a,b,c
;; constants: none
(defun simple-contextual-rules (pre x suc)
  (cond ((equal x #\a) (cond ((equal pre #\c) (list #\b))
			     ((equal pre #\b) (list #\c))
			     (t (list #\a))))
	((equal x #\b) (cond ((and (equal pre #\a) (equal suc #\c)) (list #\c #\b #\a))
			     (t (list #\b))))
	((equal x #\c) (list #\a #\b #\c))
	(t (list x))))
(defparameter *simple-contextual-axiom* (list #\a #\b #\c))
(defparameter *simple-contextual-variables* (list #\a #\b #\c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VISUALIZATION FUNCTIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UTILITY FUNCTIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts a list of characters to a string
(defun chars-to-string (chars) (coerce chars 'string))

;; Prints out the names of the predefined L-systems
(defun print-names ()
  (format t "The pre-defined L-systems are: ~{~A~^, ~}" *system-list*))

;; Prints out the axiom and replacement rules for the NAME L-system.
;; Doesn't work properly with a stochastic L-system.
(defun print-info (name)
  (let
      ((axiom (chars-to-string (eval (read-from-string (concatenate 'string "*" name "-axiom*")))))
       (rules (loop for i in (eval (read-from-string (concatenate 'string "*" name "-variables*")))
		    collect (list (string i)
				  (chars-to-string (apply-rules
						    (eval (read-from-string (concatenate 'string "#'" name "-rules")))
						    (list i) 1))))))
    (format t "The axiom for \"~A\" is: ~S~%" name axiom)
    (format t "The replacement rules for \"~A\" are:~%" name)
    (format t "~{    ~{~S~^ becomes ~}~^~%~}" rules)))

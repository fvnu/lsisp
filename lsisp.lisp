;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GENERATION FUNCTIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a set of replacement rules RULES, applies them N times to an intial sequence STARTING-CHAIN.
;; Cannot account for contextual rulesets!
(defun apply-rules (rules starting-chain n)
  (let ((chain starting-chain))
    (dotimes (i n)
      (setf chain (apply #'append
			 (map 'list rules chain))))
    chain))

;; Given a name NAME, applies the set of replacement rules NAME-RULES N times to an initial sequence *NAME-AXIOM*.
(defun apply-rules-from-name (name n)
  (let ((rules (eval (read-from-string (concatenate 'string "#'" name "-rules"))))
	(chain (eval (read-from-string (concatenate 'string "*" name "-axiom*")))))
    (apply-rules rules chain n)))

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
(defparameter *system-list* (list "algae" "cantor" "hilbert2" "sierpinski" "thue-morse" "gosper"))

;; Lindenmayer's original algae model
;; variables: a,b
;; constants: none
(defun algae-rules (x)
  (cond ((equal x #\a) (list #\a #\b))
	((equal x #\b) (list #\a))
	(t (list x))))
(defparameter *algae-axiom* (list #\a))

;; Sierpinski triangle
;; variables: f,g (both meaning "draw forwards" by the same amount)
;; constants: +,- (meaning "turn left (by pi/3)" and "turn right (by pi/3)")
(defun sierpinski-rules (x)
  (cond ((equal x #\f) (list #\f #\- #\g #\+ #\f #\+ #\g #\- #\f))
	((equal x #\g) (list #\g #\g))
	(t (list x))))
(defparameter *sierpinski-axiom* (list #\f #\- #\g #\- #\g))

;; Cantor set
;; variables: a,b (meaning "draw forward" and "move forward", both by the same amount)
;; constants: none
(defun cantor-rules (x)
  (cond ((equal x #\a) (list #\a #\b #\a))
	((equal x #\b) (list #\b #\b #\b))
	(t (list x))))
(defparameter *cantor-axiom* (list #\a))

;; Hilbert curve in two dimensions
;; variables: a,b (no meaning)
;; constants: f,+,- (meaning "draw forward", "turn left (pi/2)", "turn right (pi/2)")
(defun hilbert2-rules (x)
  (cond ((equal x #\a) (list #\+ #\b #\f #\- #\a #\f #\a #\- #\f #\b #\+))
	((equal x #\b) (list #\- #\a #\f #\+ #\b #\f #\b #\+ #\f #\a #\-))
	(t (list x))))
(defparameter *hilbert2-axiom* (list #\a))

;; Thue-Morse sequence
;; variables: 0,1
;; constants: none
(defun thue-morse-rules (x)
  (cond ((equal x 0) (list 0 1))
	((equal x 1) (list 1 0))
	(t (list x))))
(defparameter *thue-morse-axiom* (list 0))

;; Gosper curve
;; variables: f,g (both meaning "draw forwards" by the same amount)
;; constants: +,- (meaning "turn left (by pi/6)", "turn right (by pi/6)")
(defun gosper-rules (x)
  (cond ((equal x #\f) (list #\f #\- #\g #\- #\- #\g #\+ #\f #\+ #\+ #\f #\f #\+ #\g #\-))
	((equal x #\g) (list #\+ #\f #\- #\g #\g #\- #\- #\g #\- #\f #\+ #\+ #\f #\+ #\g))
	(t (list x))))
(defparameter *gosper-axiom* (list #\a))

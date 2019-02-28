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
(defparameter *system-list* (list "algae" "cantor" "hilbert2" "sierpinski" "thue-morse" "gosper" "simple-stochastic" "simple-contextual"))

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
  (cond ((equal x #\f) (list #\f #\h #\f))
	((equal x #\h) (list #\h #\h #\h))
	(t (list x))))
(defparameter *cantor-axiom* (list #\f))
(defparameter *cantor-variables* (list #\f #\h))

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
;; constants: +,- (meaning "turn left (by pi/3)", "turn right (by pi/3)")
(defun gosper-rules (x)
  (cond ((equal x #\f) (list #\f #\- #\g #\- #\- #\g #\+ #\f #\+ #\+ #\f #\f #\+ #\g #\-))
	((equal x #\g) (list #\+ #\f #\- #\g #\g #\- #\- #\g #\- #\f #\+ #\+ #\f #\+ #\g))
	(t (list x))))
(defparameter *gosper-axiom* (list #\f))
(defparameter *gosper-variables* (list #\f #\g))

;; Binary tree
;; Variables: f,g (both meaning "draw forwards" by the same amount)
;; constants: [,] (meaning "store current position/pointing, turn left (by pi/4)" and "retrieve last stored position/pointing, turn right (by pi/4)")
(defun binary-tree-rules (x)
  (cond ((equal x #\f) (list #\g #\[ #\f #\] #\f))
	((equal x #\g) (list #\g #\g))
	(t (list x))))
(defparameter *binary-tree-axiom* (list #\f))
(defparameter *binary-tree-variables* (list #\f #\g))

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
;; X has the same meaning as is implied in context-free languages, i.e., it is the element of the list that we're applying the ruleset to.
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VISUALIZATION FUNCTIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Empty characters, i.e., characters which are not supposed to result in any drawing command
(defvar *empty-characters* (list #\a #\b #\c #\0 #\1))

;; An association list of characters and their interpretation by SENTENCE-TO-POINTS
(defparameter *dictionary* (list (cons #\a "do nothing") (cons #\b "do nothing") (cons #\c "do nothing")
				 (cons #\f "draw forwards by :STEP-SIZE") (cons #\g "draw forwards by :STEP-SIZE")
				 (cons #\h "move forwards by :STEP-SIZE (do not draw)")
				 (cons #\+ "turn left by :ANGLE")
				 (cons #\- "turn right by :ANGLE")
				 (cons #\[ "store current position and direction; turn ccw by :ANGLE")
				 (cons #\[ "load position and direction; turn cw by :ANGLE")
				 (cons #\0 "do nothing") (cons #\1 "do nothing")))

;; For a given character, gives its standard meaning according to *DICTIONARY*
(defun query-dictionary (character)
  (let ((entry (assoc character *dictionary*)))
    (if entry
	(format t "~c means ~S" (car entry) (cdr entry))
      (format t "~c is not in the dictionary" character))))

;; Prints the entire dictionary
(defun print-dictionary () (dolist (entry *dictionary*) (format t "~c means ~S~%" (car entry) (cdr entry))))

;; Defines a vector of single-floats, with elements given as a list ELEMENTS
(defun make-vector (&rest elements) (map 'list #'(lambda (x) (float x 1.0s0)) elements))

;; Defines a vector sum operation
(defun vector+ (v1 v2) (map 'list #'(lambda (e1 e2) (+ e1 e2)) v1 v2))

;; Defines a vector scaling operation
(defun vector-scale (v a) (map 'list #'(lambda (e) (* a e)) v))

;;Defines a vector rotation operation on a 2d vector
(defun vector-rotate2 (v p)
  (let ((x (nth 0 v)) (y (nth 1 v)))
    (make-vector (+ (* x (cos p)) (* y (sin p))) (- (* y (cos p)) (* x (sin p))))))

;; Given a SENTENCE (in the form of a list of characters), converts it into a list of points according
;; to the standard set of instructions as defined by the keywords which follow:
;;     INITIAL-POS is the starting location of the cursor.
;;     INITIAL-DIR is the starting pointing of the cursor. This should always be of unit length.
;;     ANGLE is the angle (in radians) by which the #\+ or #\- commands will rotate the cursor.
;;     STEP-SIZE is how far the cursor will move (arb. units)
;; Returns by default a list of the points, but setting :SAVE-AS to some string will instead save the points to
;; the file ":SAVE-AS.dat". Note that this will destructively overwrite any existing files by that name.
(defun sentence-to-points (sentence &key (save-as nil) (initial-pos (make-vector 0 0)) (initial-dir (make-vector 0 1))
				    (angle (/ pi 2)) (step-size 1.0))
  (let ((points (list initial-pos))
	(pos initial-pos)
	(dir initial-dir)
	(saved-data nil))
    (dolist (i sentence)
      (cond
       ;;meaning "draw forwards"
       ((or (equal i #\f) (equal i #\g)) (setf pos (vector+ pos (vector-scale dir step-size))) (push pos points))
       ;;meaning "move forwards (but do not draw)"
       ((equal i #\h) (setf pos (vector+ pos (vector-scale dir step-size))) (push nil points) (push pos points))
       ;;meaning "turn ccw by ANGLE"
       ((equal i #\+) (setf dir (vector-rotate2 dir (- angle))))
       ;;meaning "turn cw by ANGLE"
       ((equal i #\-) (setf dir (vector-rotate2 dir angle)))
       ;;meaning:
       ((equal i #\[)
	(push (list pos dir) saved-data) ;store the current position and direction
	(setf dir (vector-rotate2 dir (- angle)))) ;turn ccw by ANGLE
       ;;meaning:
       ((equal i #\])
	(push nil points) ;make a new branch
	(multiple-value-setq (pos dir) (values-list (pop saved-data))) (push pos points) ;load the last saved position and direction
	(setf dir (vector-rotate2 dir angle))) ;turn cw by ANGLE
       ;;meaning nothing, but still possibly present in a sentence
       ((member i *empty-characters*))
       ;;no defined meaning
       (t (format t "I don't recognize ~S as a valid command and have skipped it.~%" i))))
    (if save-as
	(with-open-file
	 (s (format nil "~A.dat" save-as) :direction :output :if-does-not-exist :create :if-exists :overwrite)
	 (format s "~{~{~$~^ ~}~%~}" points))
      points)))

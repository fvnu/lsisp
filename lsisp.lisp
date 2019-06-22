;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GENERATION FUNCTIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defines the L-SYSTEM structure
(defstruct (l-system (:conc-name lsys-))
  name ;the name of the l-system, as a string
  variables ;the list of variables, given as chars
  axiom ;the initial list of chars to start from
  rules ;the replacement rules for variables
  (contextual nil) ;set to T if the :RULES are contextual
)

;; Given a set of replacement rules RULES, applies them N times to an intial sequence STARTING-CHAIN.
;; Can account for contextual rulesets: set the flag :CONTEXT T
(defun apply-rules (rules starting-chain n &key (context nil))
  (let ((chain starting-chain))
    (if (not context)
	(dotimes (i n)
	  (setf chain (apply #'append
			     (map 'list rules chain))))
      (dotimes (i n)
	(labels ((contextual-apply (x pre remainder)
				   (append (funcall rules x pre (car remainder))
					   (when remainder
					     (contextual-apply (car remainder) x (cdr remainder))))))
		(setf chain (contextual-apply (car chain) nil (cdr chain))))))
    chain))


;; Given an L-SYSTEM NAME, applies the set of replacement rules N times
(defun apply-rules-from-name (name n)
    (apply-rules (lsys-rules name) (lsys-axiom name) n :context (lsys-contextual name)))

;; Converts a list of user-defined replacement rules into a function. It is assumed that the replacement rules are given
;; as a list, where each element of that list is of the form (VARIABLE . REPLACEMENT).
(defun make-rules (rules)
  (lambda (x)
    (block fake-cond
	   (dolist (rule rules)
	     (when (equal x (car rule))
	       (return-from fake-cond (list (cdr rule)))))
	   (list x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PRE-DEFINED L-SYSTEMS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lindenmayer's original algae model
;; variables: a,b (no meaning)
;; constants: none
(defvar *algae*
  (make-l-system
   :name "algae"
   :variables (list #\a #\b)
   :axiom (list #\a)
   :rules (lambda (x)
            (cond
             ((equal x #\a) (list #\a #\b))
             ((equal x #\b) (list #\a))
             (t (list x))))))

;; Sierpinski triangle
;; variables: f,g (both meaning "draw forwards" by the same amount)
;; constants: +,- (meaning "turn left (by pi/3)" and "turn right (by pi/3)")
(defvar *sierpinski*
  (make-l-system
   :name "sierpinski"
   :variables (list #\f #\g)   
   :axiom (list #\f #\- #\g #\- #\g)
   :rules (lambda (x)
	    (cond
	     ((equal x #\f) (list #\f #\- #\g #\+ #\f #\+ #\g #\- #\f))
	     ((equal x #\g) (list #\g #\g))
	     (t (list x))))))

;; Cantor set
;; variables: f,e (meaning "draw forward" and "move forward", both by the same amount)
;; constants: none
(defvar *cantor*
  (make-l-system
   :name "cantor"
   :variables (list #\f #\e)
   :axiom (list #\f)
   :rules (lambda (x)
	    (cond ((equal x #\f) (list #\f #\e #\f))
		  ((equal x #\e) (list #\e #\e #\e))
		  (t (list x))))))

;; Hilbert curve in two dimensions
;; variables: a,b (no meaning)
;; constants: f,+,- (meaning "draw forward", "turn left (pi/2)", "turn right (pi/2)")
(defvar *hilbert2*
  (make-l-system
   :name "hilbert2"
   :variables (list #\a #\b)
   :axiom (list #\a)
   :rules (lambda (x)
	    (cond ((equal x #\a) (list #\+ #\b #\f #\- #\a #\f #\a #\- #\f #\b #\+))
		  ((equal x #\b) (list #\- #\a #\f #\+ #\b #\f #\b #\+ #\f #\a #\-))
		  (t (list x))))))

;; Thue-Morse sequence
;; variables: 0,1 (no meaning)
;; constants: none
(defvar *thue-morse*
  (make-l-system
   :name "thue-morse"
   :variables (list #\0 #\1)
   :axiom (list #\0)
   :rules (lambda (x)
	    (cond ((equal x #\0) (list #\0 #\1))
		  ((equal x #\1) (list #\1 #\0))
		  (t (list x))))))
(defparameter *thue-morse-to-koch-limit* (list (cons #\0 #\f) (cons #\1 #\+))) ;with an angle of pi/3

;; Gosper curve
;; variables: f,g (both meaning "draw forwards" by the same amount)
;; constants: +,- (meaning "turn left (by pi/3)", "turn right (by pi/3)")
(defvar *gosper*
  (make-l-system
   :name "gosper"
   :variables (list #\f #\g)
   :axiom (list #\f)
   :rules (lambda (x)
	    (cond ((equal x #\f) (list #\f #\- #\g #\- #\- #\g #\+ #\f #\+ #\+ #\f #\f #\+ #\g #\-))
		  ((equal x #\g) (list #\+ #\f #\- #\g #\g #\- #\- #\g #\- #\f #\+ #\+ #\f #\+ #\g))
		  (t (list x))))))

;; Binary tree
;; Variables: f,g (both meaning "draw forwards" by the same amount)
;; constants: [,] (meaning "store current position/pointing, turn left (by pi/4)" and "retrieve last stored position/pointing, turn right (by pi/4)")
(defvar *binary-tree*
  (make-l-system
   :name "binary-tree"
   :variables (list #\f #\g)
   :axiom (list #\f)
   :rules (lambda (x)
	    (cond ((equal x #\f) (list #\g #\[ #\f #\] #\f))
		  ((equal x #\g) (list #\g #\g))
		  (t (list x))))))

;; Ternary tree
;; Variables: f,g (both meaning "draw forwards" by the same amount)
;; constants: [,],| (meaning "store current position/pointing, turn left (by pi/4)", "retrieve last stored position/pointing, turn right (by pi/4)", "access last stored position/pointing")
(defvar *ternary-tree*
  (make-l-system
   :name "ternary-tree"
   :variables (list #\f #\g)
   :axiom (list #\f)
   :rules (lambda (x)
	    (cond ((equal x #\f) (list #\g #\[ #\f #\| #\f #\] #\f))
		  ((equal x #\g) (list #\g #\g))
		  (t (list x))))))

;; A simple stochastic process I made up to test stochastic grammars.
;; variables: a,b (no meaning)
;; constants: none
(defvar *simple-stochastic*
  (make-l-system
   :name "simple-stochastic"
   :variables (list #\a #\b)
   :axiom (list #\a)
   :rules (lambda (x)
	    (cond ((equal x #\a) (if (< (random 1.0) 0.5) (list #\b) (list #\a #\a)))
		  ((equal x #\b) (if (< (random 1.0) 0.5) (list #\b) (list #\a)))
		  (t (list x))))))

;; A simple contextual process I made up to test contextual grammars.
;; PRE means the element before X (the "predecessor")
;; SUC means the element after X (the "successor")
;; X has the same meaning as is implied in context-free languages, i.e., it is the element of the list that we're applying the ruleset to.
;; variables: a,b,c
;; constants: none
(defvar *simple-contextual*
  (make-l-system
   :name "simple-contextual"
   :variables (list #\a #\b #\c)
   :axiom (list #\a #\b #\c)
   :rules (lambda (x pre suc)
	    (cond ((equal x #\a) (cond ((equal pre #\c) (list #\b))
				       ((equal pre #\b) (list #\c))
				       (t (list #\a))))
		  ((equal x #\b) (cond ((and (equal pre #\a) (equal suc #\c)) (list #\c #\b #\a))
				       (t (list #\b))))
		  ((equal x #\c) (list #\a #\b #\c))
		  (t (list x))))))

;; A list of the pre-defined (and thus protected) system names
(defparameter *system-list* (list "algae" "cantor" "hilbert2" "sierpinski" "thue-morse" "gosper" "binary-tree" "ternary-tree" "simple-stochastic" "simple-contextual"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UTILITY FUNCTIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts a list of characters to a string
(defun chars-to-string (chars) (coerce chars 'string))

;; Converts some characters in a list to other characters. RULES must be the same as in MAKE-RULES, i.e., a list of (CHAR . REPLACEMENT)
(defun translate-list (list rules)
  (apply-rules (make-rules rules) list 1))

;; Prints out the names of the predefined L-systems
(defun print-names ()
  (format t "The pre-defined L-systems are: ~{~A~^, ~}" *system-list*))

;; Prints out the axiom and replacement rules for the L-SYSTEM NAME.
;; Doesn't work properly with a stochastic L-system.
(defun print-info (name)
  (let*
      ((axiom (lsys-axiom name))
       (rules (lsys-rules name))
       (printable-name (lsys-name name))
       (replacements (loop for i in (lsys-variables name)
			   collect (list (string i)
					 (chars-to-string (apply-rules
							   rules
							   (list i) 1))))))
    (format t "The axiom for \"~A\" is: ~S~%" printable-name axiom)
    (format t "The replacement rules for \"~A\" are:~%" printable-name)
    (format t "~{    ~{~S~^ becomes ~}~^~%~}" replacements)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VISUALIZATION FUNCTIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Empty characters, i.e., characters which are not supposed to result in any drawing command
(defvar *empty-characters* (list #\a #\b #\c #\0 #\1))

;; An association list of characters and their interpretation by SENTENCE-TO-POINTS
(defparameter *dictionary* (list (cons #\a "do nothing") (cons #\b "do nothing") (cons #\c "do nothing")
				 (cons #\e "move forwards by :STEP-SIZE (do not draw)")
				 (cons #\f "draw forwards by :STEP-SIZE") (cons #\g "draw forwards by :STEP-SIZE")
				 (cons #\+ "turn left by :ANGLE")
				 (cons #\- "turn right by :ANGLE")
				 (cons #\[ "store current position and direction; turn ccw by :ANGLE")
				 (cons #\] "load position and direction; turn cw by :ANGLE")
				 (cons #\| "load position and direction non-destructively")
				 (cons #\0 "do nothing") (cons #\1 "do nothing")))

;; For a given character, gives its standard meaning according to *DICTIONARY*
(defun query-dictionary (character)
  (let ((entry (assoc character *dictionary*)))
    (if entry
	(format t "~c means ~S" (car entry) (cdr entry))
      (format t "~c is not in the dictionary" character))))

;; Prints the entire dictionary
(defun print-dictionary () (dolist (entry *dictionary*) (format t "~c means ~S~%" (car entry) (cdr entry))))

;; Given a SENTENCE (in the form of a list of characters), converts it into a list of points according
;; to the standard set of instructions as defined by the keywords which follow:
;;     INITIAL-POS is the starting location of the cursor.
;;     INITIAL-DIR is the starting pointing of the cursor. This should always be of unit length.
;;     ANGLE is the angle (in radians) by which the #\+ or #\- commands will rotate the cursor.
;;     STEP-SIZE is how far the cursor will move (arb. units)
;; Returns by default a list of the points, but setting :SAVE-AS to some string will instead save the points to
;; the file ":SAVE-AS.dat". Note that this will destructively overwrite any existing files by that name.
(defun sentence-to-points (sentence &key (save-as nil) (initial-pos (make-point 0 0)) (initial-dir (make-point 0 1))
				    (angle (/ pi 2)) (step-size 1.0))
  (let ((points (list initial-pos))
	(pos initial-pos)
	(dir initial-dir)
	(saved-data nil))
    (dolist (i sentence)
      (cond
       ;;meaning "draw forwards"
       ((or (equal i #\f) (equal i #\g)) (setf pos (point+ pos (point-scale dir step-size))) (push pos points))
       ;;meaning "move forwards (but do not draw)"
       ((equal i #\e) (setf pos (point+ pos (point-scale dir step-size))) (push nil points) (push pos points))
       ;;meaning "turn ccw by ANGLE"
       ((equal i #\+) (setf dir (point-rotate2 dir (- angle))))
       ;;meaning "turn cw by ANGLE"
       ((equal i #\-) (setf dir (point-rotate2 dir angle)))
       ;;meaning:
       ((equal i #\[)
	(push (list pos dir) saved-data) ;store the current position and direction
	(setf dir (point-rotate2 dir (- angle)))) ;turn ccw by ANGLE
       ;;meaning:
       ((equal i #\])
	(push nil points) ;make a new branch
	(multiple-value-setq (pos dir) (values-list (pop saved-data))) (push pos points) ;load the last saved position and direction
	(setf dir (point-rotate2 dir angle))) ;turn cw by ANGLE
       ;;meaning:
       ((equal i #\|)
	(push nil points) ;make a new branch
	(multiple-value-setq (pos dir) (values-list (nth 0 saved-data))) (push pos points)) ;non-destructively load last saved position and direction
       ;;meaning nothing, but still possibly present in a sentence
       ((member i *empty-characters*))
       ;;no defined meaning
       (t (format t "I don't recognize ~S as a valid command and have skipped it.~%" i))))
    (if save-as
	(with-open-file
	 (s (format nil "~A.dat" save-as) :direction :output :if-does-not-exist :create :if-exists :overwrite)
	 (format s "~{~{~$~^ ~}~%~}" points))
      points)))

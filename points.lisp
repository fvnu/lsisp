;; Defines a point of single-floats, with elements given as a list ELEMENTS
(defun make-point (&rest elements) (map 'list #'(lambda (x) (float x 1.0s0)) elements))

(defun point-line-distance (p a b c)
  (/ (abs (+ (* a (nth 0 p)) (* b (nth 1 p)) c)) (sqrt (+ (expt a 2) (expt b 2)))))

(defun point-line-closest (p a b c)
  (let ((x (nth 0 p))
	(y (nth 1 p))
	(d (+ (expt a 2) (expt b 2))))
    (make-point (/ (- (* b (- (* b x) (* a y))) (* a c)) d)
		(/ (- (* a (- (* a y) (* b x))) (* b c)) d))))

;; Defines a point sum operation
(defun point+ (p1 p2) (map 'list #'(lambda (e1 e2) (+ e1 e2)) p1 p2))

;; Defines a point scaling operation
(defun point-scale (p scale) (map 'list #'(lambda (e) (* scale e)) p))

;; Defines a point rotation operation, clockwise, in two dimensions
(defun point-rotate2 (p angle)
  (let ((x (nth 0 p)) (y (nth 1 p)))
    (make-point (+ (* x (cos angle)) (* y (sin angle))) (- (* y (cos angle)) (* x (sin angle))))))

;; Defines a shear operation relative to some line,
;; A x + B y + c = 0
;; which defaults to y=0, i.e., a shear relative to the x-axis.
;; The magnitude of the shear is SHEAR*(distance from the line)
;; The direction of the shear is that of the unit vector (B -A).
(defun point-shear (p shear &optional (a 0) (b 1) (c 0))
  (point+ p (point-scale (make-point b (- a))
			 (* (point-line-distance p a b c)
			    shear
			    (/ (sqrt (+ (expt a 2) (expt b 2))))))))

;; Defines a stretching operating relative to some line,
;; A x + B y + c = 0
;; which defaults to x=0, i.e., a stretching relative to the y-axis.
;; The magnitude of the stretch is STRETCH*(distance from the line)
(defun point-stretch (p stretch &optional (a 1) (b 0) (c 0))
  (point+ (point-scale p stretch)
	  (point-scale (point-line-closest p a b c) (- 1 stretch))))

;; Defines a reflection operation relative to some line,
;; A x + B y + c = 0
;; which defaults to x=0, i.e., a reflection across the y-axis
(defun point-reflect (p &optional (a 1) (b 0) (c 0))
  (point+ (point-scale (point-line-closest p a b c) 2) (point-scale p -1)))


;; Applies a function FUNCTION, with arguments FUNCTION-ARGUMENTS, to POINTS, where each element
;; of POINTS is a point as defined by MAKE-POINT
(defun transform-points (points function &rest function-arguments)
  (map 'list #'(lambda (x) (apply function x function-arguments)) points))

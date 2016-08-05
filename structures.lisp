(defpackage :origami/structures
  (:use :cl)
  (:export :make-point :point- :make-edge :fold-over-edge :edge-start
	   :point-x :point-y :edge-dy :edge-dx))

(in-package :origami/structures)

(defun make-point (x y)
  (list x y))

(defun point-x (p)
  (first p))

(defun point-y (p)
  (second p))

(defun point- (point1 point2)
  (destructuring-bind (x1 y1) point1
    (destructuring-bind (x2 y2) point2
      (make-point (- x1 x2) (- y1 y2)))))

(defun make-edge (point1 point2)
  (list point1 (point- point2 point1)))

(defun edge-start (edge)
  (first edge))

(defun edge-dx (edge)
  (point-x (second edge)))

(defun edge-dy (edge)
  (point-y (second edge)))

(defun make-polygon (&rest points)
  points)

(defun polygon-area (polygon)
  (abs (/ (reduce #'+
		  (maplist (lambda (points)
			     (destructuring-bind (x1 y1) (car points)
			       (destructuring-bind (x2 y2) (if (second points) (second points) (first polygon)) ;wrap-around
				 (- (* x1 y2) (* x2 y1)))))
			   polygon)
		  :initial-value 0)
	  2)))

(defun fold-over-edge (polygon edge)
  (destructuring-bind ((x0 y0) (dx dy)) edge
    (mapcar (lambda (point)
	      (destructuring-bind (x y) point
		(let* ((a (/ (- (* dx dx) (* dy dy))
			     (+ (* dx dx) (* dy dy))))
		       (b (/ (* 2 dx dy)
			     (+ (* dx dx) (* dy dy)))))
		  (make-point (+ x0 (* a (- x x0)) (* b (- y y0)))
			      (+ y0 (* b (- x x0)) (* a (- y0 y)))))))
	    polygon)))

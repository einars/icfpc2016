(defpackage :origami/structures
  (:use :cl)
  (:export :make-point :point- :point+ :make-edge :fold-over-edge :edge-start
	   :px :py :edge-dy :edge-dx :polygon-fragment
	   :edge-end :distance :align-polygon-to-edge
	   :dot-product))

(in-package :origami/structures)

(defun make-point (x y)
  (list x y))

(defun px (p)
  (first p))

(defun py (p)
  (second p))

(defun point- (point1 point2)
  (destructuring-bind (x1 y1) point1
    (destructuring-bind (x2 y2) point2
      (make-point (- x1 x2) (- y1 y2)))))

(defun point+ (point1 point2)
  (destructuring-bind (x1 y1) point1
    (destructuring-bind (x2 y2) point2
      (make-point (+ x1 x2) (+ y1 y2)))))

(defun make-edge (point1 point2)
  (list point1 point2 (point- point2 point1)))

(defun edge-start (edge)
  (first edge))

(defun edge-end (edge)
  (second edge))

(defun edge-dx (edge)
  (px (third edge)))

(defun edge-dy (edge)
  (py (third edge)))

(defun distance (p1 p2)
  (let ((diff (point- p2 p1)))
    (+ (* (px diff) (px diff))
       (* (py diff) (py diff)))))

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

(defun get-polygon-fragment (polygon n)
  (if (< n (1- (length polygon)))
      (let ((rest (nthcdr n polygon)))
	(point- (second rest) (first rest)))
      (point- (first polygon) (car (last polygon)))))

(defun fold-over-edge (polygon edge)
  (destructuring-bind ((x0 y0) (x1 y1) (dx dy)) edge
    (declare (ignore x1 y1))
    (mapcar (lambda (point)
	      (destructuring-bind (x y) point
		(let* ((a (/ (- (* dx dx) (* dy dy))
			     (+ (* dx dx) (* dy dy))))
		       (b (/ (* 2 dx dy)
			     (+ (* dx dx) (* dy dy)))))
		  (make-point (+ x0 (* a (- x x0)) (* b (- y y0)))
			      (+ y0 (* b (- x x0)) (* a (- y0 y)))))))
	    polygon)))

(defun align-polygon-to-edge (polygon unity-edge)
  (destructuring-bind ((x0 y0) (x1 y1) (dx2 dy2)) unity-edge
    (declare (ignore x1 y1))
    (destructuring-bind (dx1 dy1) (get-polygon-fragment polygon 0)
      (destructuring-bind (px0 py0) (first polygon)
	(let* ((cosa (+ (* dx1 dx2) (* dy1 dy2)))
	       (sina (- (* dx1 dy2) (* dx2 dy1)))
	       (px0rot (- (* px0 cosa) (* py0 sina)))
	       (py0rot (+ (* px0 sina) (* py0 cosa)))
	       (dx (+ x0 (- px0rot px0)))
	       (dy (+ y0 (- py0rot py0))))
	  (mapcar (lambda (point)
		    (destructuring-bind (x y) point
		      (make-point (- (* x cosa) (* y sina) dx)
				  (- (* x sina) (* y cosa -1) dy))))
		  polygon))))))

(defun dot-product (d1 d2)
  (destructuring-bind (x1 y1) d1
    (destructuring-bind (x2 y2) d2
      (+ (* x1 x2) (* y1 y2)))))

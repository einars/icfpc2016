(defpackage :origami/polygon-flipper
  (:use :cl :screamer :origami/sandman)
  (:import-from :origami/structures :make-point :point- :point+ :dot-product :polygon-area)
  (:export :vertex :make-vertex :vertex-point :vertex-adjacent-vertices
	   :edge :make-edge :edge-vertex1 :edge-vertex2
	   :graph :make-graph :build-graph :graph-vertices :graph-edges
	   :graph-add-vertex :graph-add-edge :graph-add-vertex-edge :graph-remove-edge :visualise-graph
	   :vertex- :vertex+ :vertex-on-edge-p :vertex-signum :vertex-polygon-area
	   :find-outer-path :vect-angle :vect-pseudoangle
	   :total-dot-product :check-square
	   :find-possible-edges))

(in-package :origami/polygon-flipper)

(defstruct vertex point adjacent-vertices)
(defstruct edge vertex1 vertex2 (special nil))
(defstruct graph vertices edges polygons)

(defun connect-all (graph edge)
  (let ((edge-vertices (mapcan (lambda (vertex)
				 (when (vertex-on-edge-p vertex edge)
				   (list vertex)))
			       (graph-vertices graph))))
    (loop as vertex1 in edge-vertices
       do (loop as vertex2 in edge-vertices
	     do (unless (eq vertex1 vertex2)
		  (when (loop as neighbor in (vertex-adjacent-vertices vertex1)
			   never (eq neighbor vertex2))
		    (graph-add-vertex-edge graph vertex1 vertex2 :special :guess)))))))

(defun unconnect-all (graph edge)
  (loop as edge1 in (graph-edges graph)
     do (with-slots (vertex1 vertex2 special) edge1
	  (when (and (eql special :guess)
		     (vertex-on-edge-p vertex1 edge)
		     (vertex-on-edge-p vertex2 edge))
	    (graph-remove-edge graph edge1)))))

(defun build-graph ()
  (let ((graph (make-graph)))
    (dolist (vertex-point *vertices*)
      (graph-add-vertex graph vertex-point))
    (dolist (edge *edges*)
      (destructuring-bind (point1 point2 diff) edge
	(declare (ignore diff))
	(graph-add-edge graph point1 point2)))
    (dolist (vertex (graph-vertices graph))
      (dolist (edge (graph-edges graph))
	(let ((vertex1 (edge-vertex1 edge))
	      (vertex2 (edge-vertex2 edge)))
	  (when (and (zerop (vertex-signum vertex edge))
		     (not (eq vertex vertex1))
		     (not (eq vertex vertex2))
		     (destructuring-bind (x1 y1) (vertex-point vertex1)
		       (destructuring-bind (x2 y2) (vertex-point vertex2)
			 (destructuring-bind (px py) (vertex-point vertex)
			   (and (< x1 px x2)
				(< y1 py y2))))))
	    (setf (edge-vertex2 edge) vertex)
	    (graph-add-edge graph (vertex-point vertex) (vertex-point vertex2))))))
    graph))

(defun visualise-graph (graph)
  (let ((outer-edge (find-outer-path graph)))
    (format t "~&1~%~A~%" (length outer-edge))
    (format t "~{~{~A~^,~}~%~}" (mapcar #'vertex-point outer-edge))
    (format t "~A~%" (length (graph-edges graph)))
    (format t "~{~{~{~A~^,~}~^ ~}~%~}" (mapcar (lambda (edge)
						 (with-slots (vertex1 vertex2) edge
						   (list (vertex-point vertex1)
							 (vertex-point vertex2))))
					       (graph-edges graph)))))

(defmethod print-object ((vertex vertex) stream)
  (print-unreadable-object (vertex stream :type t)
    (destructuring-bind (x y) (vertex-point vertex)
      (format stream "~A,~A" x y))))

(defmethod print-object ((edge edge) stream)
  (print-unreadable-object (edge stream :type t)
    (with-slots (vertex1 vertex2) edge
      (destructuring-bind (x1 y1) (vertex-point vertex1)
	(destructuring-bind (x2 y2) (vertex-point vertex2)
	  (format stream "~A,~A -> ~A,~A" x1 y1 x2 y2))))))

(defun graph-add-vertex (graph point)
  (push (make-vertex :point point) (graph-vertices graph)))

(defun graph-add-vertex-edge (graph vertex1 vertex2 &key special)
  (push vertex2 (vertex-adjacent-vertices vertex1))
  (push vertex1 (vertex-adjacent-vertices vertex2))
  (car (push (make-edge :vertex1 vertex1 :vertex2 vertex2 :special special) (graph-edges graph))))

(defun graph-add-edge (graph point1 point2)
  (let ((vertex1 (find point1 (graph-vertices graph) :key #'vertex-point :test #'equalp))
	(vertex2 (find point2 (graph-vertices graph) :key #'vertex-point :test #'equalp)))
    (assert (and vertex1 vertex2))
    (assert (not (eq vertex1 vertex2)))
    (push vertex2 (vertex-adjacent-vertices vertex1))
    (push vertex1 (vertex-adjacent-vertices vertex2))
    (car (push (make-edge :vertex1 vertex1 :vertex2 vertex2) (graph-edges graph)))))

(defun graph-remove-edge (graph edge)
  (with-slots (vertex1 vertex2) edge
    (setf (vertex-adjacent-vertices vertex1)
	  (delete vertex2 (vertex-adjacent-vertices vertex1))
	  (vertex-adjacent-vertices vertex2)
	  (delete vertex1 (vertex-adjacent-vertices vertex2))
	  (graph-edges graph)
	  (delete edge (graph-edges graph)))))

(defun extract-skeleton-polygons ()
  (let ((graph (make-graph)))
    (dolist (vertex-point *vertices*)
      (graph-add-vertex graph vertex-point))
    (dolist (edge *edges*)
      (destructuring-bind (point1 point2) edge
	(graph-add-edge graph point1 (point+ point1 point2))))
    (dolist (vertex (graph-vertices graph))
      (extract-vertex-polygons graph vertex))
    graph))

(defun extract-vertex-polygons (graph vertex)
  (dolist (next-vertex (vertex-adjacent-vertices vertex))
    (let ((path (find-left-path vertex next-vertex)))
      (format t "~A~%" (mapcar #'vertex-point path)))))

(defun vertex- (vertex1 vertex2)
  (point- (vertex-point vertex1) (vertex-point vertex2)))

(defun vertex+ (vertex1 vertex2)
  (point+ (vertex-point vertex1) (vertex-point vertex2)))

(defun vertex-on-edge-p (vertex edge)
  (with-slots (vertex1 vertex2) edge
    (destructuring-bind (dx dy) (vertex- vertex2 vertex1)
      (destructuring-bind (dpx dpy) (vertex- vertex vertex1)
	(cond
	  ((zerop dx) (zerop dpx))
	  ((zerop dy) (zerop dpy))
	  (t (eql (/ dpx dx)
		  (/ dpy dy))))))))

(defun vertex-signum (vertex edge)
  (with-slots (vertex1 vertex2) edge
    (destructuring-bind (dx dy) (vertex- vertex2 vertex1)
      (signum (dot-product (vertex- vertex vertex1) (make-point dy (* -1 dx)))))))

(defun vertex-polygon-area (vertex-polygon)
  (polygon-area (mapcar #'vertex-point vertex-polygon)))

(defun vect-angle (v1 v2)
  (destructuring-bind (dx1 dy1) v1
    (destructuring-bind (dx2 dy2) v2
      (let ((a (- (atan dy1 dx1) (atan dy2 dx2))))
	(if (< a 0)
	    (+ a (* 2 pi))
	    a)))))

(defun vect-pseudoangle (v1 v2)
  (- (pseudoangle1 v2) (pseudoangle1 v1)))

(defun pseudoangle1 (v)
  (destructuring-bind (dx dy) v
    (let ((pa (/ dx (+ (abs dx) (abs dy)))))
      (if (< dy 0)
	  (1- pa)
	  (- 1 pa)))))

(defun find-left-path (vertex1 vertex2)
  (find-left-path1 vertex1 vertex2 vertex1))

(defun find-left-path1 (vertex1 vertex2 start-vertex)
  (format t "Habala Babala~%")
  (format t "~A -> ~A (~A) ~%" vertex1 vertex2 start-vertex)
  (if (eq vertex2 start-vertex)
      (list vertex1)
      (let* ((prev-vector (vertex- vertex2 vertex1))
	     (next-vertex (reduce (lambda (v1 v2)
				    (if v1
					(cond
					  ((eq v1 vertex1) v2)
					  ((eq v2 vertex1) v1)
					  ((< (vect-angle prev-vector (vertex- v1 vertex1))
					      (vect-angle prev-vector (vertex- v2 vertex1)))
					   v1)
					  (t v2))
					(unless (eq v2 vertex1)
					  v2)))
				  (vertex-adjacent-vertices vertex2)
				  :initial-value nil)))
	(cons vertex1 (find-left-path1 vertex2 next-vertex start-vertex)))))

(defun check-square (graph)
  (let ((lowest-left (first (graph-vertices graph)))
	(rightest-low (first (graph-vertices graph))))
    (loop as vertex in (graph-vertices graph)
       do (destructuring-bind (lx1 ly1) (vertex-point lowest-left)
	    (destructuring-bind (rx1 ry1) (vertex-point rightest-low)
	      (destructuring-bind (x y) (vertex-point vertex)
		(if (eql ly1 y)
		    (when (> lx1 x)
		      (setf lowest-left vertex))
		    (when (> ly1 y)
		      (setf lowest-left vertex)))
		(if (eql rx1 x)
		    (when (> ry1 y)
		      (setf rightest-low vertex))
		    (when (< rx1 x)
		      (setf rightest-low vertex)))))))
    (loop with diagl = nil and diagr = nil
       as vertex in (graph-vertices graph)
       do (let* ((lvect (vertex- vertex lowest-left))
		 (rvect (vertex- vertex rightest-low))
		 (llen (dot-product lvect lvect))
		 (rlen (dot-product rvect rvect)))
	    (when (or (> llen 2)
		      (> rlen 2))
	      (return-from check-square (values nil nil)))
	    (when (eql 2 llen)
	      (setf diagl t))
	    (when (eql 2 rlen)
	      (setf diagr t)))
       finally (return (values t (and diagl diagr))))))

(defun find-outer-path (graph)
  (let ((lowest-vertex (reduce (lambda (v1 v2)
				  (destructuring-bind (x1 y1) (vertex-point v1)
				    (destructuring-bind (x2 y2) (vertex-point v2)
				      (if (eql y1 y2)
					  (if (> x1 x2) v2 v1)
					  (if (> y1 y2) v2 v1)))))
			       (graph-vertices graph))))
    (find-outer-path1 lowest-vertex '(0 -1) (list lowest-vertex) lowest-vertex)))

(defun find-outer-path1 (curr-vertex prev-direction result start-vertex)
  #+nil(cerror "Cont?" "~A ~A" curr-vertex prev-direction)
  (multiple-value-bind (next-vertex direction)
      (loop with leftmost-vertex and leftmost-direction
	 with min-angle = 10.0
	 as vertex in (vertex-adjacent-vertices curr-vertex)
	 do (let* ((direction (vertex- vertex curr-vertex))
		   (angle (vect-angle prev-direction direction)))
	      #+nil(format t "~A ~A ~A ~A~%" vertex curr-vertex direction angle)
	      (when (and (< angle min-angle)
			 (not (zerop angle)))
		(setf min-angle angle
		      leftmost-vertex vertex
		      leftmost-direction direction)))
	 finally (return (values leftmost-vertex (point- '(0 0) leftmost-direction))))
    (if (eq next-vertex start-vertex)
	result
	(find-outer-path1 next-vertex direction (cons next-vertex result) start-vertex))))

(defun total-dot-product (graph)
  (loop as vertex in (graph-vertices graph)
     summing (loop as vertex1 in (vertex-adjacent-vertices vertex)
		summing (loop as vertex2 in (vertex-adjacent-vertices vertex)
			     summing (dot-product (vertex- vertex2 vertex) (vertex- vertex1 vertex))))))

(defun find-possible-edges (graph)
  (let ((possible-edges nil))
    (loop as edge in (graph-edges graph)
       do (let ((dangling-vertices nil))
	    (loop as vertex in (graph-vertices graph)
	       do (with-slots (vertex1 vertex2) edge
		    (unless (or (eq vertex vertex1)
				(eq vertex vertex2))
		      (when (and (vertex-on-edge-p vertex edge)
				 (destructuring-bind (x y) (vertex-point vertex)
				   (destructuring-bind (x1 y1) (vertex-point vertex1)
				     (destructuring-bind (x2 y2) (vertex-point vertex2)
				       (and (<= (* (- x x1) (- x x2)) 0)
					    (<= (* (- y y1) (- y y2)) 0))))))
			(push vertex dangling-vertices)
			(push (list vertex1 vertex) possible-edges)
			(push (list vertex vertex2) possible-edges)))))
	    (loop as vertex-tail on dangling-vertices
	       do (let ((vertex1 (first vertex-tail)))
		    (loop as vertex2 in (rest vertex-tail)
		       do (unless (eq vertex1 vertex2)
			    (push (list vertex1 vertex2) possible-edges)))))))
    possible-edges))

(defun pipe-start ()
  (ignore-errors
    (origami/sandman::start :call-cers t))
  (sb-ext:exit))


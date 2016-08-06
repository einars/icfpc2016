(defpackage :origami/polygon-flipper
  (:use :cl :screamer :origami/sandman)
  (:import-from :origami/structures :point- :point+)
  (:export :make-vertex :vertex-point :vertex-adjacent-vertices
	   :make-edge :make-vertex1 :make-vertex2
	   :make-graph :graph-vertices :graph-edges
	   :graph-add-vertex :graph-add-edge
	   :vertex- :vertex+))

(in-package :origami/polygon-flipper)

(defstruct vertex point adjacent-vertices)
(defstruct edge vertex1 vertex2)
(defstruct graph vertices edges polygons)

(defun graph-add-vertex (graph point)
  (push (make-vertex :point point) (graph-vertices graph)))

(defun graph-add-edge (graph point1 point2)
  (let ((vertex1 (find point1 (graph-vertices graph) :key #'vertex-point :test #'equalp))
	(vertex2 (find point2 (graph-vertices graph) :key #'vertex-point :test #'equalp)))
    (assert (and vertex1 vertex2))
    (assert (not (eq vertex1 vertex2)))
    (push vertex2 (vertex-adjacent-vertices vertex1))
    (push vertex1 (vertex-adjacent-vertices vertex2))
    (push (make-edge :vertex1 vertex1 :vertex2 vertex2) (graph-edges graph))))

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

(defun vect-angle (v1 v2)
  (destructuring-bind (dx1 dy1) v1
    (destructuring-bind (dx2 dy2) v2
      (- (atan dy1 dx1) (atan dy2 dx2)))))

(defun find-left-path (vertex1 vertex2)
  (find-left-path1 vertex1 vertex2 vertex1))

(defun find-left-path1 (vertex1 vertex2 start-vertex)
  (format t "~A -> ~A (~A) ~%" (vertex-point vertex1) (vertex-point vertex2) (vertex-point start-vertex))
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

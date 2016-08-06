(screamer:define-screamer-package :origami/point-and-edge-show
  (:use :cl :screamer :origami/sandman :origami/polygon-flipper)
  (:import-from :origami/structures :make-point :point- :point+ :dot-product))

(in-package :origami/point-and-edge-show)

(defun build-graph ()
  (let ((graph (make-graph)))
    (dolist (vertex-point *vertices*)
      (graph-add-vertex graph vertex-point))
    (dolist (edge *edges*)
      (destructuring-bind (point1 point2) edge
	(graph-add-edge graph point1 (point+ point1 point2))))
    graph))

(defun any-element (list)
  (if list
      (either (first list) (any-element (rest list)))
      (fail)))

(defun find-fold (graph)
  (all-values
    (let ((edge (any-element (graph-edges graph)))
	  (vertex (any-element (graph-vertices graph))))
      (assert! (not (vertex-on-edge-p vertex edge)))
      (cons edge vertex))))

(defun find-same-side-subgraph (vertex edge)
  (let ((*subgraph* (list (list vertex)))
	(vsig (vertex-signum vertex edge)))
    (declare (special *subgraph*))
    (for-effects
      (find-same-side-subgraph1 vertex edge vsig))
    (values
     (car *subgraph*)
     (loop as vertex in (car *subgraph*)
	always (loop as neighbor in (vertex-adjacent-vertices vertex)
		  always (let ((nsig (vertex-signum neighbor edge)))
			   (or (zerop nsig)
			       (eql nsig vsig))))))))

(defun find-same-side-subgraph1 (vertex edge vsig)
  (declare (special *subgraph*))
  (let ((next-vertex (any-element (vertex-adjacent-vertices vertex))))
    (if (eql vsig (vertex-signum next-vertex edge))
	(let ((next-subgraph (adjoin next-vertex (car *subgraph*))))
	  (if (eq next-subgraph (car *subgraph*))
	      (fail)
	      (progn
		(setf (car *subgraph*) next-subgraph)
		(find-same-side-subgraph1 next-vertex edge vsig))))
	(fail))))

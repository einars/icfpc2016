(screamer:define-screamer-package :origami/point-and-edge-show
  (:use :cl :screamer :origami/sandman :origami/polygon-flipper)
  (:import-from :origami/structures :make-point :point- :point+ :dot-product :fold-over-edge))

(in-package :origami/point-and-edge-show)

(defun any-element (list)
  (if list
      (either (first list) (any-element (rest list)))
      (fail)))

(defun archa-solve ()
  (multiple-value-bind (new-graph solution) (one-value (unfold (build-graph)))
    (declare (ignore new-graph))
    solution))

(defun unfold (graph)
  (let ((*trace*)
	(*area* 0)
	(*dot-product* (total-dot-product graph)))
    (declare (special *trace* *area* *dot-product*))
    (local (unfold1 graph))))

(defun unfold1 (graph)
  (declare (special *trace* *area* *dot-product*))
  (format t "DP: ~F~%" (total-dot-product graph))
  (multiple-value-bind (edge subgraph) (find-fold graph)
    (unfold2 edge subgraph))
  (let ((new-area (vertex-polygon-area (find-outer-path graph)))
	(new-dot-product (total-dot-product graph)))
    (format t "Is square: ~A~%" (check-square graph))
    (cond
      ((=  new-area 1) (values graph (reverse *trace*)))
      ((< new-dot-product *dot-product*)
       (setf *area* new-area)
       (setf *dot-product* new-dot-product)
       (unfold1 graph))
      (t (fail)))))

(defun unfold2 (edge subgraph)
  (declare (special *trace*))
  (let* ((polygon (mapcar #'vertex-point subgraph))
	 (folded-polygon (fold-over-edge polygon
					 (origami/structures:make-edge (vertex-point (edge-vertex1 edge))
								       (vertex-point (edge-vertex2 edge))))))
    (push (list (vertex-point (edge-vertex1 edge)) (vertex-point (edge-vertex2 edge)) polygon) *trace*)
    #+nil(cerror "ok?" "Moving ~A to ~A" polygon folded-polygon)
    (loop as vertex in subgraph
       as new-point in folded-polygon
       do (setf (vertex-point vertex) new-point))
    (trail (lambda ()
	     #+nil(cerror "ok?" "Returning ~A to ~A" folded-polygon polygon)
	     (pop *trace*)
	     (loop as vertex in subgraph
		as point in polygon
		do (setf (vertex-point vertex) point))))))

(defun find-fold (graph)
  (let ((max-onside (- (length (graph-vertices graph)) 2)))
    (let ((edge (any-element (graph-edges graph)))
	  (vertex (any-element (graph-vertices graph))))
      (assert! (not (vertex-on-edge-p vertex edge)))
      (multiple-value-bind (subgraph complete) (find-same-side-subgraph vertex edge)
	(if (and complete
		 (< (length subgraph) max-onside))
	    (values edge subgraph)
	    (fail))))))

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
    (let ((nsig (vertex-signum next-vertex edge)))
      (if (eql vsig nsig)
	  (let ((next-subgraph (adjoin next-vertex (car *subgraph*))))
	    (if (eq next-subgraph (car *subgraph*))
		(fail)
		(progn
		  (setf (car *subgraph*) next-subgraph)
		  (find-same-side-subgraph1 next-vertex edge vsig))))
	  (fail)))))

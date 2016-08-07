(screamer:define-screamer-package :origami/point-and-edge-show
  (:use :cl :screamer :origami/sandman :origami/polygon-flipper)
  (:import-from :origami/structures :make-point :point- :point+ :dot-product :fold-over-edge))

(in-package :origami/point-and-edge-show)

(defun any-element (list)
  (if list
      (either (first list) (any-element (rest list)))
      (fail)))

(defun archa-solve ()
  (one-value (unfold (build-graph))))

(defun unfold (graph &key (visualise nil))
  (let ((*trace*)
	(*area* 0)
	(*dot-product* (total-dot-product graph)))
    (declare (special *trace* *area* *dot-product*))
    (local
      (unfold1 graph visualise))))

(defparameter *min-min-prod* -10000.0)

(defun unfold1 (graph visualise)
  (declare (special *trace* *area* *dot-product*))
  #+nil(format t "~%======~%")
  #+nil(visualise-graph graph)
  (multiple-value-bind (edge subgraph) (find-fold graph)
    (unfold2 edge subgraph graph))
  (let ((new-dot-product (total-dot-product graph)))
    (cond
      ((check-square graph)
       (when visualise
	 (format t "~%=======~%")
	 (visualise-graph graph))
       (reverse *trace*))
      ((< new-dot-product *dot-product*)
       (let ((old-product *dot-product*))
	 (when (< new-dot-product *min-min-prod*)
	   (setf *min-min-prod* new-dot-product)
	   (format t "NEW DP: ~F~%" new-dot-product)
	   (when visualise
	     (visualise-graph graph)))
	 (setf *dot-product* new-dot-product)
	 (trail (lambda () (setf *dot-product* old-product)))
	 (unfold1 graph visualise)))
      (t (fail)))))

(defun unfold2 (edge subgraph graph)
  (declare (special *trace*))
  (let* ((polygon (mapcar #'vertex-point subgraph))
	 (folded-polygon (fold-over-edge polygon
					 (origami/structures:make-edge (vertex-point (edge-vertex1 edge))
								       (vertex-point (edge-vertex2 edge))))))
    (push (list (vertex-point (edge-vertex1 edge)) (vertex-point (edge-vertex2 edge)) polygon) *trace*)
    #+nil(cerror "ok?" "Moving ~A to ~A" polygon folded-polygon)
    #+nil(origami/polygon-flipper::connect-all graph edge)
    (loop as vertex in subgraph
       as new-point in folded-polygon
       do (setf (vertex-point vertex) new-point))
    (trail (lambda ()
	     #+nil(cerror "ok?" "Returning ~A to ~A" folded-polygon polygon)
	     (pop *trace*)
	     #+nil(origami/polygon-flipper::unconnect-all graph edge)
	     (loop as vertex in subgraph
		as point in polygon
		do (setf (vertex-point vertex) point))))))

(defun find-fold (graph)
  (let ((graph-size (length (graph-vertices graph))))
    (let ((edge (any-element (graph-edges graph)))
	  (vertex (any-element (graph-vertices graph))))
      (assert! (not (vertex-on-edge-p vertex edge)))
      (let ((vertices-on-edge (count-if (lambda (vertex1) (vertex-on-edge-p vertex1 edge)) (graph-vertices graph))))
	(multiple-value-bind (subgraph complete) (find-same-side-subgraph vertex edge)
	  (if (and complete
		   (< (length subgraph) (- graph-size vertices-on-edge)))
	      (values edge subgraph)
	      (fail)))))))

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

(screamer:define-screamer-package :origami/point-and-edge-show
  (:use :cl :screamer :origami/sandman :origami/polygon-flipper)
  (:import-from :origami/structures :make-point :point- :point+ :dot-product :fold-over-edge))

(in-package :origami/point-and-edge-show)

(defun any-element (list)
  (if list
      (either (first list) (any-element (rest list)))
      (fail)))

(defun any-combination (list)
  (if list
      (either (any-combination (cdr list))
	      (cons (car list) (any-combination (cdr list))))
      nil))

(defun archa-solve ()
  (one-value (unfold-valid (build-graph))))

(defun unfold-valid (graph &key visualise)
  (let ((valid-graph (next-valid graph)))
    (when visualise
      (format t "Processing graph - ~A edges~%" (length (graph-edges valid-graph))))
    (unfold valid-graph :visualise visualise)))

(defun next-valid (graph)
  (let ((next-graph (gen-next-graph graph)))
    (if (valid-graph-p next-graph)
	next-graph
	(fail))))

(defun gen-next-graph (graph)
  (let ((*added-edges* nil)
	(*removed-edges* nil))
    (declare (special *added-edges* *removed-edges*))
    (multiple-value-bind (possible-edges possible-removals) (find-possible-edges graph)
      (let ((new-edges (any-combination possible-edges))
	    (removals (any-combination possible-removals)))
	(when (or new-edges removals)
	  (let ((added-edges nil)
		(removed-edges (copy-list removals)))
	    (dolist (pseudo-edge new-edges)
	      (destructuring-bind (vertex1 vertex2) pseudo-edge
		(push (graph-add-vertex-edge graph vertex1 vertex2) added-edges)))
	    (dolist (edge removals)
	      (graph-remove-edge graph edge))
	    (setf *added-edges* (mapcar (lambda (edge)
					  (list (vertex-point (edge-vertex1 edge))
						(vertex-point (edge-vertex2 edge))))
					added-edges)
		  *removed-edges* (mapcar (lambda (edge)
					    (list (vertex-point (edge-vertex1 edge))
						  (vertex-point (edge-vertex2 edge))))
					  removed-edges))
	    (trail (lambda ()
		     (dolist (edge added-edges)
		       (graph-remove-edge graph edge))
		     (dolist (edge removed-edges)
		       (graph-add-vertex-edge graph (edge-vertex1 edge) (edge-vertex2 edge)))))))
	graph))))

(defun valid-graph-p (graph)
  (let ((duplet-count (loop as vertex in (graph-vertices graph)
			 counting (let ((edge-count (length (vertex-adjacent-vertices vertex))))
				    (when (< edge-count 2)
				      (return-from valid-graph-p nil))
				    (eql edge-count 2)))))
    (<= duplet-count 4)))


(defun unfold (graph &key (visualise nil))
  (let ((*trace*)
	(*area* 0)
	(*dot-product* (total-dot-product graph)))
    (declare (special *trace* *area* *dot-product*))
    (local
      (unfold1 graph visualise))))

(defparameter *min-min-prod* -10000.0)

(defun unfold1 (graph visualise)
  (declare (special *trace* *area* *dot-product* *added-edges* *removed-edges*))
  #+nil(format t "~%======~%")
  #+nil(visualise-graph graph)
  (multiple-value-bind (edge subgraph) (find-fold graph)
    (unfold2 edge subgraph graph))
  (let ((new-dot-product (total-dot-product graph)))
    (multiple-value-bind (valid square) (check-square graph)
      (cond
	((not valid) (fail))
	(square
	 (when visualise
	   (format t "~%=======~%")
	   (visualise-graph graph))
	 (values (reverse *trace*)
		 (mapcar (lambda (edge)
			   (list (vertex-point (edge-vertex1 edge))
				 (vertex-point (edge-vertex2 edge))))
			 *added-edges*)
		 (mapcar (lambda (edge)
			   (list (vertex-point (edge-vertex1 edge))
				 (vertex-point (edge-vertex2 edge))))
			 *removed-edges*)))
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
	(t (fail))))))

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
      (multiple-value-bind (subgraph complete) (find-same-side-subgraph vertex edge)
	(if (and complete (< (length subgraph) graph-size))
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
	always (or (zerop (vertex-signum vertex edge))
		   (loop as neighbor in (vertex-adjacent-vertices vertex)
		      always (let ((nsig (vertex-signum neighbor edge)))
			       (or (zerop nsig)
				   (eql nsig vsig)))))))))

(defun find-same-side-subgraph1 (vertex edge vsig)
  (declare (special *subgraph*))
  (let ((next-vertex (any-element (vertex-adjacent-vertices vertex))))
    (let ((nsig (vertex-signum next-vertex edge)))
      (if (or (zerop nsig)
	      (eql vsig nsig))
	  (let ((next-subgraph (adjoin next-vertex (car *subgraph*))))
	    (if (eq next-subgraph (car *subgraph*))
		(fail)
		(progn
		  (setf (car *subgraph*) next-subgraph)
		  (unless (zerop nsig)
		    (find-same-side-subgraph1 next-vertex edge vsig)))))
	  (fail)))))

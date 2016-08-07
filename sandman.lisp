(defpackage :origami/sandman
  (:use :cl :origami/structures)
  (:export :read-input
	   :translate-pos
	   :*vertices*
	   :*edges*
	   :*outer-silhouette*
	   :*cers-solutions*
	   :archa-solve))

(in-package :origami/sandman)

(defvar *vertices* nil)
(defvar *edges* nil)
(defvar *outer-silhouette* nil)
(defvar *silhouette-vertices* nil)
(defvar *cers-solutions* nil)
(defvar *cers-vertices* nil)
(defvar *cers-edges* nil)
(defvar *debug* nil)

(defvar *solved-vertices* nil)
(defvar *solved-edges* nil)

(defparameter *max-steps* 100)

(defun bail-out (msg)
  (format t "ERROR: ~A~%" msg)
  (sb-ext:exit))

(defun flatten (lists)
  (when lists (nconc (first lists) (flatten (rest lists)))))

(defun read-vertex ()
  (let ((x) (y))
    (setf x (read))
    (read-char)
    (setf y (read))
    (make-point x y)))

(defun update-vertices (x)
  (unless (member x *vertices* :test #'equal)
    (push x *vertices*)))

(defun read-edge ()
  (let ((a (read-vertex))
	(b (read-vertex)))
    (update-vertices a)
    (update-vertices b)
    (make-edge a b)))

(defun read-input ()
  (setf *vertices* nil
	*edges* nil
	*silhouette-vertices* nil
	*outer-silhouette* nil)
  (let ((polygon-count (read)))
    (dotimes (i polygon-count)
      (let ((vertex-count (read)))
	(dotimes (j vertex-count)
	  (let ((vertice (read-vertex)))
	    (when (zerop i)
	      (push vertice *outer-silhouette*))
	    (push vertice *silhouette-vertices*))))))
  (let ((edge-count (read)))
    (dotimes (i edge-count)
      (push (read-edge) *edges*))))

(defun print-positions (vertices &key source)
  (when source (format t "~A~%" (length vertices)))
  (dolist (i vertices)
    (format t "~A,~A~%" (first i) (second i))))

(defun print-facets (facets)
  (format t "~A~%" (length facets))
  (dolist (i facets)
    (format t "~A ~{~A ~}~%" (length i) i)))

(defun rotate-pos (pos alpha)
  (make-point (+ (* (edge-dx alpha) (px pos)) (* (edge-dy alpha) (py pos)))
	      (- (* (edge-dx alpha) (py pos)) (* (edge-dy alpha) (px pos)))))

(defun translate-pos (pos bottom)
  (rotate-pos (point- pos (edge-start bottom)) bottom))

(defun good-one (pos)
  (and (<= 0 (px pos) 1) (<= 0 (py pos) 1)))

(defun is-underneath (pos start)
  (or (< (py pos) (py start))
      (and (= (py pos) (py start))
	   (< (px pos) (px start)))))

(defun is-to-right (pos start end)
  (and (> (px pos) (px start))
       (>= (py pos) (py start))
       (= 1 (distance pos start))
       (or (null end) (< (py pos) (py end)))))

(defun pick-bottom (candidates)
  (let ((start (first candidates)) (end nil))
    (dolist (pos candidates)
      (when (is-underneath pos start)
	(setf start pos)))
    (dolist (pos candidates)
      (when (is-to-right pos start end)
	(setf end pos)))
    (make-edge start end)))

(defun find-bottom (pos)
  ;; FIXME (this is not correct)
  (let ((edges nil) (diags nil))
    (dolist (grain *solved-vertices*)
      (let ((len (distance pos (first grain))))
	(when (= 1 len) (push (first grain) edges))
	(when (= 2 len) (push (first grain) diags))))
    (when (and (>= (length edges) 2) (>= (length diags) 1))
      (pick-bottom (cons pos (nconc edges diags))))))

(defun is-vertex-edge (vertex edge)
  (or (equal vertex (edge-start edge))
      (equal vertex (edge-end edge))))

(defun other-end (vertex edge)
  (if (equal vertex (edge-end edge))
      (edge-start edge)
      (edge-end edge)))

(defun index-of (vertex)
  (position vertex *vertices* :test #'equal))

(defun find-edges (v)
  (remove-if-not (lambda (e) (is-vertex-edge v e)) *edges*))

(defun normalize (edge)
  (let* ((x (px (third edge)))
	 (y (py (third edge)))
	 (len (sqrt (+ (* x x) (* y y)))))
    (make-point (/ x len) (/ y len))))

(defun edge-angle (v edgeA edgeB)
  (let* ((p1 (normalize (make-edge v (other-end v edgeA))))
	 (p2 (normalize (make-edge v (other-end v edgeB))))
	 (angle (- (atan (py p1) (px p1)) (atan (py p2) (px p2)))))
    (if (< angle 0)
	(+ angle (* 2 pi))
	angle)))

(defun produce-angles (vertex prev-edge candidates)
  (mapcar (lambda (edge) (list (edge-angle vertex prev-edge edge) edge))
	  candidates))

(defun look-for-edge (vertex prev-edge)
  (let* ((candidates (remove prev-edge (find-edges vertex) :test #'equal))
	 (angles (produce-angles vertex prev-edge candidates)))
    (when (null candidates) (bail-out "edge candidates absent"))
    (second (first (sort angles #'< :key #'first)))))

(defun find-next-edge (vertex prev-edge facet)
  (let ((index (index-of vertex)))
    (or (and (member index facet) facet)
	(let ((edge (look-for-edge vertex prev-edge)))
	  (find-next-edge (other-end vertex edge) edge (cons index facet))))))

(defun find-single-facet (vertex edge)
  (find-next-edge (other-end vertex edge) edge (list (index-of vertex))))

(defun find-vertex-facets (v)
  (mapcar (lambda (e) (find-single-facet v e)) (find-edges v)))

(defun find-all-facets ()
  (mapcar #'find-vertex-facets *vertices*))

(defun normalize-facet (f)
  (sort (copy-list f) #'<))

(defun equal-facets (a b)
  (equal (normalize-facet a) (normalize-facet b)))

(defun find-facets ()
  (let ((*edges* (mapcar #'first *solved-edges*))
	(*vertices* (mapcar #'first *solved-vertices*)))
    (remove-duplicates (flatten (find-all-facets)) :test #'equal-facets)))

(defun fetch-positions (index)
  (first (nth index *solved-vertices*)))

(defun is-outer-vertex (vertex)
  (or (= 0 (px vertex))
      (= 0 (py vertex))
      (= 1 (px vertex))
      (= 1 (py vertex))))

(defun is-outer-facet (facet)
  (let ((vertices (mapcar #'fetch-positions facet)))
    (and (member (make-point 0 0) vertices :test #'equal)
	 (member (make-point 0 1) vertices :test #'equal)
	 (member (make-point 1 0) vertices :test #'equal)
	 (member (make-point 1 1) vertices :test #'equal)
	 (not (member-if-not #'is-outer-vertex vertices)))))

(defun remove-outer-facet (pos-map facets)
  (let ((*solved-vertices* pos-map))
    (or (and (= 1 (length facets)) facets)
	(remove-if #'is-outer-facet facets))))

(defun match-vertice (vertice)
  (lambda (x) (equal (first x) vertice)))

(defun match-edge (vertice)
  (lambda (x) (is-vertex-edge vertice (first x))))

(defun find-solved-vertice (vertice)
  (first (member-if (match-vertice vertice) *solved-vertices*)))

(defun find-solved-edges-for-vertice (v)
  (remove-if-not (match-edge v) *solved-edges*))

(defun clone-edge (old new edge-entry)
  (make-edge new (other-end old (first edge-entry))))

(defun replace-edge-endpoint (old new edges)
  (mapcar (lambda (x) (cons (clone-edge old new x) x)) edges))

(defun clone-vertex/edge (old new)
  (let ((vertice (find-solved-vertice old))
	(edges (find-solved-edges-for-vertice old)))
    (push (cons new vertice) *solved-vertices*)
    (dolist (i (replace-edge-endpoint old new edges))
      (push i *solved-edges*))))

(defun remove-vertex/edge (v)
  (let ((upd-v (delete-if (match-vertice v) *solved-vertices*))
	(upd-e (delete-if (match-edge v) *solved-edges*)))
    (setf *solved-vertices* upd-v)
    (setf *solved-edges* upd-e)))

(defun fold-vertex-over-edge (vertex edge)
  (let ((new-vertex (first (fold-over-edge (list vertex) edge))))
    (when (not (equal vertex new-vertex))
      (clone-vertex/edge vertex new-vertex)
      (remove-vertex/edge vertex)
      (find-bottom new-vertex))))

(defun find-solved-edges (v)
  (remove-if-not (lambda (e) (is-vertex-edge v (first e))) *solved-edges*))

(defun find-adjacent (vertex solved-edge)
  (find-solved-edges (other-end vertex (first solved-edge))))

(defun test-corner-vertex (vertex)
  (let ((edges (find-solved-edges vertex)))
    (when (= 2 (length edges))
      (first (first (intersection
		     (find-adjacent vertex (first edges))
		     (find-adjacent vertex (second edges))
		     :test #'equal))))))

(defun find-corner-vertex (vertices)
  (unless (null vertices)
    (let* ((vertex (first (first vertices)))
	   (edge (test-corner-vertex vertex)))
      (cond ((or (null edge) (> (length (first vertices)) 1))
	     (find-corner-vertex (rest vertices)))
	    (t (fold-vertex-over-edge vertex edge))))))

(defun solve-cers ()
  (let ((last-solve nil))
    (dolist (solution *cers-solutions* last-solve)
      (let ((edge (make-edge (first solution) (second solution))))
	(dolist (vertex (third solution))
	  (let ((result (fold-vertex-over-edge vertex edge)))
	    (when result (setf last-solve result))))))))

(defun add-edge (vertex1 vertex2)
  (push (list (make-edge vertex1 vertex2)) *solved-edges*))

(defun solve-cers-or-dump ()
  (dolist (i *cers-edges*)
    (add-edge (first i) (second i)))
  (let ((result (solve-cers)))
    (when (or (null result) *debug*)
      (format t "~%SOLUTION: ~A~%~%" result)
      (format t "=== VERTICES ===~%~{~A~%~}~%~%" *solved-vertices*)
      (format t "=== EDGES ===~%~{~A~%~}~%~%" *solved-edges*))
    result))

(defun fold-some-vertex-over-some-edge ()
  (find-corner-vertex *solved-vertices*))

(defun generate-sand-cloud (&optional (steps 0))
  (if *cers-solutions*
      (solve-cers-or-dump)
      (or (and (> steps *max-steps*) (bail-out "max steps reached"))
	  (fold-some-vertex-over-some-edge)
	  (generate-sand-cloud (1+ steps)))))

(defun pre-generate ()
  (or (find-bottom (first *vertices*))
      (generate-sand-cloud)))

(defun get-original (pos-map)
  (mapcar (lambda (x) (first (last (second x)))) pos-map))

(defun translate-positions (bottom)
  (let ((good-positions nil))
    (dolist (dst (copy-list *solved-vertices*) (nreverse good-positions))
      (let ((src (translate-pos (first dst) bottom)))
	(if (good-one src)
	    (push (list src dst) good-positions)
	    (remove-vertex/edge (first dst)))))))

(defun search-for-mapping (vertex)
  (lambda (x) (equal vertex (first (second x)))))

(defun translate-and-format-vertex (pos-map vertex seperator)
  (let* ((translated (member-if (search-for-mapping vertex) pos-map))
	 (new-vertex (first (first translated))))
    (format t "~A,~A" (px new-vertex) (py new-vertex))
    (format t seperator)))

(defun dump-solution-as-problem (pos-map)
  (format t "=== SOLUTION-AS-PROBLEM ===~%")
  (format t "1~%4~%0,0~%1,0~%1,1~%0,1~%")
  (format t "~A~%" (length *solved-edges*))
  (dolist (edge *solved-edges*)
    (translate-and-format-vertex pos-map (edge-start (first edge)) " ")
    (translate-and-format-vertex pos-map (edge-end (first edge)) "~%")))

(defun is-lonely-vertice (edges)
  (and (= 2 (length edges))
       (let ((edge1 (first (first edges)))
	     (edge2 (first (second edges))))
	 (or (= 0 (edge-dy edge1) (edge-dy edge2))
	     (and (not (= 0 (edge-dy edge1)))
		  (not (= 0 (edge-dy edge2)))
		  (= (/ (edge-dx edge1) (edge-dy edge1))
		     (/ (edge-dx edge2) (edge-dy edge2))))))))

(defun index-of-solved (v)
  (position v *solved-vertices* :test (lambda (x y) (equal x (first y)))))

(defun construct-edges (vertex facets edges)
  (let* ((neighbors (mapcar (lambda (x) (other-end vertex (first x))) edges))
	 (n-indices (mapcar #'index-of-solved neighbors))
	 (index (index-of-solved vertex)))
    (dolist (facet facets)
      (when (member index facet)
	(dolist (i facet)
	  (when (and (not (member i n-indices)) (not (= i index)))
	    (add-edge vertex (first (elt *solved-vertices* i)))))))))

(defun find-lonely-vertices (facets)
  (dolist (record *solved-vertices*)
    (let* ((vertex (first record))
	   (edges (find-solved-edges vertex)))
      (when (is-lonely-vertice edges)
	(construct-edges vertex facets edges)))))

(defun get-facets (pos-map)
  (remove-outer-facet pos-map (find-facets)))

(defun print-output ()
  (let* ((*solved-vertices* (mapcar #'list *vertices*))
	 (*solved-edges* (mapcar #'list *edges*))
	 (pos-map (translate-positions (pre-generate))))
    (print-positions (mapcar #'first pos-map) :source t)
    (let ((facets (get-facets pos-map)))
      (find-lonely-vertices facets)
      (setf facets (get-facets pos-map))
      (print-facets facets))
    (print-positions (get-original pos-map))
    (when *debug* (dump-solution-as-problem pos-map))))

(defun start (&key call-cers debug)
  (let ((*cers-solutions* nil)
	(*cers-vertices* nil)
	(*cers-edges* nil)
	(*debug* nil)
	(*vertices* nil)
	(*edges* nil))
    (read-input)
    (setf *debug* debug)
    (when call-cers
      (multiple-value-bind (solutions edges)
	  (archa-solve)
	(setf *cers-edges* edges)
	(setf *cers-solutions* solutions)
	(when *debug* (format t "CERS: ~A~%" *cers-solutions*))))
    (print-output)
    (unless call-cers
      (sb-ext:exit))))

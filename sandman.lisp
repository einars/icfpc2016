(defpackage :origami/sandman
  (:use :cl :origami/structures)
  (:export :read-input
	   :translate-pos
	   :*vertices*
	   :*edges*
	   :*outer-silhouette*))

(in-package :origami/sandman)

(defvar *vertices* nil)
(defvar *edges* nil)
(defvar *outer-silhouette* nil)
(defvar *skeletal-vertices* nil)

(defvar *solved-vertices* nil)
(defvar *solved-edges* nil)

(defparameter *max-steps* 100)

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
    (push x *skeletal-vertices*)
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
	*skeletal-vertices* nil
	*outer-silhouette* nil)
  (let ((polygon-count (read)))
    (dotimes (i polygon-count)
      (let ((vertex-count (read)))
	(dotimes (j vertex-count)
	  (let ((vertice (read-vertex)))
	    (when (zerop i)
	      (push vertice *outer-silhouette*))
	    (push vertice *vertices*))))))
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

(defun translate-positions (bottom)
  (let ((good-positions nil))
    (dolist (dst *vertices* (nreverse good-positions))
      (let ((src (translate-pos dst bottom)))
	(when (good-one src) (push (list src dst) good-positions))))))

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
  (let* ((p1 (normalize (make-edge (other-end v edgeA) v)))
	 (p2 (normalize (make-edge v (other-end v edgeB)))))
    (- (* (px p1) (py p2)) (* (py p1) (px p2)))))

(defun produce-angles (vertex prev-edge candidates)
  (mapcar (lambda (edge) (list (edge-angle vertex prev-edge edge) edge))
	  candidates))

(defun look-for-edge (vertex prev-edge)
  (let* ((candidates (remove prev-edge (find-edges vertex) :test #'equal))
	 (angles (produce-angles vertex prev-edge candidates)))
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
	 (member-if-not #'is-outer-facet vertices))))

(defun remove-outer-facet (facets)
  (or (and (= 1 (length facets)) facets)
      (remove-if #'is-outer-facet facets)))

(defun bail-out ()
  (format t "Could not find solution after ~A steps~%" *max-steps*)
  (sb-ext:exit))

(defun add-sand ()
  (seventh *edges*))

(defun generate-sand-cloud (&optional (steps 0))
  (let ((grain (add-sand)))
    (cond (grain grain)
	  ((< steps *max-steps*)
	   (generate-sand-cloud (1+ steps)))
	  (t (bail-out)))))

(defun pre-generate ()
  (or (find-bottom (first *vertices*))
      (generate-sand-cloud)))

(defun print-output ()
  (let* ((*solved-vertices* (mapcar #'list *vertices*))
	 (*solved-edges* (mapcar #'list *edges*))
	 (pos-map (translate-positions (pre-generate))))
    (print-positions (mapcar #'first pos-map) :source t)
    (print-facets (remove-outer-facet (find-facets)))
    (print-positions (mapcar #'second pos-map))))

(defun start ()
  (let ((*vertices* nil)
	(*edges* nil))
    (read-input)
    (print-output)
    (sb-ext:exit)))

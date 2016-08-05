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

(defun read-vertex ()
  (let ((x) (y))
    (setf x (read))
    (read-char)
    (setf y (read))
    (make-point x y)))

(defun update-vertices (x)
  (unless (member x *vertices* :test #'equal) (push x *vertices*)))

(defun read-edge ()
  (let ((a (read-vertex))
	(b (read-vertex)))
    (update-vertices a)
    (update-vertices b)
    (make-edge a b)))

(defun read-input ()
  (setf *vertices* nil
	*edges* nil
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

(defun find-bottom ()
  (fourth *edges*))

(defun rotate-pos (pos alpha)
  (make-point (+ (* (edge-dx alpha) (px pos)) (* (edge-dy alpha) (py pos)))
	      (- (* (edge-dx alpha) (py pos)) (* (edge-dy alpha) (px pos)))))

(defun translate-pos (pos bottom)
  (rotate-pos (point- pos (edge-start bottom)) bottom))

(defun good-one (pos)
  (and (<= 0 (px pos) 1) (<= 0 (py pos) 1)))

(defun find-positions ()
  (let ((bottom (find-bottom))
	(good-positions nil))
    (dolist (dst *vertices* good-positions)
      (let ((src (translate-pos dst bottom)))
	(when (good-one src) (push (list src dst) good-positions))))))

(defun find-facets (pos-map)
  '((0 1 2 3)))

(defun print-output ()
  (let ((pos-map (find-positions)))
    (print-positions (mapcar #'first pos-map) :source t)
    (print-facets (find-facets pos-map))
    (print-positions (mapcar #'second pos-map))))

(defun start ()
  (let ((*vertices* nil)
	(*edges* nil))
    (read-input)
    (print-output)
    (sb-ext:exit)))

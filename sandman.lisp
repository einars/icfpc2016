(defpackage :origami/sandman
  (:use :cl :origami/structures))

(in-package :origami/sandman)

(defvar *vertices* nil)
(defvar *skeleton* nil)
(defvar *silhouette* nil)

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
    (list a b)))

(defun update-silhouette (vertices)
  (when (cdr vertices)
    (push (list (first vertices) (second vertices)) *silhouette*)
    (update-silhouette (cdr vertices))))

(defun generate-silhouette (vertices)
  (push (list (first vertices) (car (last vertices))) *silhouette*)
  (update-silhouette vertices))

(defun read-input ()
  (let ((polygon-count (read)))
    (dotimes (i polygon-count)
      (let ((vertex-count (read)))
	(dotimes (j vertex-count)
	  (push (read-vertex) *vertices*)))))
  (generate-silhouette *vertices*) ;; FIXME
  (let ((edge-count (read)))
    (dotimes (i edge-count)
      (push (read-edge) *skeleton*))))

(defun start ()
  (let ((*vertices* nil)
	(*skeleton* nil)
	(*silhouette* nil))
    (read-input)
    (format t "VERTICES: ~A~%" *vertices*)
    (format t "SKELETON: ~A~%" *skeleton*)
    (format t "SILHOUETTE: ~A~%" *silhouette*)
    (sb-ext:exit)))

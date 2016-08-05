(defpackage :origami/sandman
  (:use :cl :origami/structures))

(in-package :origami/sandman)

(defvar *vertices* nil)
(defvar *edges* nil)

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
  (let ((polygon-count (read)))
    (dotimes (i polygon-count)
      (let ((vertex-count (read)))
	(dotimes (j vertex-count)
	  (push (read-vertex) *vertices*)))))
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

(defun start ()
  (let ((*vertices* nil)
	(*edges* nil))
    (read-input)
    (print-positions *vertices* :source t)	; TODO
    (print-facets '((0 1 2 3)))			; TODO
    (print-positions *vertices*)		; TODO
    (sb-ext:exit)))

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

(defun find-bottom ()
  (first (last *edges*)))

(defun rotate-pos (pos b)
  (make-point (- (* (edge-dx b) (point-x pos)) (* (edge-dy b) (point-y pos)))
	      (+ (* (edge-dy b) (point-x pos)) (* (edge-dx b) (point-y pos)))))

(defun translate-pos (pos bottom)
  (rotate-pos (point- pos (edge-start bottom)) bottom))

(defun good-one (pos)
  (and (<= 0 (point-x pos) 1) (<= 0 (point-y pos) 1)))

(defun find-positions ()
  (let ((bottom (find-bottom))
	(good-positions nil))
    (dolist (dst *vertices* good-positions)
      (let ((src (translate-pos dst bottom)))
	(format t "SRC:~A DST:~A~%" src dst)
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

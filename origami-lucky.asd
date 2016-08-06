(in-package :asdf)

(defsystem :origami
  :name "Origami solver"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :components ((:file "structures")
	       (:file "sandman")
	       (:file "polygon-flipper")
	       (:file "point-and-edge-show"))
  :depends-on (:screamer))

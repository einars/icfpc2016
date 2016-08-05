(in-package :asdf)

(defsystem :origami
  :name "Origami solver"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:origami/sandman))

all:
	@sbcl --load origami-lucky.asd \
		--eval "(ql:quickload 'origami)" \
		--eval "(compile-file \"point-and-edge-show.lisp\")" \
		--eval "(origami/polygon-flipper::pipe-start)" < 42.txt

sandman:
	@sbcl --noinform --noprint --load origami.asd \
		--eval "(asdf::load-system 'origami)" \
		--eval "(origami/sandman::start)" < 12.txt

buildcers:
	@sbcl --load origami-lucky.asd \
		--eval "(ql:quickload 'origami)" \
		--eval "(compile-file \"point-and-edge-show.lisp\")" \
		--eval "(sb-ext::save-lisp-and-die \"cers.bin\" :toplevel #'origami/polygon-flipper::pipe-start :executable t)"

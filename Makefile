all:
	@sbcl --noinform --noprint --load origami.asd \
		--eval "(asdf::load-system 'origami)" \
		--eval "(origami/sandman::start)" < 12.txt


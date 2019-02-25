SOURCES=$(shell find src/Beeraffe -type f)

EMOJIONE_DIR=$(HOME)/Stash/emojione-4.5/emojione-4.5-free

PULP=$(shell npm bin)/pulp
WEBPACK=$(shell npm bin)/webpack

# Prevents make from removing the temporary files.
.SECONDARY:

.PHONY: default all
default: dist/Beeraffe.bundle.js dist/index.html static

dist/index.html: static
	cp build/index.html $@

dist/Beeraffe.bundle.js: build/Beeraffe.js static
	$(WEBPACK) --progress --config webpack.config.js

build/%.js: src/%.purs $(SOURCES)
	mkdir -p build
	$(PULP) --psc-package build -O --to $@ -m $*

.PHONY: clean
clean:
	rm -rf build dist

.PHONY: deploy
deploy:
	bash scripts/deploy.sh

.PHONY: setup
setup:
	npm install webpack-cli pulp xhr2 file-loader css-loader

.PHONY: static
static: build/sprites.png build/words.txt
	mkdir -p build
	rsync data/*.css build
	rsync data/*.html build
	rsync data/*.png build
	rsync data/*.ttf build

build/sprites.png: data/words.txt
	mkdir -p build
	python scripts/generate-sprites.py

build/words.txt: data/words.txt
	sed 's/:.*$$//' $< >$@

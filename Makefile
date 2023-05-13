EMACS?=emacs
SOURCE_DIR?=$(CURDIR)
PACKAGE_VERSION=$(shell cask version)

.PHONY: clean
clean:
	cask clean-elc
	rm -rf ./dist

.cask:
	cask install

dist: .cask
	cask build
	cask package

install: dist
	$(EMACS) --batch -f package-initialize --eval "(package-install-file \"$(SOURCE_DIR)/dist/wal-line-$(PACKAGE_VERSION).tar\")"

clean-install: clean install

EMACS?=emacs
SOURCE_DIR?=$(CURDIR)
PACKAGE_VERSION=$(shell cask version)

clean:
	cask clean-elc

package: clean
	cask build
	cask package

install: package
	$(EMACS) --batch -f package-initialize --eval "(package-install-file \"$(SOURCE_DIR)/dist/wal-line-$(PACKAGE_VERSION).tar\")"

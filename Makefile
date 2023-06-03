EMACS?=emacs
SOURCE_DIR?=$(CURDIR)
PACKAGE_VERSION=$(shell cask version)

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:

# -- Default goal

ifdef CI
install: ci
else
install: local
endif

.PHONY: package-install
package-install: dist
	$(EMACS) --batch -f package-initialize --eval "(package-install-file \"$(SOURCE_DIR)/dist/wal-line-$(PACKAGE_VERSION).tar\")"

.PHONY: clean-install
clean-install: clean install

.PHONY: ci
ci: .cask

.PHONY: local
local: dist

dist: .cask
	cask build
	cask package

.cask:
	cask install

# -- Checks

# Run tests using cask
.PHONY: test
test: .cask
	mkdir -p coverage
	cask exec ert-runner $(TEST_ARGS)

# -- Clean-up

.PHONY: clean
clean:
	cask clean-elc
	rm -rf dist

.PHONY: clobber
clobber: clean
	rm -rf .cask

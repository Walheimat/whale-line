EMACS?=emacs
SOURCE_DIR?=$(CURDIR)
PACKAGE_VERSION=$(shell cask version)
TEST_PRE_ARGS=
TEST_ARGS=

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
	$(EMACS) --batch -f package-initialize --eval "(package-install-file \"$(SOURCE_DIR)/dist/whale-line-$(PACKAGE_VERSION).tar\")"

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
	$(TEST_PRE_ARGS) cask exec ert-runner $(TEST_ARGS)

.PHONY: local-test
local-test: test
	cat coverage/results.txt
	cask emacs --batch -f bydi-calculate-coverage

.PHONY: coverage
coverage: TEST_PRE_ARGS=COVERAGE_WITH_JSON=true
coverage: test

# -- Clean-up

.PHONY: clean
clean:
	cask clean-elc
	rm -rf dist

.PHONY: clobber
clobber: clean
	rm -rf .cask

PACKAGE_NAME=whale-line
PACKAGE_SUFFIX=tar

CURRENT_PACKAGE_VERSION=0.9.1
UPDATE_VERSION_FILES=Cask \
					 whale-line.el \
					 whale-line-segments.el \
					 whale-line-iconify.el \
					 whale-line-edit.el \
					 Makefile

include dinghy/emacs-package.mk

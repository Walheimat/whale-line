PACKAGE_NAME=whale-line
PACKAGE_SUFFIX=tar

CURRENT_PACKAGE_VERSION=0.7.4
UPDATE_VERSION_FILES=Cask \
					 whale-line.el \
					 whale-line-core.el \
					 whale-line-segments.el \
					 whale-line-iconify.el

include dinghy/emacs-package.mk

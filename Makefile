clean:
	cask clean-elc

package: clean
	cask build
	cask package

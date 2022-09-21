# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0]

### Added

- New segment for `org-mode` to display the most recent heading before
  window start.
- `icons` package has new custom variable
  `wal-line-icons-prettify-buffer-status` to do just that.
- New macro `wal-line-create-static-segment` to create `defvar`-based
  segments that don't need to be created often.
- Segments `buffer-name`, `project`, `icons`, `vc` and `whale` use
  this macro.
- New macro `wal-line-create-dynamic-segment` to create `defun`-based
  segments that need to be updated all the time.
- Segments `org` and `mc` use this macro.

### Changed

- Position segment shows column number.
- Macro `wal-line-add-segment` is now a function.

### Fixed

- Process segment can handle `mode-line-process` being a string. It is
  then displayed using the `wal-line-shadow` face.

## [0.2.0]

### Added

- LSP status is now indicated using either the icon or the buffer name.
- A strategy can be selected that decides what to do when there's not
  enough space to display both sides' segments. Segments now are
  either `t`, `low` or `nil` indicating their priority. The default
  strategy only shows segments that are `t`. Strategy `elide` only
  shows the left hand side. Strategy `ignore` is the previous behavior
  of displaying everything whether there's enough space or not.

### Changed

- Buffer with no underlying file now use `&` for the buffer status.
- While `flycheck` is running, the buffer name is shown underlined but
  shadowed.

### Fixed

- No longer runs `kill-buffer-hook` to tear down the whale segment.
- Use `wal-line--spacer` before `process` segment.

## [0.1.1] - 2021-04-30

### Changed

- Code from package `wal-line-utils` was relocated into base package.

### Fixed

- Requiring other packages now happens on activation.

## [0.1.0] - 2021-04-30

### Added

- The base package that includes segments for buffer name, buffer
  status, position, selection, process, minor modes and global mode
  string.
- Package to integrate `flycheck` (embellishing buffer name).
- Package to integrate `all-the-icons` (including buffer segment).
- Package to integrate `multiple-cursors` as a segment.
- Package to integrate `minions` (embellishing minor modes).
- Package to integrate `projectile` and `project` as a segment.
- Package to integrate version control information using `vc` as a
  segment.
- An animated whale as a segment.

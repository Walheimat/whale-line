# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Custom variable `whale-line-log`. Setting it to `t` will log setups
  and teardowns. To see the output, call `whale-line-pop-to-logs`.
- `flymake` segment now also indicates running state.
- Segment setups and teardowns are now easier to reason about.
  Segments are only set up or torn down during
  `whale-line-{setup,teardown}-hook` if the segment is part of
  `whale-line-segments`. When segments are rebuild, only the
  added/removed segments are set up/torn down. Augments are handled
  differently, they **need** a verify function that is called when the
  hooks are run. During rebuild, they are not setup/torn down (the
  same goes for segments that use `:verify`).

### Changed

- Segment `selection` now shows string `mc` when region *and*
  `multiple-cursors-mode` is active.
- Icons are no longer enabled in non-GUI-mode since they are garbled.
- **BREAKING** segment `dap` was renamed to `debug` to allow for also
  handling alternative packages (like `dape`).

### Removed

- Segment for `partial-recall` was removed as its mode-line lighter
  now offers that same functionality.

### Fixed

- `flymake` segment shows the default help text.

## [v0.7.4]

### Added

- Augment for `flymake` that (hopefully) works just like the
  `flycheck` augment.

### Changed

- Position segment now renders the default `mode-line-position`
  construct if `doc-view` or `pdf-view` are used.

## [v0.7.3]

### Removed

- `dir-locals.el` is no longer tracked.
- Custom variable `whale-line-segments-org-max-length`.

### Changed

- `org` segment now reduces the max heading length if space is an
  issue using a crude heuristic.

## [v0.7.2]

### Added

- Segments can now set key `:padded` to `left`, `right` or `all` to
  communicate that they come with padding. `minor-modes` sets this to
  `left`. This value is respected and no additional padding is added
  to segments to the left or right of it.
- Space calculations for windows are now cached, greatly reducing the
  time spent on figuring out which segments can be shown.
- Priorities can now be updated in bulk using macro
  `whale-line-with-priorities`.

### Changed

- Segment `buffer-identification` now uses face `mode-line-buffer-id`
  as base.
- Segments that are based on a variable now use keyword `:var` instead
  of providing an anonymous function returning it.

### Fixed

- Augment `flycheck` now sets variables for `buffer-identification` so
  that segment running its hooks won't remove the underlining.
- Passing lambdas as a `:getter` should work now.

## [v0.7.1]

### Added

- Package `whale-line-iconify` to house all the icon-related logic.
  Icon usage has been changed to derive the icon from
  `whale-line-iconify-specs` that declare a default if an icon can't
  or shouldn't be used. The latter is controlled by
  `whale-line-iconify-disabled`.
- Segments can use `:dense` again. Such segments will receive
  no padding irrespective of the side they're on.
- Segment for `dap-mode`.
- `project` segment now has a local map, binding `project-dired` to
  `mouse-1`.
- `window-status` segment now indicates when a window has parameter
  `no-other-window`.

### Removed

- `whale-line-segments-project-provider` was removed; `projectile` is
  no longer supported.
- `whale-line-cursors` segment. The information is already available
  through the minor mode strings.

### Changed

- The logic for the `org` segment has been changed. Custom variable
  `whale-line-segments-org-include` was replaced in favor of
  `whale-line-segments-org-max-count`. If a heading has more parents
  than set in this variable, they are elided. The elision is indicated
  by `whale-line-segments-org-elision`, defaulting to an asterisk.
  Variable `whale-line-segments-org-divider` (between it and previous
  segment) was removed and `whale-line-segment-org-separator` (between
  subsegments) added.
- Segment `partial-recall` now just shows the amount of mapped
  buffers, the full information is displayed in the help echo.
- Macros have been renamed
  - `whale-line-create-{dynamic=>stateless}-segment` and
  - `whale-line-create-{static=>stateful}-segment`
  to better communicate their difference and usage.
- Segment `tab-bar` now displays the tab index if no explicit name was
  set.
- Segment `lsp-mode` now shows the servers/workspaces it's connected
  to.
- The logic for `vc` segment has changed. It now can display all
  states for Git-based projects.
- Segment `buffer-icon` was renamed to `major-mode`; it now displays
  either the icon or the `mode-name` when the segment is part of
  `whale-line-iconify-disabled`.
- Segment `position` now respects `mode-line-column-line-format` and
  `mode-line-percent-position`.

### Fixed

- The animation segment is now a dynamic segment, allowing the
  animation frame to no longer be local to a buffer.

## [v0.7.0]

### Added

- Segment for my own library `partial-recall`. Will toggle implanted
  state on left-click, show a menu on right-click.
- Static segments are now refreshed after the buffer list is updated
  using an idle timer.

### Changed

- All segments use prefix `whale-line-segments-` now, including custom
  variables. They also belong to new customization group
  `whale-line-segments`.
- The widths of the sides are now calculated using
  `string-pixel-width` if possible.

## [v0.6.2]

### Added

- Segment `lsp` and custom variable `whale-line-icons-lsp-icon`.

### Changed

- Segment `global-mode-string` is now `misc-info` and returns the
  equivalent information in `mode-line-misc-info`.
- Segment `process` now returns `mode-line-process` as-is.
- Segment `buffer-name` is now `buffer-identification` and was
  simplified.
- Default animation speed was reduced to 1.0.
- Segment `icons` was replaced by `buffer-icon`. The augmentations of
  other segments were spliced out as `iconify-*`.

### Removed

- Augment `lsp` and custom variable `whale-line-lsp-delimiters`.

## [v0.6.1]

### Added

- Segments are now padded based on the side they're on (left padding
  on the left, right padding on the right). This is done during mode
  line formatting. This means that segments can be freely positioned
  and will have the correct padding based on their position.

### Changed

- Verification is no longer performed on macro expansion. Instead,
  during building the segments, any segment that doesn't pass a
  defined verification is not added. Furthermore, such a segment's (or
  augment's) setup function will return early as well.

### Removed

- Keyword `:dense` has been removed from macros. This means some
  segments that previously avoided having both a natural and an added
  padding have double padding for now. Resolving this will be part of
  a future fix.

## [v0.6.0]

### Changed

- Removed segments modules and factored them into
  `whale-line-segments`.

### Removed

- `whale-line-{enable,disable}-feature` was removed as features no
  longer exist.

## [v0.5.1]

### Changed

- Macros to create segments (or augments) have been unified and now
  use helper macros. `{segment}--{set-segment, augment}` is now
  `{segment}--action`.
- Default segments were factored out into package
  `whale-line-segments`.
- Switched to using `dinghy`.

### Fixed

- Segment `buffer-name` now searches `mode-line-buffer-identification`
  for the first `car` that is a string. If that fails, it returns
  `buffer-name`. This was a problem with packages like
  `git-timemachine`.
- `doc-view-mode` buffers now display page numbers.

## [v0.5.0]

### Added

- Segment `project` and augment `flycheck` now have help echo texts
  showing root path and errors respectively.
- Testing setup. This required reworking the macros a bit to allow for
  passing functions that can be tested independently.
- All packages now have test suites.

### Changed

- Package was renamed from `wal-line` to `whale-line`.=SF200
- `ready-symbol-shorthands` used in some sub-packages to keep their
  name lengths in check.
- `buffer-name` segment now uses the local map and help echo from
  `modeline-buffer-identification`.

## [0.4.0]

### Added

- Keyword `:verify` or all segment types that takes a function as an
  argument. If that function yields `nil` the segment won't be
  added/augment won't take place.
- Uses this for `flycheck` and `all-the-icons` augment and segment.
- New Macro `wal-line-create-augment` to create augmentations.
- Segments `lsp`, `minions` and `flycheck` use this macro.
- New priority `current-low` to have low priority segments that should
  only appear on the current window.
- Uses `project-name` to determine project name when using
  `project.el` as provider.
- Segment for `window-status` that displays if the window is dedicated
  or not.
- Sub-package `wal-line-mc` was superseded by `wal-line-cursors` that
  has the same functionality but also shows `iedit-mode` cursors.
- Sub-package `wal-line-whale` was superseded by `wal-line-animation`
  since it can display any animation.
- `project` segment is only displayed for file and Dired buffers.
- Each icon can now be customized.
- Segment `tab-bar` to show the names of tabs where it was explicitly
  named.

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

# Changelog

## [0.9.1](https://github.com/Walheimat/whale-line/compare/v0.9.0...v0.9.1) (2024-10-13)


### Improvements

* **buffer-ident:** allow truncating additional segments ([bd2c3d8](https://github.com/Walheimat/whale-line/commit/bd2c3d8a5f7e9974f3970f953682be4551cd57f4))
* **buffer-identification:** subtract project-root ([8c53da4](https://github.com/Walheimat/whale-line/commit/8c53da402ca12d7b7068b89f851610056ce388e6))
* **segments:** change priority current-low=>current ([934115e](https://github.com/Walheimat/whale-line/commit/934115e5fd83a52f56b46458e1f6186359e57327))
* **tab-bar-segment:** always use the name ([0a6b47f](https://github.com/Walheimat/whale-line/commit/0a6b47ff38541198780531d9155c848dd36fee96))

# [0.9.0](https://github.com/Walheimat/whale-line/compare/v0.8.5...v0.9.0) (2024-06-29)


### Features

* **segments:** buffer-identification now prepends path ([e100675](https://github.com/Walheimat/whale-line/commit/e1006758836322c7b545ca8ed90f19885f646a84))

## [0.8.5](https://github.com/Walheimat/whale-line/compare/v0.8.4...v0.8.5) (2024-01-28)


### Features

* **ci:** add semantic-release ([3352cda](https://github.com/Walheimat/whale-line/commit/3352cdac59f7bc052ec795680e380f74e7d9883e))
* **padding:** add variable to highlight padding ([3932b55](https://github.com/Walheimat/whale-line/commit/3932b55b7c2012524322d84b0c100bf398dfbff1))
* **segments:** consider misc-info and process problematic ([12c3be9](https://github.com/Walheimat/whale-line/commit/12c3be9ab17ed927a4a2a130a7555d97b43d030d))

## [0.8.4]

### Added

- Keyword `:wraps` for augments. This is a shorthand to `:after-while`
  and passing the name of the render function of another segment.

## [0.8.3]

### Added

- `vc` segment now has different indicators per state that are
  propertized with faces instead of the branch name itself.
- Segment for `mode-line-client`.

### Changed

- `whale-line-log` is now either `nil`, `0`, or `1`. The numbers
  represent log levels.
- `minions` segment no longer propertizes the prominent modes using
  `whale-line-shadow`.

## [v0.8.1]

### Added

- Module `whale-line-edit`. It provides a command of the same name to
  edit buffer segment positioning (and inclusion) in a separate
  buffer. The resulting edit can be applied or persisted (by saving
  the customization). The user may also revert or abort. The changes
  are validated before they are applied.
- `whale-line-iconify` is now set as the default value for new custom
  variable `whale-line-segments-decorator`, decoupling
  `whale-line-segments` from it.

### Changed

- Segment `buffer-status` now differentiates between writable and
  read-only buffers. When using icons, a writable buffer that has no
  file name will indicate this through icon color. When not using
  icons, states are concatenated, meaning if the icon is both modified
  and doesn't belong to a file both fallback strings are shown.
- Package `whale-line-core` was folded into `whale-line`. Package
  `whale-line-segments` is now required during
  `whale-line--build-segments`.
- `whale-line-iconify` no longer defaults, it's the responsibility of
  the segment.
- `org` segment now shows only the deepest segment if its title
  exceeds the combined max length of the remaining segments.

## Fixed

- Padding for `buffer-status`.

## [v0.8.0]

### Added

- A section was added that explains how users can create their own
  segments and augments in detail.
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
- Command `whale-line-trigger-augments` to set up or tear down
  augments.
- Stateful and stateless segments can now define a "port" function.
  This for augments using new key `:plugs-into <segment-name>`.
  The actions of augments using this key will call the port function
  with their result.
- Stateful segments can now use new key `:after` instead of
  constructing with `:advice`.
- Augments now can use `:after` and `:after-while` in the same way.
- Passing single item values to keys that expect a list now normalize
  to a list.

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
- `flymake` segment now calls hook to update hook to update
  `buffer-identification` like `flycheck` does.

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

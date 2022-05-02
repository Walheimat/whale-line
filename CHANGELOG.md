# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- Buffer with no underlying file now use `&` for the buffer status.

### Fixed

- No longer runs `kill-buffer-hook` to tear down the whale segment.

### Fixed

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

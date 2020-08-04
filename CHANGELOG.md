# Revision history for tg-form
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Package Versioning Policy (PVP)](https://pvp.haskell.org).

## [Unreleased](https://git.sr.ht/~rd/tg-form/tree/master)

### Added
### Changed
### Deprecated
### Removed
### Fixed
### Security

## [0.1.0.0](https://git.sr.ht/~rd/tg-form/tree/v0.1.0.0) -- 2020-08-04

* First version. Released on an unsuspecting world.
* This version is a major rewrite of a code written for [OZI
  Hackathon](https://hackathon.ozi-ru.org/) (site is in Russian)
* Hackathon submission can be found
  [here](https://github.com/Hackathon-on-Internet-freedom/tg-stat)
  (documentation is mainly in Russian)

### Added

* Enum type for questions: user is presented with predefined set of answers
* User can add own forms by sending Google sheet to the bot

### Changed

* Geography type for questions has a precision: bot can save exact coordinates
  of the user, or coordinates of the city etc.
* Time and user-id are saved only if necessary (i.e. form author asks for that)

### Removed

* Encryption and hash-chains are removed as unnecessary complication

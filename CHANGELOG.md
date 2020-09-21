# Change Log
All notable changes to the `monad-memo` project will be documented in this file

## [0.5.3] - 2020-09-21
### Fixed
- `README.md` links
### Removed
- Travis-ci build configuration

## [0.5.2] - 2020-09-20
### Added
- CI on Github actions with test coverage and Hackage upload
### Fixed
- `monad-memo.cabal` structure: redundancy and to enable test coverage calculation
- `CHANGELOG.md` structure

## [0.5.1] - 2018-08-31
### Added
- Support multiple mutable caches in transformers stack
  This allows Array/Vector-based caches to be used for mutually recursive function memoization

## [0.5.0] - 2018-08-06
### Fixed
- Refresh project to be compilable with latest GHC and libraries
- Remove dependency on `mtl` package (`transformers` is sufficient)
- Use `Except` instead of deprecated `Error`
- Remove support for `ListT` transformer since it is now deprecated
- Use standard `StateT` & `ReaderT` for `MonadCache` implementations

## [0.4.1] - 2013-03-06
### Fixed
- Documentation
- `Example` is renamed to `example` and is excluded from package's module hierarchy

## [0.4.0] - 2013-02-26
### Added
- `ArrayCache`: mutable array-based `MonadCache` for top performance
- `VectorCache` (and flavours) `vector`-based `MonadCache` for even better performance
- Simple benchmark included
### Fixed
- Bug fixes in transformer implementations (`Reader`, `State`, `RWS`)

## [0.3.0] - 2011-04-03
### Added
- Added generalized `MemoStateT` transformer (to host any `Data.MapLike` cache-container)
- `MemoT` is now `MemoStateT` instantiated with `Data.Map`

## [0.2.0] - 2011-03-27
### Added
- A set of `forX` functions (`for2`, `for3` and `for4`) to adapt curried function into uncurried `MemoCache`

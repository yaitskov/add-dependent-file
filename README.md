# add-dependent-file

The library provides a safer wrapper around
[addDependentFile](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html#v:addDependentFile)
that checks that `extra-source-files` section in the cabal file
contains **addDependendFile** argument and issues a correspondent
compilation warning if it does not, because `addDependentFile` has no
effect otherwise.

## Development environment

```shell
$ nix develop
$ emacs &
$ cabal build
$ cabal test
```

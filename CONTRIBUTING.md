# How to contribute

## To build

Prepare Nix environment first, See [the Nix official instructions](https://nixos.org/download/).
Or install GHC and Cabal manually.

Then, enter the Nix shell by running:

```console
$ nix develop
```

You can build the project by running:

```console
$ cabal build
```

When building with all tested GHC versions, run:

```console
$ make TARGET=build each
```

## To test

There are no unit tests for now, but you can build and run the example programs to verify everything works fine:

```console
$ make test
```

To run tests with all tested GHC versions, run:

```console
$ make TARGET=test each
```

## To format

```console
$ make format # or
$ nix fmt
```

Without Nix, you need to run nixfmt and stylish-haskell manually.

# Assignment solutions

## Haskell assignment

Haskell solution can be found in `sim` directory. I used `stack` for
building. It is sufficient to run:

```
stack build
```

to build it and

```
stack exec solution
```

to run the main program.

The actual code, with extensive comments can be found in the `sim/Sim.hs`
file. The `sim/solution.hs` is just and app entrypoints.

The `sim/SimOld.hs` is the first and deprecated iteration and can be ignored.

Some cross-checks were performed using python (jupyter notebook); commited in
the `sim` directory.

## Elm assignment

Design seems to use Futura font which is not freely available (only on MacOS). Moreover,
weights suggest that it may be commercial variant (seems to be more I can get from system one).

Styles do not correspond idealy to specification, but it should be straighforward
to adjust them.

Solution for the click-outside taken from https://discourse.elm-lang.org/t/clicking-outside-a-dialog-modal-to-close/3735/5

Code requires some refactoring to improve readability.

### Building

```console
cd drops
elm make src/Main.elm --output elm.js
```

### Viewing

Just open `drops/index.html`.
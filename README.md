# Sound Notional Machines - Submitted PLDI 2023

This is the repository for the paper.
There is a separate repository for the artifact: [notional-machines](https://github.com/LuCEresearchlab/sound-notional-machines).

Because a version of the artifact is necessary to build the paper, 
this paper repository also includes a packaged version of the artifact
that is ready to install and referred to in the `cabal.project` file.


## Build the Paper

To build the paper, do the following:

- Install GHCup (https://www.haskell.org/ghcup/)
- Install Haskell and Cabal (using ghcup)
- Install the artifact and the Haskell Diagrams library
```
cabal update
cabal install -fpgf diagrams-builder
cabal install --lib diagrams-builder diagrams-lib diagrams-pgf notional-machines mtl prettyprinter --package-env=./
```
- Install ([lhs2tex](https://hackage.haskell.org/package/lhs2tex-1.18/src/doc/Guide2.pdf))
```
cabal install lhs2tex
```
- Build the paper
```
make
```


## Continuous Compilation of the Paper

To build the paper in continuous compilation style use:

```sh
while true; do fswatch -1 main.tex sections/*.tex sections/*.lhs; make; done
```


## Reinstall the Artifact

If you need to install a new version of the artifact you need to remove the environment manually first,
 and maybe remove the cache created by diagrams-latex
(there's gotta be a better way to do this, but I couldn't find it).

```sh
rm .ghc.environment.x86_64-darwin-9.0.2
rm -r .diagrams_cache
```


## Dependencies

- The uses of equational reasoning are formatted with Literate Haskell using [lhs2tex](https://hackage.haskell.org/package/lhs2tex-1.18/src/doc/Guide2.pdf).
- We use our artifact to produce many of the concrete representations of notional machines used in the paper. The artifact produces these concrete representations using the Haskell [diagrams](https://diagrams.github.io/) library. The diagrams are embedded directly into the tex files using [diagrams-latex](https://diagrams.github.io/doc/latex.html).

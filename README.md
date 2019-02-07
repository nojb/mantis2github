## Mantis => GH Migration tool for OCaml bugtracker

This a one-shot script to help migrate the OCaml bugtacker
https://caml.inria.fr/mantis to GH issues.

## Prerequisites

```
opam install mysql yojson cmdliner dune
```

## Getting and building

```
git clone https://github.com/nojb/mantis2github
cd mantis2github
dune build
```

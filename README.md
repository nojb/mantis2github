## Mantis => GH Migration tool for OCaml bugtracker

This a one-shot script to help migrate the OCaml bugtacker
https://caml.inria.fr/mantis to GH issues.

## Prerequisites

- https://github.com/ygrek/ocaml-mysql (`opam install mysql`)
- https://github.com/ocaml-community/yojson (`opam install yojson`)
- `unix` library

## Getting and building

```
git clone https://github.com/nojb/mantis2github
cd mantis2github
dune build
```

## User names

In order to map Mantis user names to GH user names, the file `user_map.txt` is
used. If a Mantis username `foo` is not found in that file, then an (illegal) GH
name `foo@Mantis` is used as a placeholder.

## How to run

For now, the script assumes:

- MySQL server listening on `127.0.0.1` with user `root` and no password;
- A database named `db` with the Mantis data in it.

You can run the script by doing:

```
_build/install/default/bin/mantis2github > mantis.json 2> missing.txt
```

and you will obtain

- `mantis.json`, a JSON dump of the Mantis data. It contains GH usernames
  obtained as explained above. Timestamps are in ISO 8601 format, as required by
  the GH API.

- `missing.txt` contains a list of user names which are missing from
  `user_map.txt`.

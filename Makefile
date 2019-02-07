OWNER=nojb
REPO=ocaml-mantis-wip
PROG=_build/install/default/bin/mantis2github

ifeq (,$(TOKEN))
$(error Please specify TOKEN)
endif

.PHONY: all
all:
	dune build

.PHONY: migrate
migrate: all
	$(PROG) --owner $(OWNER) --repo $(REPO) --token $(TOKEN) 2> err.txt | tee out.txt

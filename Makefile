OWNER=nojb
REPO=ocaml-mantis-wip
PROG=_build/install/default/bin/mantis2github

FLAGS=--owner $(OWNER) --repo $(REPO) --token $(TOKEN)

ifdef FROM
FLAGS += --from $(FROM)
endif
ifdef VERBOSE
FLAGS += --verbose
endif

ifeq (,$(TOKEN))
$(error Please specify TOKEN)
endif

.PHONY: all
all:
	dune build

.PHONY: migrate
migrate: all
	$(PROG) migrate $(FLAGS) 2> err.txt | tee out.txt

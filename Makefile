OWNER=mantismigrationtest-123
REPO=ocaml-mantis-wip
PROG=_build/install/default/bin/mantis2github

FLAGS=$(OWNER)/$(REPO) --token $(TOKEN) --assignee $(OWNER)

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
	$(PROG) import $(FLAGS) $(BUGS) 2>&1 | tee out.txt

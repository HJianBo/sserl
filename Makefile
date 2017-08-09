ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := video_manager

REBAR := ./rebar3
REBAR_OPTS ?=

.PHONY: all
all: compile

.PHONY: rel prod-rel
rel: 
	$(REBAR) release

prod-rel: 
	$(REBAR) as prod tar

.PHONY: shell
shell:
	$(REBAR) shell --name=video_manager@127.0.0.1	

# Use Rebar to get, update and compile dependencies
.PHONY: upgrade-deps compile-video_manager compile 

upgrade-deps: $(REBAR)
	$(REBAR) $(REBAR_OPTS) upgrade

compile: $(REBAR)
	$(REBAR) $(REBAR_OPTS) compile


# Generate documentation
.PHONY: docs edocs
docs:
	@echo Building HTML documentation...
	cd doc && $(MAKE) stubs && $(MAKE) html
	@echo HTML documentation is now available in doc/_build/html/

edocs:
	@echo Building reference edoc documentation...
	bin/video_manager generate-edoc

# Cleaning

.PHONY: clean
clean:  $(REBAR)
	rm -rf ebin
	$(REBAR) $(REBAR_OPTS) clean

.PHONY: dist-clean
dist-clean: clean
	$(REBAR) $(REBAR_OPTS) clean -a

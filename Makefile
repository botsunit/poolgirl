PROJECT = poolgirl

DEP_PLUGINS = mix.mk
BUILD_DEPS = mix.mk
ELIXIR_VERSION = ~> 1.2
dep_mix.mk = git https://github.com/botsunit/mix.mk.git master

DOC_DEPS = edown
dep_edown = git https://github.com/botsunit/edown.git master

include erlang.mk

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {top_level_readme, {"./README.md", "https://github.com/botsunit/$(PROJECT)"}}

dev: app tests
	@erl -pa ebin include deps/*/ebin deps/*/include test

release: app mix.exs


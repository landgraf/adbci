DESTDIR	?= /usr/local
LIBDIR ?= ${DESTDIR}/lib/adbci
# Default destination directory

all: default 

default: directories examples 
	@gnatmake -P adbci.gpr

clean:
	@rm -f lib/* bld/*
	@rm -f examples/active_record/bin/*
	@rm -f examples/active_record/bld/*

directories:
	@mkdir -p bld
	@mkdir -p lib
	@mkdir -p examples/active_record/bin
	@mkdir -p examples/active_record/bld

example_active_record:
	@cd examples/active_record && gnatmake -P active_record.gpr

examples: example_active_record

install:
	@mkdir -p ${LIBDIR}
	@cp src/core/*.ad? ${LIBDIR}
	@cp src/active_record/*.ad? ${LIBDIR}
	@cp src/postgresql/*.ad? ${LIBDIR}


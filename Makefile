DESTDIR	?= /usr/local/lib/adbci
# Default destination directory

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
	@mkdir -p ${DESTDIR}
	@cp src/core/*.ad? ${DESTDIR}
	@cp src/active_record/*.ad? ${DESTDIR}
	@cp src/postgresql/*.ad? ${DESTDIR}


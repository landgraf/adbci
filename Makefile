DESTDIR	= /usr/local/lib/adbci
# Default destination directory

default: 
	@gnatmake -P adbci.gpr

clean:
	@rm -f lib/* bld/*

install:
	@mkdir -p ${DESTDIR}
	@cp src/core/*.ad? ${DESTDIR}
	@cp src/active_record/*.ad? ${DESTDIR}
	@cp src/postgresql/*.ad? ${DESTDIR}


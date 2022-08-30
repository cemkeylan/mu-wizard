.POSIX:

PREFIX     = /usr/local
BINDIR     = ${PREFIX}/bin
SHAREDIR   = ${PREFIX}/share
MANDIR     = ${SHAREDIR}/man
MAN1       = ${MANDIR}/man1
MUSHAREDIR = ${SHAREDIR}/mu-wizard
INSTALLSH  = ./tools/install.sh
VERSION    = 1.0.0-rc1

all:
	@echo "Run 'make install' to install mu-wizard."

dist:
	mkdir -p mu-wizard-${VERSION}
	cp -r LICENSE README.org mu4e-config.el tools Makefile bin man overrides mu-wizard-${VERSION}
	tar cf - mu-wizard-${VERSION} | xz -c > mu-wizard-${VERSION}.tar.xz
	rm -rf mu-wizard-${VERSION}

clean:
	rm -f mu-wizard-${VERSION}.tar.xz

install:
	${INSTALLSH} -Dm755 -s 's|/usr/share/mu-wizard|${MUSHAREDIR}|g;s|@VERSION@|${VERSION}|g' \
		bin/muw ${DESTDIR}${BINDIR}/muw
	${INSTALLSH} -Dm644 mu4e-config.el ${DESTDIR}${MUSHAREDIR}/mu4e-config.el
	${INSTALLSH} -Dm644 -t ${DESTDIR}${MUSHAREDIR}/overrides overrides/*
	${INSTALLSH} -Dm644 -t ${DESTDIR}${MAN1} man/*.1

uninstall:
	rm -rf ${DESTDIR}${BINDIR}/muw ${DESTDIR}${MUSHAREDIR}
	for man in man/*; do rm -f ${DESTDIR}${MANDIR}/man$${man##*.}/$${man##*/}; done

.PHONY: all dist clean install uninstall

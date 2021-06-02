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

install:
	${INSTALLSH} -Dm755 -s 's|/usr/share/mu-wizard|${MUSHAREDIR}|g;s|@VERSION@|${VERSION}|g' \
		bin/muw ${DESTDIR}${BINDIR}/muw
	${INSTALLSH} -Dm644 mu4e-config.el ${DESTDIR}${MUSHAREDIR}/mu4e-config.el
	${INSTALLSH} -Dm644 -t ${DESTDIR}${MUSHAREDIR}/overrides overrides/*
	${INSTALLSH} -Dm644 -t ${DESTDIR}${MAN1} man/*.1

uninstall:
	rm -rf ${DESTDIR}${BINDIR}/muw ${DESTDIR}${MUSHAREDIR}
	for man in man/*; do rm -f ${DESTDIR}${MANDIR}/man$${man##*.}/$${man##*/}; done

.PHONY: all install uninstall

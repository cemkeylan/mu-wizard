PREFIX     = /usr/local
BINDIR     = ${PREFIX}/bin
SHAREDIR   = ${PREFIX}/share
MUSHAREDIR = ${SHAREDIR}/mu-wizard

all:
	@echo "Run 'make install' to install mu-wizard."

install:
	mkdir -p ${DESTDIR}${BINDIR} ${DESTDIR}${MUSHAREDIR}
	sed 's|/usr/share/mu-wizard|${MUSHAREDIR}|g' < bin/muw > ${DESTDIR}${BINDIR}/muw
	chmod 755 ${DESTDIR}${BINDIR}/muw
	cp mu4e-config.el ${DESTDIR}${MUSHAREDIR}
	chmod 644 ${DESTDIR}${MUSHAREDIR}/mu4e-config.el
	mkdir -p ${DESTDIR}${MUSHAREDIR}/overrides
	for override in overrides/*; do\
		cp $${override} ${DESTDIR}${MUSHAREDIR}/overrides/$${override##*/}; \
		chmod 644 ${DESTDIR}${MUSHAREDIR}/overrides/$${override##*/}; done

uninstall:
	rm -rf ${DESTDIR}${BINDIR}/muw ${DESTDIR}${MUSHAREDIR}

.PHONY: all install uninstall

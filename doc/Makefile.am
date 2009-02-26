
cdoxdeps = Doxyfile

all: html/index.html

html/index.html: $(cdoxdeps)
	[ -x /usr/local/bin/doxygen-f ] && doxygen-f  $< || [ -x /usr/bin/doxygen ] && doxygen  $<


EXTRA_DIST = Doxyfile.in fpp mainpage.dox breve_presentazione.odp
#SUBDIRS=pictures

install-data-local:
	for pkg in html ; do \
		test -d $$pkg && \
		$(mkinstalldirs) $(DESTDIR)$(docdir)/$$pkg && \
		find $$pkg -type f -exec $(INSTALL_DATA) '{}' $(DESTDIR)$(docdir)/$$pkg/ \; ; \
	done || true

uninstall-local:
	for pkg in html ; do \
		rm -rf $(DESTDIR)$(docdir)/$${pkg}; \
	done || true

clean-local:
	rm -rf html


cdoxdeps = Doxyfile

if DOXYDOC

all: html/index.html

html/index.html: $(cdoxdeps)
	[ -x /usr/local/bin/doxygen-f ] && doxygen-f  $< || [ -x /usr/bin/doxygen ] && doxygen  $<

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

endif

EXTRA_DIST = Doxyfile.in fpp mainpage.dox footer.html \
 v7d_transform_tut.dox space_transform.dox \
 stat_proc_agg.png stat_proc_diff.png stat_proc_disagg_inst.png stat_proc_inst_agg.png \
 breve_presentazione.odp

#SUBDIRS=pictures


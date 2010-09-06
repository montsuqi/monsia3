## Process this file with automake to produce Makefile.in

SUBDIRS = icons

libgladeui = $(top_builddir)/gladeui/libgladeui-1.la


gladepanda_LTLIBRARIES = libgladepanda.la
gladepandadir = $(pkglibdir)/modules

libgladepanda_la_SOURCES = glade-panda.c
libgladepanda_la_CPPFLAGS = $(AM_CPPFLAGS)
libgladepanda_la_CFLAGS = \
        -DG_LOG_DOMAIN=\"PANDA\" \
	-I$(top_srcdir)                  \
	-I$(top_builddir)                \
	-I$(top_srcdir)/plugins/gtk+     \
	$(GTKPANDA2_CFLAGS)              \
	$(PLUGINS_WARN_CFLAGS)           \
	$(AM_CPPFLAGS)
libgladepanda_la_LDFLAGS = -module -avoid-version $(GTKPANDA2_LIBS) $(AM_LDFLAGS)
libgladepanda_la_LIBADD = $(libgladeui) $(GLADE_LIBS)

# catalog data

catalogsdir = $(pkgdatadir)/catalogs
catalogs_DATA = 					\
				panda.xml		\
				panda.xml.in
@INTLTOOL_XML_NOMERGE_RULE@


CLEANFILES = panda.xml

EXTRA_DIST = $(catalogs_DATA)

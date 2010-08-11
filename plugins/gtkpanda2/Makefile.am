## Process this file with automake to produce Makefile.in

SUBDIRS = icons

libgladeui = $(top_builddir)/gladeui/libgladeui-1.la


gladegtkpanda2_LTLIBRARIES = libgladegtkpanda2.la
gladegtkpanda2dir = $(pkglibdir)/modules

libgladegtkpanda2_la_SOURCES = glade-gtkpanda2.c
libgladegtkpanda2_la_CPPFLAGS = $(AM_CPPFLAGS)
libgladegtkpanda2_la_CFLAGS = \
        -DG_LOG_DOMAIN=\"GTKPANDA2\" \
	-I$(top_srcdir)                  \
	-I$(top_builddir)                \
	-I$(top_srcdir)/plugins/gtk+     \
	$(GTKPANDA2_CFLAGS)              \
	$(PLUGINS_WARN_CFLAGS)           \
	$(AM_CPPFLAGS)
libgladegtkpanda2_la_LDFLAGS = -module -avoid-version $(GTKPANDA2_LIBS) $(AM_LDFLAGS)
libgladegtkpanda2_la_LIBADD = $(libgladeui) $(GLADE_LIBS)

# catalog data

catalogsdir = $(pkgdatadir)/catalogs
catalogs_DATA = 					\
				gtkpanda2.xml		\
				gtkpanda2.xml.in
@INTLTOOL_XML_NOMERGE_RULE@


CLEANFILES = gtkpanda2.xml

EXTRA_DIST = $(catalogs_DATA)

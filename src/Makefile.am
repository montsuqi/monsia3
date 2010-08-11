## Process this file with automake to produce Makefile.in

bin_PROGRAMS = monsia3

monsia3_CPPFLAGS = \
	-I$(top_srcdir)    \
	-I$(top_builddir)  \
	-DGLADE_GNOMEHELPDIR="\"$(HELP_DIR)\""
	$(AM_CPPFLAGS)

monsia3_CFLAGS =           \
	$(GTK_CFLAGS)      \
	$(WARN_CFLAGS)     \
	$(AM_CFLAGS)

monsia3_LDFLAGS = $(AM_LDFLAGS)

monsia3_LDADD = $(top_builddir)/gladeui/libgladeui-1.la

monsia3_SOURCES = \
	glade-window.c \
	glade-window.h \
	main.c

if NATIVE_WIN32
monsia3_LDADD += glade-win32-res.o
if !GLADE_UNSTABLE
monsia3_LDFLAGS += -mwindows
endif
endif

glade-win32-res.o: glade-3.rc
	$(WINDRES) $< $@

EXTRA_DIST = glade-3.rc.in

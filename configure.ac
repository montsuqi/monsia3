dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.52)

m4_define(glade_major_version, 1)
m4_define(glade_minor_version, 0)
m4_define(glade_micro_version, 0)
m4_define(glade_version, glade_major_version.glade_minor_version.glade_micro_version)

AC_INIT([monsia3], [glade_version],
	[http://www.montsuqi.org/svn/monsia3])
	
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([gladeui/glade.h])
AC_CONFIG_MACRO_DIR([m4])

AC_DEFINE(GLADE_MAJOR_VERSION, glade_major_version, [Glade major version])
AC_SUBST(GLADE_MAJOR_VERSION, glade_major_version)
AC_DEFINE(GLADE_MINOR_VERSION, glade_minor_version, [Glade minor version])
AC_SUBST(GLADE_MINOR_VERSION, glade_minor_version)
AC_DEFINE(GLADE_MICRO_VERSION, glade_micro_version, [Glade micro version])
AC_SUBST(GLADE_MICRO_VERSION, glade_micro_version)

AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE([1.9 foreign no-dist-gzip dist-bzip2])

AM_MAINTAINER_MODE

IT_PROG_INTLTOOL([0.35.0])

AC_PATH_PROG(GLIB_GENMARSHAL, glib-genmarshal)
AC_PATH_PROG(DLLTOOL, dlltool)

AC_ISC_POSIX
AC_PROG_CC
AC_DISABLE_STATIC
AC_PROG_INSTALL
AC_PROG_MAKE_SET

AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AM_PROG_LIBTOOL

# If the source code has changed at all, increment GLADE_REVISION
# If any interfaces have been added, removed, or changed, increment GLADE_CURRENT, and set GLADE_REVISION to 0.
# If any interfaces have been added since the last public release, then increment GLADE_AGE.
# If any interfaces have been removed since the last public release, then set GLADE_AGE to 0.
# Reference: http://www.gnu.org/software/libtool/manual.html#Versioning
GLADE_REVISION=4
GLADE_CURRENT=7
GLADE_AGE=0
GLADE_CURRENT_MINUS_AGE=`expr $GLADE_CURRENT - $GLADE_AGE`
AC_SUBST(GLADE_REVISION)
AC_SUBST(GLADE_CURRENT)
AC_SUBST(GLADE_AGE)
AC_SUBST(GLADE_CURRENT_MINUS_AGE)

dnl are we unstable or stable?
m4_define([glade_unstable],
          m4_if(m4_eval(glade_minor_version % 2), [1], [yes], [no]))
GLADE_UNSTABLE=glade_unstable
AC_SUBST(GLADE_UNSTABLE)

dnl The symbol GLADE_UNSTABLE is defined above for substitution in
dnl Makefiles and conditionally defined here as a preprocessor symbol
dnl and automake conditional.
if test "x$GLADE_UNSTABLE" = "xyes"; then
  AC_DEFINE(GLADE_UNSTABLE, 1,
            [Define to 1 if this is an unstable version of Glade])
fi
AM_CONDITIONAL(GLADE_UNSTABLE, test "x$GLADE_UNSTABLE" = "xyes")

# ================================================================

dnl ================================================================
dnl Gettext stuff.
dnl ================================================================
GETTEXT_PACKAGE=AC_PACKAGE_NAME
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE, "$GETTEXT_PACKAGE", [Name of the gettext package.])
AM_GLIB_GNU_GETTEXT

dnl ================================================================
dnl va_copy.
dnl ================================================================
AC_MSG_CHECKING([how to copy va_list])
AC_TRY_LINK([#include <stdarg.h>], [va_list ap1, ap2; va_copy(ap1, ap2);],
        AC_MSG_RESULT([va_copy]),
        [ AH_TEMPLATE([va_copy], [define if va_copy is not available])
        AC_TRY_LINK([#include <stdarg.h>], [va_list ap1, ap2; __va_copy(ap1, ap2);],
            [ AC_DEFINE([va_copy], [__va_copy])
            AC_MSG_RESULT([__va_copy])],
            [ AC_DEFINE([va_copy(dest,src)], [memcpy(&dest,&src,sizeof(va_list))])
            AC_MSG_RESULT([memcpy])]
            )
        ])

dnl ================================================================
dnl Check for gtk-doc
dnl ================================================================
GTK_DOC_CHECK(1.4)

dnl ================================================================
dnl Check for gtk+
dnl ================================================================
PKG_CHECK_MODULES(GTK, [gtk+-2.0 >= 2.12.0  gthread-2.0 libxml-2.0 >= 2.4.0])
AC_SUBST(GTK_LIBS)
AC_SUBST(GTK_CFLAGS)

dnl ================================================================
dnl Check for the unix print widgets in gtk+
dnl ================================================================
LIBS=$GTK_LIBS
AC_CHECK_FUNCS(gtk_print_unix_dialog_new,[have_unix_print=yes]; break,[have_unix_print=no])
AM_CONDITIONAL(HAVE_GTK_UNIX_PRINT, test x"$have_unix_print" = "xyes")

dnl ================================================================
dnl Check for optional gtk+panda2 libs
dnl ================================================================
AC_ARG_ENABLE(gtkpanda2,
    AS_HELP_STRING([--disable-gtkpanda2], [disable gtk+panda2 catalog]),
    check_gtkpanda2=$enableval, check_gtkpanda2=yes)

if test x"$check_gtkpanda2" = x"yes"; then
  PKG_CHECK_MODULES(GTKPANDA2, [gtkpanda2],
    [have_gtkpanda2=yes],[have_gtkpanda2=no])
  AC_SUBST(GTKPANDA2_LIBS)
  AC_SUBST(GTKPANDA2_CFLAGS)
else
  have_gtkpanda2=no
fi

AM_CONDITIONAL(BUILD_GTKPANDA2, test x"$have_gtkpanda2" = "xyes")

dnl ================================================================
dnl Check for windows
dnl ================================================================
case $host_os in
  *mingw* | pw32* | cygwin*)
    platform_win32=yes
    ;;
  *)
    platform_win32=no
    ;;
esac
AM_CONDITIONAL(PLATFORM_WIN32, test x"$platform_win32" = "xyes")

case $host_os in
  *mingw*)
    native_win32=yes
    ;;
  *)
    native_win32=no
    ;;
esac
AM_CONDITIONAL(NATIVE_WIN32, test x"$native_win32" = "xyes")

if test "$native_win32" = "yes"; then
  AC_CHECK_TOOL(WINDRES, windres, no)
  if test "$WINDRES" = no; then
    AC_MSG_ERROR([*** Could not find an implementation of windres in your PATH.])
  fi
fi

AM_LDFLAGS=-g -Wl,-z,defs -Wl,-O1 -Wl,--as-needed

AC_SUBST([AM_CPPFLAGS])
AC_SUBST([AM_CFLAGS])
AC_SUBST([AM_CXXFLAGS])
AC_SUBST([AM_LDFLAGS])

AC_CONFIG_FILES([
Makefile
data/monsia3.desktop.in
data/Makefile
data/icons/Makefile
data/icons/hicolor/Makefile
data/icons/hicolor/16x16/Makefile
data/icons/hicolor/16x16/apps/Makefile
data/icons/hicolor/22x22/Makefile
data/icons/hicolor/22x22/apps/Makefile
data/icons/hicolor/24x24/Makefile
data/icons/hicolor/24x24/apps/Makefile
data/icons/hicolor/32x32/Makefile
data/icons/hicolor/32x32/apps/Makefile
data/icons/hicolor/48x48/Makefile
data/icons/hicolor/48x48/apps/Makefile
data/icons/hicolor/scalable/Makefile
data/icons/hicolor/scalable/apps/Makefile
gladeui/Makefile
src/Makefile
plugins/Makefile
plugins/gtk+/Makefile
plugins/gtk+/icons/Makefile
plugins/gtk+/icons/16x16/Makefile
plugins/gtk+/icons/22x22/Makefile
plugins/panda/Makefile
plugins/panda/icons/Makefile
plugins/panda/icons/16x16/Makefile
plugins/panda/icons/22x22/Makefile
po/Makefile.in
])

AC_OUTPUT

echo "

Configuration:

	Source code location:	 ${srcdir}
	Compiler:		 ${CC}
	GTK+ UNIX Print Widgets: ${have_unix_print}
	GTK+Panda2 Widgets:	 ${have_gtkpanda2}
"



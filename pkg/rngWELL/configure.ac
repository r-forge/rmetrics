#############################################################################
#   Copyright (c) 2023 Christophe Dutang and Petr Savicky                   #
#                                                                           #
#   This code can be used freely for personal, academic, or                 #
#   non-commercial purposes. For commercial purposes, please contact        #
#   P. L'Ecuyer at: lecuyer@iro.UMontreal.ca                                #
#                                                                           #
#                                                                           #
#############################################################################
### rng WELL package
###
###			'autoconf' config file 
### 

###
### we remove obsolete commands: 
### - AC_FD_MSG see change by autoupdate to AS_MESSAGE_FD, 
### - AC_TRY_COMPILE see https://www.gnu.org/software/autoconf/manual/autoconf-2.70/html_node/Obsolete-Macros.html, 
### - AC_CONFIG_HEADER see https://www.gnu.org/software/autoconf/manual/autoconf-2.70/html_node/Configuration-Headers.html#index-AC_005fCONFIG_005fHEADERS
### We add some commands 
### - AC_LANG_SOURCE, 
### - AC_LANG_PROGRAM, see https://lists.gnu.org/archive/html/bug-autoconf/2010-09/msg00092.html.
### - AC_LANG_CONFTEST, see https://www.gnu.org/software/autoconf/manual/autoconf-2.63/html_node/Generating-Sources.html 
### Help at https://www.gnu.org/software/autoconf/manual/
### good examples also in ff, RODBC and RDieHarder packages


### initialize
# see https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Initializing-configure.html
AC_INIT([rngWELL],[0.10-9],[christophe.dutang@ensimag.fr])

### set the CC variable to avoid searching for gcc
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_PROG_CC([cc cl gcc])

### check header (1st arg) => no longer use AC_CHECK_HEADER()
AC_CHECK_HEADERS([sys/time.h])

AC_CHECK_HEADERS([time.h])

AC_CHECK_HEADERS([windows.h])


### output config header file
AC_CONFIG_HEADERS([src/config.h])

AC_OUTPUT

### see gnu doc 
### http://www.gnu.org/software/autoconf/manual/autoconf-2.57/html_node/autoconf.html
### http://www.gnu.org/software/autoconf/manual/html_node/Autoconf-Language.html
###
### to check the config file, use 
### >autoreconf --warnings=all
### to make the config file, use in this order the 3 following commands in the top directory
### >autoheader
### >autoconf
### >./configure
### and open src/config.h
 
 
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
### the following macro is __NOT__ mine. see copyleft section to know the
### author, we remove obsolete commands: 
### - AC_FD_MSG see change by autoupdate to AS_MESSAGE_FD, 
### - AC_TRY_COMPILE see https://www.gnu.org/software/autoconf/manual/autoconf-2.70/html_node/Obsolete-Macros.html, 
### - AC_CONFIG_HEADER see https://www.gnu.org/software/autoconf/manual/autoconf-2.70/html_node/Configuration-Headers.html#index-AC_005fCONFIG_005fHEADERS
### We add some commands 
### - AC_LANG_SOURCE, 
### - AC_LANG_PROGRAM, see https://lists.gnu.org/archive/html/bug-autoconf/2010-09/msg00092.html.
### - AC_LANG_CONFTEST, see https://www.gnu.org/software/autoconf/manual/autoconf-2.63/html_node/Generating-Sources.html 
### Help at https://www.gnu.org/software/autoconf/manual/
### good examples also in RODBC and RDieHarder packages


### initialize
# see https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Initializing-configure.html
AC_INIT([rngWELL],[0.10-8],[christophe.dutang@ensimag.fr])

### check header (1st arg), preprocessor @DEFS@ (2nd arg) if found, nothing if not found (3rd arg) 
# see https://www.gnu.org/software/autoconf/manual/autoconf-2.61/html_node/Generic-Headers.html
AC_CHECK_HEADER([sys/time.h], 
		[AC_DEFINE(HAVE_SYS_TIME_H,1,[manual check for sys/time.h header. define to 1 if you know you have it.])])
	
AC_CHECK_HEADER([time.h], 
		[AC_DEFINE(HAVE_TIME_H,1,[manual check for time.h header. define to 1 if you know you have it.])])

AC_CHECK_HEADER([windows.h],  
		[AC_DEFINE(HAVE_WINDOWS_H,1,
[manual check for windows.h header. define to 1 if you know you have it. NB: you may not have both sys/time.h and windows.h headers.])])


### output config header file
AC_CONFIG_HEADERS([src/config.h])

AC_OUTPUT

### see gnu doc 
### http://www.gnu.org/software/autoconf/manual/autoconf-2.57/html_node/autoconf.html
### http://www.gnu.org/software/autoconf/manual/html_node/Autoconf-Language.html
###
### to check the config file, use 
### >autoreconf --warnings=obsolete
### to make the config file, use in this order the 3 following commands in the top directory
### >autoheader
### >autoconf
### >./configure
### and open src/config.h
 
 
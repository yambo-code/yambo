## Process this file with automake to produce Makefile.in

## Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.
##
## $Id: Makefile.am 7479 2011-02-18 11:33:08Z xavier $

ACLOCAL_AMFLAGS = -I m4
SUBDIRS = build src testsuite

EXTRA_DIST = ChangeLog PACKAGING TODO

DISTCLEANFILES = ChangeLog
CLEANFILES = *~ *.bak

.PHONY: ChangeLog

ChangeLog:
	(svn2cl -i -o .changelog.tmp \$(top_srcdir) && \
	mv .changelog.tmp ChangeLog) || (rm -f .changelog.tmp; touch ChangeLog; \
	echo 'svn2cl command not found: installing possibly empty changelog.' >&2)

dist-hook: ChangeLog

rpm: dist
#	@topdir=`rpmbuild --showrc | @GREP@ ': *_topdir' | cut -f 2`; \
#	mv -f $(PACKAGE)-$(VERSION).tar.gz $$topdir/SOURCES; \
#	cp build/$(PACKAGE).spec $$topdir/SPECS; \
#	rpmbuild -ba $$topdir/SPECS/$(PACKAGE).spec; \
#	echo "-------------------------------------------------------------"; \
#        echo "  RPMs are built, and are located under:"; \
#        echo "       $$topdir/RPMS/<arch>"; \
#        echo "       $$topdir/SRPMS"; \
#        echo "-------------------------------------------------------------"
##       scp $(RPM_DIR)/RPMS/*/libxc*.rpm marques@www.tddft.org:/server/www/tddft.org/programs/octopus/download/

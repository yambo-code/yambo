#! /bin/sh
#
#        Copyright (C) 2000-2017 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): HM, DS
#
# This file is distributed under the terms of the GNU
# General Public License. You can redistribute it and/or
# modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation;
# either version 2, or (at your option) any later version.
#
# This program is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
# MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
#
# Create git hooks to update yambo version and hash
#
# 1. "pre-commit" --> yambo_versions_update.tcsh h
#
cat <<EOF > .git/hooks/pre-commit
#!/bin/bash
sbin/yambo_versions_update.tcsh r
git add configure
git add include/version.inc
if [ -e config/version.m4 ]     ; then  git add config/version.m4 ;     fi
if [ -e config/version.m4_gpl ] ; then  git add config/version.m4_gpl ; fi
EOF
chmod +x .git/hooks/pre-commit
#
# 2. "Prepare commit msg"
#
cat <<EOF > .git/hooks/prepare-commit-msg
#!/bin/bash
SOB=\$(git var GIT_AUTHOR_IDENT | sed -n 's/^\(.*>\).*$/ \1/p')
echo " " >> \$1
case "\$2,\$3" in
  merge,)
    echo "Merge commit, configure check prepared"
    touch .check_configure
  ;;
  *)
    if [ "\$2" != "message" ]; then
      ./sbin/make_message.pl -p "\$SOB"
      cat commit.msg >> \$1
      rm commit.msg
    fi
  ;;
esac
EOF
chmod +x .git/hooks/prepare-commit-msg
#
# 3. "Post merge"
#
cat <<EOF > .git/hooks/post-merge
#!/bin/bash
if [ -f .check_configure ]; then
  echo "Post MERGE hook: Checking if configure was correctly updated"
  rm .check_configure
  echo "Regenerating configure after merge"
  cp configure configure_save
  autoconf configure.ac > configure
  if [ \$(diff configure configure_save | head -c 5) ]; then
    echo "configure automatically updated after merge"
    rm configure_save
    git commit -m "Automatic commit: configure regenerated after merge"  --no-edit
  else
    rm configure_save
    echo "configure did not need update after merge"
  fi
fi
EOF
chmod +x .git/hooks/post-merge
#
# 4. "Post commit"
#
cat <<EOF > .git/hooks/post-commit
#!/bin/bash
if [ -f .check_configure ]; then
  echo "Post MERGE hook: Checking if configure was correctly updated"
  rm .check_configure
  echo "Regenerating configure after merge"
  cp configure configure_save
  autoconf configure.ac > configure
  rm -fr autom4te.cache
  if [ \$(diff configure configure_save | head -c 5) ]; then
    echo "configure automatically updated after merge"
    rm configure_save
    git add configure
    git commit -m "Automatic commit: configure regenerated after merge"  --no-edit
  else
    rm configure_save
    echo "configure did not need update after merge"
  fi
fi
EOF
chmod +x .git/hooks/post-commit
#
# 5. git config
#
git config merge.keepTheirs.driver "cp -f %B %A"
git config merge.commit no
git config core.editor "vim"
git config pull.rebase false



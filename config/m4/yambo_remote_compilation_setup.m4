#
#        Copyright (C) 2000-2022 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): DS AM
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
# Folders
#
if ! test -d log      ; then mkdir log      ; fi
if ! test -d bin      ; then mkdir bin      ; fi
if ! test -d include  ; then mkdir include  ; fi
if ! test -d include/headers ; then mkdir include/headers ; fi
if ! test -d lib      ; then mkdir lib      ; fi
if ! test -d lib/bin  ; then mkdir lib/bin  ; fi
if   test -d include/system ; then rm -r include/system ; fi
if ! test -d include/system ; then mkdir include/system ; fi
#
# Remote compilation
#
if [[ "$compdir" != "$srcdir" ]] && [[ "$srcdir" != "." ]] ; then
 #
 cp     $srcdir/Makefile $compdir/
 cp -r  $srcdir/config   $compdir/ 
 #
 basecomp=$(basename $compdir)
 #
 rsync -az --exclude="$basecomp/"  --include='*/' --include='*.h.F'        --exclude='*' $srcdir/ $compdir
 rsync -az --exclude="$basecomp/"  --include='*/' --include='*.h'          --exclude='*' $srcdir/ $compdir
 rsync -az --exclude="$basecomp/"  --include='*/' --include='*.objects'    --exclude='*' $srcdir/ $compdir
 rsync -az --exclude="$basecomp/"  --include='*/' --include='*.dep'        --exclude='*' $srcdir/ $compdir
 rsync -az --exclude="$basecomp/"  --include='*/' --include='*Makefile.lo' --exclude='*' $srcdir/ $compdir
 rsync -az --exclude="$basecomp/"  --include='*/' --include='*Makefile*'   --exclude='*' $srcdir/lib $compdir/
 rsync -az --exclude="$basecomp/"  --include='*/' --include='*list*'       --exclude='*' $srcdir/lib $compdir/
 rsync -az --exclude="$basecomp/"  --include='*/' --include='*make*'       --exclude='*' $srcdir/lib $compdir/
 rsync -az --exclude="$basecomp/"  --include='*/' --include='*.tar.*'      --exclude='*' $srcdir/lib $compdir/
 #
 if test ! -d "$compdir/log"        ; then mkdir "$compdir/log"; fi
 if test ! -d "$compdir/lib/archive"; then mkdir "$compdir/lib/archive"; fi
 if test ! -d "$compdir/lib/config";  then mkdir "$compdir/lib/config"; fi
 cp -r  $srcdir/lib/archive/*         $compdir/lib/archive/
 cp     $srcdir/lib/config/*          $compdir/lib/config/
 #
fi

#
# License-Identifier: GPL
#
# Copyright (C) 2021 The Yambo Team
#
# Authors (see AUTHORS file for details): DS AM
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

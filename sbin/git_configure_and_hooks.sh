#! /bin/sh
#
# License-Identifier: GPL
#
# Copyright (C) 2016 The Yambo Team
#
# Authors (see AUTHORS file for details): HM DS
#
# Create git hooks to update yambo version and hash
#
# 1. "pre-commit" --> yambo_versions_update.tcsh h
#
cat <<EOF > .git/hooks/pre-commit
#!/bin/bash
sbin/yambo_versions_update.tcsh r
git add configure
if [ -e include/version/version.m4 ]     ; then  git add include/version/version.m4 ;     fi
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
    echo "Merge commit"
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
# 3. git config
#
git config merge.keepTheirs.driver "cp -f %B %A"
git config merge.commit no
git config core.editor "vim"
git config pull.rebase false

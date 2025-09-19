#/bin/zsh -l

make clean
make core

rm  /home/tgeirsson/apps/lumen-fork/bin/yambo

ln -s /home/tgeirsson/lumen-fork/bin/yambo ~/apps/lumne-fork/bin/yambo

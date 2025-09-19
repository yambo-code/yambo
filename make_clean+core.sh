#/bin/zsh -l

make clean
make core

rm  /home/tgeirsson/apps/lumen-fork/bin/yambo
rm  /home/tgeirsson/apps/lumen-fork/bin/p2y
rm  /home/tgeirsson/apps/lumen-fork/bin/a2y
rm  /home/tgeirsson/apps/lumen-fork/bin/c2y
rm  /home/tgeirsson/apps/lumen-fork/bin/ypp

ln -s /home/tgeirsson/lumen-fork/bin/yambo ~/apps/lumen-fork/bin/yambo
ln -s /home/tgeirsson/lumen-fork/bin/p2y   ~/apps/lumen-fork/bin/p2y
ln -s /home/tgeirsson/lumen-fork/bin/a2y   ~/apps/lumen-fork/bin/a2y
ln -s /home/tgeirsson/lumen-fork/bin/c2y   ~/apps/lumen-fork/bin/c2y
ln -s /home/tgeirsson/lumen-fork/bin/ypp   ~/apps/lumen-fork/bin/ypp

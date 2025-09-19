#/bin/zsh -l

make distclean
./yambo_intel_single_precission_omp_configure.sh
make core

ln -s /home/tgeirsson/lumen-fork/bin/yambo ~/apps/lumne-fork/bin/yambo
ln -s /home/tgeirsson/lumen-fork/bin/p2y   ~/apps/lumne-fork/bin/p2y
ln -s /home/tgeirsson/lumen-fork/bin/a2y   ~/apps/lumne-fork/bin/a2y
ln -s /home/tgeirsson/lumen-fork/bin/c2y   ~/apps/lumne-fork/bin/c2y
ln -s /home/tgeirsson/lumen-fork/bin/ypp   ~/apps/lumne-fork/bin/ypp

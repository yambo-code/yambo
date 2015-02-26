TARGET_DIR="write here the path to the yamb trunk"
TRUNK_DIR="write here the path to the yambo branch/gpl"
cd $TRUNK_DIR
make clean_all
rm -r $TARGET_DIR/*
cp -r $TRUNK_DIR/* $TARGET_DIR/
cd $TARGET_DIR
 ./sbin/yamboo_new.pl -p="KERR SURF YPP_SURF ELPH YPP_ELPH FFTW FFTSG OPENMP MPI"
 chmod +x delete.batch 
./delete.batch
svn status | grep -v libxc

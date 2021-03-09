
make -f Download


#
# Special procedure for driver
#
VERSION=0.0.1

rm -rf lib/yambo/driver
cp lib/archive/${VERSION}.tar.gz lib/yambo/
cd lib/yambo/
tar -xzf ${VERSION}.tar.gz
mv yambo-libraries-${VERSION}/* ./
rm -rf yambo-libraries-${VERSION}
rm ${VERSION}.tar.gz
cd -

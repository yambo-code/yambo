CLANG_FMT=clang-format
$CLANG_FMT -i -style=file \
      ../src/*/*.c ../include/driver/*.h ../include/headers/parser/*.h 

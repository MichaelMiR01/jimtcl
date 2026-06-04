./configure --host=i686-w64-mingw32  --shared LDFLAGS="-L. -ltcc"  CFLAGS="-static-libgcc -s -m32" --full --with-ext="tcc"
#./configure --host=i686-w64-mingw32  --shared LDFLAGS="-L. -ltcc"  CFLAGS="-static-libgcc -s -m32" --full --with-ext="tcc,tcc4tcl"
make
./configure  LDFLAGS="-Wl,-rpath=. -L. -ltcc" "CFLAGS= -s -O2" --full --with-ext=tcc
#./configure  LDFLAGS="-Wl,-rpath=. -L. -ltcc" "CFLAGS= -s -O2" --full --with-ext=tcc,tcc4tcl 
make
# cleanup
rm *.o
rm _*.c


# for tcc we use something like
#./configure --host=i686-w64-mingw32  --shared LDFLAGS="-L. -ltcc -rdynamic"  CFLAGS="-s -m32" --full --with-ext="tcc" CC=/host/data/tcl/depr/jimtcl-0.83/tcc.exe
#./configure  LDFLAGS="-Wl,-rpath=. -L. -ltcc" "CFLAGS= -s -O2" --full --with-ext=tcc CC=/host/data/tcl/depr/jimtcl-0.83/tcc

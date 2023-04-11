make clean
make emu EMU_CXX_EXTRA_FLAGS="-DFIRST_INST_ADDRESS=0x80000000" EMU_TRACE=1 -j8
./ssd.sh -r ./ready-to-run/all/

./build/emu -i ready-to-run/all/load-store-cpu-tests.bin --dump-wave -b 0 -e 50000
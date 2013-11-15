include ../makefile.inc

install: gtest json
	cp libjson/*.a ../lib
	cp gmock-gtest/*.a ../lib

gtest:
	$(MAKE) lib -C gmock-gtest

json:
	$(MAKE) lib -C libjson

clean:
	cd libjson && $(MAKE) clean
	cd gmock-gtest && $(MAKE) clean


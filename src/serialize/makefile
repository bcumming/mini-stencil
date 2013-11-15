include makefile.inc

default:
	$(MAKE) install -C libs
	$(MAKE) install -C src
	$(MAKE) install -C test

clean:
	$(MAKE) clean -C libs
	$(MAKE) clean -C src
	$(MAKE) clean -C test
	rm -rf lib/*
	rm -rf bin/*


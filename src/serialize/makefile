include options

default:
	$(MAKE) install -C libs
	$(MAKE) install -C src
	$(MAKE) install -C test

clean:
	$(MAKE) clean -C libs
	$(MAKE) clean -C src
	$(MAKE) clean -C test
	rm -rf lib/*.a
	rm -rf bin/*.json bin/*.dat


TESTDIR = test
TESTTARGET = parsing_test

MAIN = main

SRCDIR  = src

DEFAULT = byte


flag =-Is $(TESTDIR),$(SRCDIR) -use-menhir -use-ocamlfind

main:
	corebuild $(flag) $(MAIN).native


native:
	corebuild $(flag) $(TESTDIR)/$(TARGET).native

byte:
	corebuild $(flag) $(SRCDIR),$(TESTDIR) $(TESTDIR)/$(TARGET).byte

cleanall:
	corebuild -clean; rm *.native; rm *.byte

clean:
	rm *.native; rm *.byte



TESTDIR = test
TESTTARGET = parsing_test

MAIN = main

SRCDIR  = src

DEFAULT = byte

BUILD = corebuild


flag =-Is $(TESTDIR),$(SRCDIR) -use-menhir -use-ocamlfind

main:
	$(BUILD) $(flag) $(MAIN).native


native:
	$(BUILD) $(flag) $(TESTDIR)/$(TARGET).native

byte:
	$(BUILD) $(flag) $(SRCDIR),$(TESTDIR) $(TESTDIR)/$(TARGET).byte

cleanall:
	$(BUILD) -clean; rm *.native; rm *.byte

clean:
	rm *.native; rm *.byte



TARGET = parsing_test

#SRCDIR  = src
TESTDIR = test

DEFAULT = byte


flag =-Is $(TESTDIR) -use-menhir -use-ocamlfind

native:
	corebuild $(flag) $(TESTDIR)/$(TARGET).native

byte:
	corebuild $(flag) $(SRCDIR),$(TESTDIR) $(TESTDIR)/$(TARGET).byte

clean:
	corebuild -clean; rm *.native; rm *.byte



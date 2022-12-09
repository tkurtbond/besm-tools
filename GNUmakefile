ifndef CSC
CSC=csc
endif

BINDIR=$(HOME)/local/bin

INSTALL_PROGRAMS=besm-totals
OTHER_PROGRAMS=
PROGRAMS=$(INSTALL_PROGRAMS:%=build/%$(EXE)) $(OTHER_PROGRAMS:%=build/%$(EXE))

all: build $(PROGRAMS)

build:
	mkdir build

clean:
	-rm -v $(PROGRAMS)

install: $(foreach e,$(PROGRAMS:%=%$(EXE)),$(BINDIR)/$(notdir $(e)))

build/% : %.scm
	$(CSC) $(CSCFLAGS) -o $@ $^

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	cp $< $@


ifndef CSC
CSC=csc
endif

BINDIR=$(HOME)/local/bin

INSTALL_PROGRAMS=besm-totals
OTHER_PROGRAMS=
PROGRAMS=$(INSTALL_PROGRAMS:%=build/%$(EXE)) $(OTHER_PROGRAMS:%=build/%$(EXE))

all: build $(PROGRAMS)

test: $(PROGRAMS) \
	build/FV2021-Coleopteran.gen.rst build/FV2021-Coleopteran.stmt.ms.pdf

native: $(PROGRAMS) \
	build/FV2021-Coleopteran.native

ms: $(PROGRAMS) \
	build/FV2021-Coleopteran.stmt.ms

clean:
	-rm -v $(PROGRAMS) build/*.gen.rst build/*.stmt.ms.pdf build/*.native \
		build/*.stmt.ms

install: $(foreach e,$(PROGRAMS:%=%$(EXE)),$(BINDIR)/$(notdir $(e)))


# .INTERMEDIATE: $(wildcard build/*.gen.rst)

build/%.gen.rst : test-data/%.dat
	build/besm-totals $(BTOPTS) $< >$@

build/%.stmt.ms.pdf : build/%.gen.rst
	pandoc -r rst -w ms --template=statement \
		--pdf-engine-opt=-P-p8.5i,5.5i \
		-o $@ $<

build/%.stmt.ms : build/%.gen.rst
	pandoc -r rst -w ms --template=statement \
		-o $@ $<

build/%.native : build/%.gen.rst
	pandoc -r rst -w native -o $@ $<

build:
	mkdir build

build/% : %.scm
	$(CSC) $(CSCFLAGS) -o $@ $^

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	cp $< $@


ifndef CSC
CSC=$(shell type -p csc || type -p chicken-csc || echo 'echo "CSC does not exist; "')
endif

BROPTS=

# besm-totals is retired.
INSTALL_PROGRAMS=besm-rst
OTHER_PROGRAMS=
PROGRAMS=$(INSTALL_PROGRAMS:%=build/%$(EXE)) $(OTHER_PROGRAMS:%=build/%$(EXE))
TESTDATA=$(wildcard test-data/*.yaml)
TESTOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .gen.rst,$(basename $(f) .yaml)))
STMTOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .stmt.ms.pdf,$(basename $(f) .yaml)))
LETTEROUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .ms.pdf,$(basename $(f) .yaml)))
MSOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .stmt.ms,$(basename $(f) .yaml)))
NATIVEOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .native,$(basename $(f) .yaml)))
HTMLOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .html,$(basename $(f) .yaml)))

all: build $(PROGRAMS)

$(wildcard build/*.gen.rst): build/besm-rst

test:	build/besm-rst \
	$(TESTOUTPUT)

stmt:	test $(STMTOUTPUT)

letter: test $(LETTEROUTPUT)

native: test $(NATIVEOUTPUT)

ms: test $(MSOUTPUT)

html: test $(HTMLOUTPUT)

clean:
	-rm -v $(PROGRAMS) build/*.gen.rst build/*.stmt.ms.pdf build/*.native \
		build/*.stmt.ms build/*.html

BINDIR=$(HOME)/local/bin
install: $(foreach e,$(PROGRAMS:%=%$(EXE)),$(BINDIR)/$(notdir $(e)))


#??? .INTERMEDIATE: $(wildcard build/*.gen.rst)

#build/%.gen.rst : test-data/%.dat
#	build/besm-rst $(BROPTS) $< >$@

build/%.gen.rst : test-data/%.yaml
	build/besm-rst $(BROPTS) $< >$@

build/%.ms.pdf : build/%.gen.rst
	pandoc -r rst -w ms --template=tkb -V twocol -o $@ $<

build/%.stmt.ms.pdf : build/%.gen.rst
	pandoc -r rst -w ms --template=statement \
		--pdf-engine-opt=-P-p8.5i,5.5i \
		-o $@ $<

build/%.stmt.ms : build/%.gen.rst
	pandoc -r rst -w ms --template=statement \
		-o $@ $<

build/%.native : build/%.gen.rst
	pandoc -r rst -w native -o $@ $<

build/%.html : build/%.gen.rst
	pandoc -s -r rst -w html -o $@ $<

build:
	mkdir build

build/% : %.scm
	$(CSC) $(CSCFLAGS) -o $@ $^

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	cp $< $@


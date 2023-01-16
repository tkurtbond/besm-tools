ifndef CSC
CSC=$(shell type -p csc || type -p chicken-csc || echo 'echo "csc does not exist; "')
endif

BROPTS=

# besm-totals is retired.
INSTALL_PROGRAMS=besm-rst
OTHER_PROGRAMS=
PROGRAMS=$(INSTALL_PROGRAMS:%=build/%$(EXE)) $(OTHER_PROGRAMS:%=build/%$(EXE))

TESTDATA=$(wildcard test-data/*.yaml)
TESTOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .gen.rst,$(basename $(f) .yaml)))
TESTTERSEOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix -terse.gen.rst,$(basename $(f) .yaml)))
STMTOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .stmt.ms.pdf,$(basename $(f) .yaml)))
LETTEROUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .ms.pdf,$(basename $(f) .yaml)))
MSOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .stmt.ms,$(basename $(f) .yaml)))
NATIVEOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .native,$(basename $(f) .yaml)))
HTMLOUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .html,$(basename $(f) .yaml)))
YAMLERROUTPUT=$(foreach f,$(notdir $(TESTDATA)),build/$(addsuffix .yamlerr,$(basename $(f) .yaml)))

all: build $(PROGRAMS)

$(wildcard build/*.gen.rst): build/besm-rst

# Note: enyon-boase.yaml has attributes, defects, and skills out of order,
# for testing sorting.

test:	build/besm-rst \
	$(TESTOUTPUT) $(TESTTERSEOUTPUT)

RPBHENTITIES=FV2021-Coleopteran enyon-boase pawl-cardynham nessa-kitto
RPBHGENRST=$(foreach e,$(RPBHENTITIES),build/$(addsuffix .gen.rst,$(e)))
$(RPBHGENRST): BROPTS+=-s

stmt:	test $(STMTOUTPUT)

letter: test $(LETTEROUTPUT)

native: test $(NATIVEOUTPUT)

ms: test $(MSOUTPUT)

html: test $(HTMLOUTPUT)

yamlerr: $(YAMLERROUTPUT)

clean:
	-rm -v $(PROGRAMS) build/*.gen.rst build/*.stmt.ms.pdf build/*.native \
		build/*.stmt.ms build/*.html build/*.yamlerr

BINDIR=$(HOME)/local/bin
install: $(foreach e,$(PROGRAMS:%=%$(EXE)),$(BINDIR)/$(notdir $(e)))


#??? .INTERMEDIATE: $(wildcard build/*.gen.rst)

#build/%.gen.rst : test-data/%.dat
#	build/besm-rst $(BROPTS) $< >$@

build/%.gen.rst : test-data/%.yaml
	build/besm-rst $(BROPTS) $< >$@

build/%-terse.gen.rst : test-data/%.yaml
	build/besm-rst -t $(BROPTS) $< >$@

build/%.yamlerr : test-data/%.yaml
	yamllint -f parsable  $< | tee $@

build/%.ms.pdf : build/%.gen.rst
	pandoc -r rst -w ms --template=tkb -V twocolumns -o $@ $<

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


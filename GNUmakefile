SHELL=bash
ifndef CSC
CSC=$(shell type -p csc || type -p chicken-csc || echo 'echo "csc does not exist; "')
endif

BROPTS=
BR2EOPTS=

# besm-totals is retired.
INSTALL_PROGRAMS=besm4-rst besm2-rst
OTHER_PROGRAMS=
PROGRAMS=$(INSTALL_PROGRAMS:%=build/%$(EXE)) $(OTHER_PROGRAMS:%=build/%$(EXE))

TEST_DATA=$(wildcard test-data/*.yaml)
TEST_OUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .gen.rst,$(basename $(f) .yaml)))
TEST_TERSEOUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -terse.gen.rst,$(basename $(f) .yaml)))
TEST_TBLOUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.gen.rst,$(basename $(f) .yaml)))
TEST_STMTOUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .stmt.ms.pdf,$(basename $(f) .yaml))) $(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.stmt.ms.pdf,$(basename $(f) .yaml))) $(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -terse.stmt.ms.pdf,$(basename $(f) .yaml)))
TEST_LETTEROUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .ms.pdf,$(basename $(f) .yaml))) $(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.ms.pdf,$(basename $(f) .yaml)))
TEST_TERSELETTEROUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -terse.ms.pdf,$(basename $(f) .yaml)))
TEST_TBLOUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .ms,$(basename $(f) .yaml))) $(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.ms,$(basename $(f) .yaml)))
TEST_NATIVEOUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .native,$(basename $(f) .yaml))) $(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.native,$(basename $(f) .yaml)))
TEST_HTMLOUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .html,$(basename $(f) .yaml))) $(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.html,$(basename $(f) .yaml)))
TEST_YAMLERROUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .yamlerr,$(basename $(f) .yaml)))

all: $(PROGRAMS)

$(wildcard build/*.gen.rst): build/besm4-rst

# Note: enyon-boase.yaml has attributes, defects, and skills out of order,
# for testing sorting.

test:	build/besm4-rst build/besm2-rst \
	$(TEST_OUTPUT) $(TEST_TERSEOUTPUT) $(TEST_TBLOUTPUT)

echo:
	echo $(TEST_OUTPUT) $(TEST_TERSEOUTPUT) $(TEST_TBLOUTPUT)

RPBHENTITIES=FV2021-Coleopteran-4e enyon-boase-4e pawl-cardynham-4e nessa-kitto-4e
RPBHGENRST=$(foreach e,$(RPBHENTITIES),build/$(addsuffix .gen.rst,$(e)))
$(RPBHGENRST): BROPTS+=-s

stmt:	test $(TEST_TBLSTMTOUTPUT) $(TEST_STMTOUTPUT)

letter: test $(TEST_LETTEROUTPUT)

terseletter: test $(TEST_TERSELETTEROUTPUT)

native: test $(TEST_NATIVEOUTPUT)

tbl: test $(TEST_TBLOUTPUT)

html: test $(TEST_HTMLOUTPUT)

yamlerr: $(TEST_YAMLERROUTPUT)

clean: testclean
	-rm -v $(PROGRAMS)
testclean:
	-rm -v	build/*.gen.rst build/*.ms.pdf \
		build/*.native build/*.ms \
		build/*.html build/*.yamlerr

BINDIR=$(HOME)/local/bin
install: $(foreach e,$(PROGRAMS:%=%$(EXE)),$(BINDIR)/$(notdir $(e)))


#??? .INTERMEDIATE: $(wildcard build/*.gen.rst)

#build/%.gen.rst : test-data/%.dat
#	build/besm4-rst $(BROPTS) $< >$@

build/%-4e.gen.rst : test-data/%-4e.yaml build/besm4-rst
	build/besm4-rst -s $(BROPTS) $< >$@

build/%-4e-terse.gen.rst : test-data/%-4e.yaml build/besm4-rst
	build/besm4-rst -s -t $(BROPTS) $< >$@ # terse

build/%-4e-tbl.gen.rst : test-data/%-4e.yaml build/besm4-rst
	build/besm4-rst -s -m $(BROPTS) $< >$@ # ms tables

build/%-2e.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s $(BR2EOPTS) $< >$@

build/%-2e-terse.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s -t $(BR2EOPTS) $< >$@ # terse

build/%-2e-tbl.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s -m $(BR2EOPTS) $< >$@ # ms tables

build/%.yamlerr : test-data/%.yaml
	yamllint -f parsable  $< | tee $@

#MS_COLUMNS=-V twocolumns
build/%.ms.pdf : build/%.gen.rst
	pandoc -r rst -w ms --template=tkb $(MS_COLUMNS) -o $@ $<

build/%.ms : build/%.gen.rst
	pandoc -r rst -w ms --template=tkb $(MS_COLUMNS) -o $@ $<

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

build/% : %.scm
	$(CSC) $(CSCFLAGS) -o $@ $^

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	cp $< $@

.PRECIOUS: %.gen.rst

print-%  : ; @echo $* = $($*)

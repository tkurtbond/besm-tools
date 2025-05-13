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

# This is the list of generated reST files using reST tables.
TEST_OUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .gen.rst,$(basename $(f) .yaml)))

# This is the list of generated reST files using terse output.
TEST_TERSEOUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -terse.gen.rst,$(basename $(f) .yaml)))

# This is the list  of generated reST files using TBL tables in a raw block.
TEST_TBLOUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.gen.rst,$(basename $(f) .yaml)))

# This is the  list of statement-sized PDFs produced from reST tables, TBL tables, and terse mode.
TEST_STMTOUTPUT=\
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .stmt.ms.pdf,$(basename $(f) .yaml))) \
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.stmt.ms.pdf,$(basename $(f) .yaml))) \
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -terse.stmt.ms.pdf,$(basename $(f) .yaml)))

# This is the  list of letter-sized PDFs produced from reST tables, TBL tables, and terse mode.
TEST_LETTEROUTPUT=\
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .ms.pdf,$(basename $(f) .yaml))) \
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.ms.pdf,$(basename $(f) .yaml))) \
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -terse.ms.pdf,$(basename $(f) .yaml)))

# This is the the terse letter output separate.
TEST_TERSELETTEROUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -terse.ms.pdf,$(basename $(f) .yaml)))

# This is the .ms files for output using reST tables and TBL in raw blocks.
TEST_TBLOUTPUT=\
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .ms,$(basename $(f) .yaml))) \
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.ms,$(basename $(f) .yaml)))

# This is the native output of pandoc, for debugging, for reST tables TBL in raw blocks, and terse output.
TEST_NATIVEOUTPUT=\
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .native,$(basename $(f) .yaml))) \
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -tbl.native,$(basename $(f) .yaml))) \
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -terse.native,$(basename $(f) .yaml)))

# This is HTML output.
TEST_HTMLOUTPUT=\
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .html,$(basename $(f) .yaml))) \
	$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix -terse.html,$(basename $(f) .yaml)))

# This is the output of running yamllint on each of the YAML files.
TEST_YAMLERROUTPUT=$(foreach f,$(notdir $(TEST_DATA)),build/$(addsuffix .yamlerr,$(basename $(f) .yaml)))

all: $(PROGRAMS)

$(wildcard build/*-4e*.gen.rst): build/besm4-rst
$(wildcard build/*-2e*.gen.rst): build/besm2-rst

# Note: enyon-boase.yaml has attributes, defects, and skills out of order,
# for testing sorting.

rst: build/besm4-rst build/besm2-rst \
	$(TEST_OUTPUT) $(TEST_TERSEOUTPUT) $(TEST_TBLOUTPUT)

# What is this doing?
RPBHENTITIES=FV2021-Coleopteran-4e enyon-boase-4e pawl-cardynham-4e nessa-kitto-4e
RPBHGENRST=$(foreach e,$(RPBHENTITIES),build/$(addsuffix .gen.rst,$(e)))
$(RPBHGENRST): BROPTS+=-s

test: stmt letter native

stmt: rst $(TEST_STMTOUTPUT)

letter: rst $(TEST_LETTEROUTPUT)

# terseletter: rst $(TEST_TERSELETTEROUTPUT)

native: rst $(TEST_NATIVEOUTPUT)

tbl: rst $(TEST_TBLOUTPUT)

html: rst $(TEST_HTMLOUTPUT)

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

build/%-terse.html : build/%-terse.gen.rst
	pandoc -s -r rst -w html -o $@ $<


build/% : %.scm
	$(CSC) $(CSCFLAGS) -o $@ $^

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	cp $< $@

.PRECIOUS: \
	build/%-4e.gen.rst build/%-4e-terse.gen.rst build/%-4e-tbl.gen.rst \
	build/%-2e.gen.rst build/%-2e-terse.gen.rst build/%-2e-tbl.gen.rst

print-%  : ; @echo $* = $($*)

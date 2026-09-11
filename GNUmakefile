SHELL=bash
ifndef CSC
CSC=$(shell type -p csc || type -p chicken-csc || echo 'echo "csc does not exist; "')
endif

BROPTS=
BR2EOPTS=

# Number of repetitions of besm2-rst run by benchmark-fyaml, for each
# of the yaml egg and the slibfyaml egg (-f/--fyaml).
BENCH_N=20

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

# 2E-only: -n/--unicode-minus isn't implemented in besm4-rst.scm (yet),
# so these variants -- which render negative numbers (defect points,
# and enhancement/limiter signs) with Unicode MINUS SIGN instead of
# ASCII hyphen-minus, for comparison against the default output above
# -- only exist for the 2E test data.
TEST_DATA_2E=$(filter %-2e.yaml,$(TEST_DATA))

# This is the list of generated reST files using reST tables, with
# Unicode MINUS SIGN instead of ASCII hyphen-minus for negative numbers.
TEST_UNICODE_MINUS_OUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -unicode-minus.gen.rst,$(basename $(f) .yaml)))

# This is the list of generated reST files using TBL tables in a raw
# block, with Unicode MINUS SIGN instead of ASCII hyphen-minus for
# negative numbers.
TEST_UNICODE_MINUS_TBLOUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -tbl-unicode-minus.gen.rst,$(basename $(f) .yaml)))

# The letter-sized PDFs for the two Unicode-MINUS-SIGN variants above,
# for comparing side by side against their hyphen-minus counterparts
# in TEST_LETTEROUTPUT.
TEST_UNICODE_MINUS_LETTEROUTPUT=\
	$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -unicode-minus.ms.pdf,$(basename $(f) .yaml))) \
	$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -tbl-unicode-minus.ms.pdf,$(basename $(f) .yaml)))

# besm2-rst can load YAML using either the yaml egg (the default) or
# the slibfyaml egg (-f/--fyaml). The lists and rules below build
# every besm2-rst reST variant (plain, terse, TBL, and the
# Unicode-MINUS-SIGN variants) a second time using -f, with "-fyaml"
# appended to the file name, so the two eggs' output can be diffed
# (see compare-fyaml) and their speed compared (see benchmark-fyaml).
# 2E-only, since besm2-rst only handles 2E test data.
ENTITY_NAMES_2E=$(basename $(notdir $(TEST_DATA_2E)) .yaml)

TEST_FYAML_OUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -fyaml.gen.rst,$(basename $(f) .yaml)))
TEST_TERSEFYAMLOUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -terse-fyaml.gen.rst,$(basename $(f) .yaml)))
TEST_TBLFYAMLOUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -tbl-fyaml.gen.rst,$(basename $(f) .yaml)))
TEST_UNICODE_MINUS_FYAMLOUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -unicode-minus-fyaml.gen.rst,$(basename $(f) .yaml)))
TEST_UNICODE_MINUS_TBLFYAMLOUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -tbl-unicode-minus-fyaml.gen.rst,$(basename $(f) .yaml)))

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

test: stmt letter native html

stmt: rst $(TEST_STMTOUTPUT)

letter: rst $(TEST_LETTEROUTPUT)

# terseletter: rst $(TEST_TERSELETTEROUTPUT)

native: rst $(TEST_NATIVEOUTPUT)

tbl: rst $(TEST_TBLOUTPUT)

html: rst $(TEST_HTMLOUTPUT)

# The Unicode-MINUS-SIGN comparison variants (2E only -- see
# TEST_DATA_2E above), plus their letter-sized PDFs.
unicode-minus: rst \
	$(TEST_UNICODE_MINUS_OUTPUT) $(TEST_UNICODE_MINUS_TBLOUTPUT) \
	$(TEST_UNICODE_MINUS_LETTEROUTPUT)

# Every besm2-rst variant (plain, terse, TBL, and the Unicode-MINUS-SIGN
# variants), built with the slibfyaml egg (-f/--fyaml) instead of the
# yaml egg, for comparison against the default output built by "rst"
# and "unicode-minus".
fyaml: rst unicode-minus \
	$(TEST_FYAML_OUTPUT) $(TEST_TERSEFYAMLOUTPUT) $(TEST_TBLFYAMLOUTPUT) \
	$(TEST_UNICODE_MINUS_FYAMLOUTPUT) $(TEST_UNICODE_MINUS_TBLFYAMLOUTPUT)

# Diff each yaml-egg reST file against its slibfyaml-egg counterpart
# and report which pairs match and which differ.
compare-fyaml: fyaml
	@status=0; \
	for base in $(ENTITY_NAMES_2E); do \
		for suf in .gen.rst -terse.gen.rst -tbl.gen.rst \
			   -unicode-minus.gen.rst -tbl-unicode-minus.gen.rst; do \
			yamlf=build/$$base$$suf; \
			fyamlf=build/$$base$${suf%.gen.rst}-fyaml.gen.rst; \
			if diff -q $$yamlf $$fyamlf >/dev/null 2>&1; then \
				echo "MATCH:   $$yamlf == $$fyamlf"; \
			else \
				echo "DIFFER:  $$yamlf != $$fyamlf"; \
				diff -u $$yamlf $$fyamlf; \
				status=1; \
			fi; \
		done; \
	done; \
	exit $$status

# Compare wall-clock/user/sys time for besm2-rst run BENCH_N times with
# the yaml egg vs. the slibfyaml egg (-f/--fyaml), on each 2E test file.
# Override BENCH_N=n on the command line to change the repetition count.
benchmark-fyaml: build/besm2-rst
	@TIMEFORMAT='  %3lR real  %3lU user  %3lS sys'; \
	for f in $(TEST_DATA_2E); do \
		echo "=== $$f ($(BENCH_N) runs) ==="; \
		echo "--- yaml egg ---"; \
		time ( for i in $$(seq 1 $(BENCH_N)); do \
			build/besm2-rst -s $(BR2EOPTS) $$f >/dev/null; \
		done ); \
		echo "--- slibfyaml egg (-f/--fyaml) ---"; \
		time ( for i in $$(seq 1 $(BENCH_N)); do \
			build/besm2-rst -s -f $(BR2EOPTS) $$f >/dev/null; \
		done ); \
	done

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

build/%-2e-unicode-minus.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s -n $(BR2EOPTS) $< >$@ # unicode minus sign

build/%-2e-tbl-unicode-minus.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s -m -n $(BR2EOPTS) $< >$@ # ms tables, unicode minus sign

# The same variants as above, but loading the YAML with the slibfyaml
# egg (-f/--fyaml) instead of the yaml egg, for comparison.
build/%-2e-fyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s -f $(BR2EOPTS) $< >$@ # fyaml egg

build/%-2e-terse-fyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s -t -f $(BR2EOPTS) $< >$@ # terse, fyaml egg

build/%-2e-tbl-fyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s -m -f $(BR2EOPTS) $< >$@ # ms tables, fyaml egg

build/%-2e-unicode-minus-fyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s -n -f $(BR2EOPTS) $< >$@ # unicode minus sign, fyaml egg

build/%-2e-tbl-unicode-minus-fyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst
	build/besm2-rst -s -m -n -f $(BR2EOPTS) $< >$@ # ms tables, unicode minus sign, fyaml egg

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
	build/%-2e.gen.rst build/%-2e-terse.gen.rst build/%-2e-tbl.gen.rst \
	build/%-2e-unicode-minus.gen.rst build/%-2e-tbl-unicode-minus.gen.rst \
	build/%-2e-fyaml.gen.rst build/%-2e-terse-fyaml.gen.rst \
	build/%-2e-tbl-fyaml.gen.rst build/%-2e-unicode-minus-fyaml.gen.rst \
	build/%-2e-tbl-unicode-minus-fyaml.gen.rst

print-%  : ; @echo $* = $($*)

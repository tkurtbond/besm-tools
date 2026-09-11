SHELL=bash
ifndef CSC
CSC=$(shell type -p csc || type -p chicken-csc || echo 'echo "csc does not exist; "')
endif

BROPTS=
BR2EOPTS=

# Number of repetitions of besm2-rst/besm2-rst-f run by
# benchmark-fyaml on each *small* test-data file, for each of the
# three programs compared (yaml egg, slibfyaml egg -f/--fyaml, and
# besm2-rst-f). Note: build/benchmark-fyaml.out only reruns when its
# file prerequisites are newer than it (see below), so changing
# BENCH_N/BENCHMARK_LARGE_RUNS alone, with no file touched, will not
# by itself trigger a rerun -- force one with
# "rm -f build/benchmark-fyaml.out && make benchmark-fyaml" or
# "make -B benchmark-fyaml".
BENCH_N=20

# besm-totals is retired.
INSTALL_PROGRAMS=besm4-rst besm2-rst
# besm2-rst-f is besm2-rst refactored to load YAML through slibfyaml's
# handle/tree-based document API instead of materializing it up front
# -- a comparison variant (see treefyaml/compare-treefyaml below), not
# something to install alongside the two programs above.
OTHER_PROGRAMS=besm2-rst-f
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

# Synthetic large 2E YAML files used only by benchmark-fyaml, to see
# whether the yaml-egg/slibfyaml-egg(-f)/besm2-rst-f performance gap
# widens with input size: each repeats the single entity in
# enyon-boase-2e.yaml the given number of times. Generated on demand
# into build/, not kept in test-data/, since they exist purely to
# stress-test parsing speed, not as realistic character data.
BENCHMARK_LARGE_SIZES=1 100 500 1500
# Repetition counts paired positionally with BENCHMARK_LARGE_SIZES
# above -- fewer runs for the larger, slower files, so the whole sweep
# finishes in a reasonable time. Change both lists together.
BENCHMARK_LARGE_RUNS=200 10 5 3
BENCHMARK_LARGE_FILES=$(BENCHMARK_LARGE_SIZES:%=build/synthetic-%.yaml)

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

# The same five variants again, this time built by besm2-rst-f (loads
# YAML via slibfyaml's handle/tree-based document API -- (slibfyaml
# documents) + (slibfyaml nodes) -- rather than materializing it into
# alists up front, unlike either besm2-rst's default yaml-egg path or
# its own -f/--fyaml slibfyaml-scheme path above), with "-treefyaml"
# appended to the file name, so its output can be diffed against the
# plain yaml-egg output (see compare-treefyaml). 2E-only, since
# besm2-rst-f only handles 2E test data, same as besm2-rst itself.
TEST_TREEFYAML_OUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -treefyaml.gen.rst,$(basename $(f) .yaml)))
TEST_TERSETREEFYAMLOUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -terse-treefyaml.gen.rst,$(basename $(f) .yaml)))
TEST_TBLTREEFYAMLOUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -tbl-treefyaml.gen.rst,$(basename $(f) .yaml)))
TEST_UNICODE_MINUS_TREEFYAMLOUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -unicode-minus-treefyaml.gen.rst,$(basename $(f) .yaml)))
TEST_UNICODE_MINUS_TBLTREEFYAMLOUTPUT=$(foreach f,$(notdir $(TEST_DATA_2E)),build/$(addsuffix -tbl-unicode-minus-treefyaml.gen.rst,$(basename $(f) .yaml)))

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

# Every besm2-rst-f variant (plain, terse, TBL, and the
# Unicode-MINUS-SIGN variants), built with besm2-rst-f -- which loads
# YAML through slibfyaml's handle/tree-based document API instead of
# materializing it up front -- for comparison against the default
# yaml-egg output built by "rst" and "unicode-minus" (see
# compare-treefyaml).
treefyaml: rst unicode-minus \
	$(TEST_TREEFYAML_OUTPUT) $(TEST_TERSETREEFYAMLOUTPUT) $(TEST_TBLTREEFYAMLOUTPUT) \
	$(TEST_UNICODE_MINUS_TREEFYAMLOUTPUT) $(TEST_UNICODE_MINUS_TBLTREEFYAMLOUTPUT)

# Diff each yaml-egg reST file against its besm2-rst-f (handle/tree
# API) counterpart and report which pairs match and which differ.
compare-treefyaml: treefyaml
	@status=0; \
	for base in $(ENTITY_NAMES_2E); do \
		for suf in .gen.rst -terse.gen.rst -tbl.gen.rst \
			   -unicode-minus.gen.rst -tbl-unicode-minus.gen.rst; do \
			yamlf=build/$$base$$suf; \
			treefyamlf=build/$$base$${suf%.gen.rst}-treefyaml.gen.rst; \
			if diff -q $$yamlf $$treefyamlf >/dev/null 2>&1; then \
				echo "MATCH:   $$yamlf == $$treefyamlf"; \
			else \
				echo "DIFFER:  $$yamlf != $$treefyamlf"; \
				diff -u $$yamlf $$treefyamlf; \
				status=1; \
			fi; \
		done; \
	done; \
	exit $$status

# Convenience alias for the log below -- kept as a separate name (with
# no recipe of its own) so "make benchmark-fyaml" still works as
# before; it just now only reruns build/benchmark-fyaml.out's recipe
# when that file's own prerequisites say it's stale (see BENCH_N's
# comment above for how to force a rerun otherwise).
benchmark-fyaml: build/benchmark-fyaml.out

# The raw timing log the two generated tables below are parsed from:
# for each small test-data file and each synthetic large file, for
# each of the three programs (yaml egg, slibfyaml egg -f/--fyaml, and
# besm2-rst-f -- slibfyaml's handle/tree-based document API), one
# line "RESULT category label program runs total-real-seconds", where
# total-real-seconds is bash's own `time` builtin's real elapsed time
# for the *whole* runs-repetition loop (not per run -- benchmark-
# report.py divides by runs itself). category is "small" (label is
# the test-data file's base name) or "large" (label is the synthetic
# file's entity count).
build/benchmark-fyaml.out: build/besm2-rst build/besm2-rst-f \
		$(TEST_DATA_2E) $(BENCHMARK_LARGE_FILES)
	@TIMEFORMAT='%R'; \
	: >$@; \
	run_case() { \
		category=$$1; label=$$2; file=$$3; runs=$$4; \
		for prog in yaml fyaml treefyaml; do \
			case $$prog in \
				yaml)      cmd="build/besm2-rst -s $(BR2EOPTS) $$file" ;; \
				fyaml)     cmd="build/besm2-rst -s -f $(BR2EOPTS) $$file" ;; \
				treefyaml) cmd="build/besm2-rst-f -s $(BR2EOPTS) $$file" ;; \
			esac; \
			echo "=== $$label ($$runs runs, $$prog) ==="; \
			real=$$( { time ( for i in $$(seq 1 $$runs); do \
				$$cmd >/dev/null; \
			done ); } 2>&1 >/dev/null ); \
			echo "RESULT $$category $$label $$prog $$runs $$real" >>$@; \
		done; \
	}; \
	for f in $(TEST_DATA_2E); do \
		run_case small $$(basename $$f) $$f $(BENCH_N); \
	done; \
	sizes=($(BENCHMARK_LARGE_SIZES)); runs=($(BENCHMARK_LARGE_RUNS)); \
	for i in $${!sizes[@]}; do \
		run_case large $${sizes[$$i]} build/synthetic-$${sizes[$$i]}.yaml $${runs[$$i]}; \
	done

# The two reST grid tables benchmark-fyaml.rst pulls in with
# ".. include::" -- one for the small test-data files, one for the
# synthetic large files -- parsed out of the timing log above.
build/benchmark-fyaml-small.gen.rst build/benchmark-fyaml-large.gen.rst &: \
		build/benchmark-fyaml.out benchmark-report.py
	python3 benchmark-report.py build/benchmark-fyaml.out \
		build/benchmark-fyaml-small.gen.rst \
		build/benchmark-fyaml-large.gen.rst

# Render the benchmark-fyaml.rst write-up (yaml egg vs. slibfyaml egg
# vs. besm2-rst-f comparison) to PDF and HTML. Depends transitively,
# through the two generated tables above, on build/benchmark-fyaml.out
# and (through that) on both programs and every small/synthetic test
# file -- so this rebuilds whenever a benchmark rerun would actually
# change the numbers, without needing an explicit "make benchmark-
# fyaml" first.
benchmark-fyaml-report: build/benchmark-fyaml.ms.pdf build/benchmark-fyaml.html

yamlerr: $(TEST_YAMLERROUTPUT)

clean: testclean
	-rm -v $(PROGRAMS)
testclean:
	-rm -v	build/*.gen.rst build/*.ms.pdf \
		build/*.native build/*.ms \
		build/*.html build/*.yamlerr \
		build/synthetic-*.yaml build/benchmark-fyaml.out

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

# The same variants again, but built with besm2-rst-f, which loads YAML
# through slibfyaml's handle/tree-based document API ((slibfyaml
# documents) + (slibfyaml nodes)) instead of materializing it up front
# the way besm2-rst does (both its default yaml-egg path and its own
# -f/--fyaml slibfyaml-scheme path above) -- for comparison against the
# default output (see compare-treefyaml).
build/%-2e-treefyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst-f
	build/besm2-rst-f -s $(BR2EOPTS) $< >$@ # handle/tree slibfyaml

build/%-2e-terse-treefyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst-f
	build/besm2-rst-f -s -t $(BR2EOPTS) $< >$@ # terse, handle/tree slibfyaml

build/%-2e-tbl-treefyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst-f
	build/besm2-rst-f -s -m $(BR2EOPTS) $< >$@ # ms tables, handle/tree slibfyaml

build/%-2e-unicode-minus-treefyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst-f
	build/besm2-rst-f -s -n $(BR2EOPTS) $< >$@ # unicode minus sign, handle/tree slibfyaml

build/%-2e-tbl-unicode-minus-treefyaml.gen.rst : test-data/%-2e.yaml build/besm2-rst-f
	build/besm2-rst-f -s -m -n $(BR2EOPTS) $< >$@ # ms tables, unicode minus sign, handle/tree slibfyaml

# A synthetic large 2E YAML file for benchmark-fyaml: the body of
# enyon-boase-2e.yaml (everything after its leading "---") repeated N
# times, which is already a valid way to get an N-entity YAML
# sequence, since each repetition's own "- name: ..." starts a new
# list item at the top level.
build/synthetic-%.yaml : test-data/enyon-boase-2e.yaml
	{ echo '---'; for i in $$(seq 1 $*); do tail -n +2 $<; done; } >$@

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

# benchmark-fyaml.rst is a plain reST file at the top of the tree, not
# one of the generated %.gen.rst files above, so it needs its own
# explicit rules instead of the generic build/%.ms.pdf / build/%.html
# pattern rules. It also ".. include::"s the two generated benchmark
# tables, so both are prerequisites here too -- pandoc has no way to
# see through an include on its own, so without this, editing just
# the tables (e.g. via a fresh "make benchmark-fyaml") would leave
# these outputs stale.
build/benchmark-fyaml.ms.pdf : benchmark-fyaml.rst \
		build/benchmark-fyaml-small.gen.rst build/benchmark-fyaml-large.gen.rst
	pandoc -r rst -w ms --template=tkb $(MS_COLUMNS) -o $@ $<

build/benchmark-fyaml.html : benchmark-fyaml.rst \
		build/benchmark-fyaml-small.gen.rst build/benchmark-fyaml-large.gen.rst
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
	build/%-2e-tbl-unicode-minus-fyaml.gen.rst \
	build/%-2e-treefyaml.gen.rst build/%-2e-terse-treefyaml.gen.rst \
	build/%-2e-tbl-treefyaml.gen.rst build/%-2e-unicode-minus-treefyaml.gen.rst \
	build/%-2e-tbl-unicode-minus-treefyaml.gen.rst

print-%  : ; @echo $* = $($*)

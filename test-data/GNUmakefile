#YAMLFILES=android-battle-maid.yaml cabbit-battle.yaml FV2021-Coleopteran.yaml
YAMLFILES=$(wildcard *.yaml)
RSTFILES=$(YAMLFILES:.yaml=.gen.rst)
ERRFILES=$(foreach f,$(YAMLFILES:.yaml=.err),../build/$(f))


all:	$(ERRFILES)

../build/%.err : %.yaml
	yamllint -f parsable $< | tee $@

# These get build by GNUmakefile in parent directory.
#%.gen.rst : %.yaml
#	../build/besm-rst 

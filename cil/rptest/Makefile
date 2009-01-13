CILLY=../bin/cilly
MERGED_DIR=merged_files/
REPAIRED_DIR=repaired_files/

SOURCES=$(wildcard */*.c)
MERGED_SOURCES=$(addprefix $(MERGED_DIR),$(SOURCES))
REPAIRED_SOURCES=$(addprefix $(REPAIRED_DIR),$(SOURCES))

all: $(MERGED_SOURCES) $(REPAIRED_SOURCES)

$(MERGED_DIR)% : %
	mkdir -p $(dir $@)
	-$(CILLY) --out=$@ $<

$(REPAIRED_DIR)% : %
	mkdir -p $(dir $@)
	-$(CILLY) --out=$@ $< --domakeCFG --merge --noPrintLn --dorandomrepair

clean:
	rm -rf $(MERGED_DIR) $(REPAIRED_DIR)

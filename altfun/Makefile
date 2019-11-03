DIRS := ../include/gpio
ALTS := $(wildcard *.txt)
HDRS := $(ALTS:%.txt=../include/gpio/%.h)

all: dirs $(HDRS)

.PHONY: dirs

dirs: ${DIRS}

${DIRS}:
	mkdir -p $@

../include/gpio/%.h: %.txt
	stack runghc ../util/AltFun.hs $< > $@

clean:
	rm -rf $(DIRS)


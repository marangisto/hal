SVDS := $(wildcard svd/*.svd)
DEVS := $(SVDS:svd/%.svd=include/device/%.h)
VECT := $(SVDS:svd/%.svd=src/vector/%.c)

include/device/%.h: svd/%.svd
	[ -d include/device ] || mkdir include/device
	SVD2CPP $< > $@

src/vector/%.c: svd/%.svd
	[ -d src/vector ] || mkdir src/vector
	SVD2CPP --interrupt $< > $@

all: $(DEVS) $(VECT)

clean:
	rm -f $(DEVS) $(VECT)


TEAM_ID="761516ab-2a60-4b4e-a8e8-486e75c4c850"

ARCHIVE=icfp-$(TEAM_ID).tar.gz

all: $(ARCHIVE)

PRODUCTION_FILES := $(wildcard production/*)
SCRIPT_FILES := $(wildcard scripts/*)


$(ARCHIVE): \
		deploy/tbd_python.tar.xz \
		deploy/make.production.item \
		deploy/make.scripts.item
	tar -czf $@ -C deploy .


# Python

deploy/tbd_python.tar.xz: tbd_python.tar.xz | deploy/
	cp $^ $@

tbd_python.tar.xz:
	echo "Compile yourself" && exit 1


# production

deploy/make.production.item: $(PRODUCTION_FILES) | deploy/production/
	cp -rf $^ deploy/production/ && touch $@


# scripts and READMEs

deploy/make.scripts.item: $(SCRIPT_FILES) | deploy/
	cp -rf $^ deploy && touch $@


# stuff

deploy/ deploy/production/:
	mkdir -p $@


# clean

clean::
	rm -rf deploy $(ARCHIVE)

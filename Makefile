TEAM_ID="761516ab-2a60-4b4e-a8e8-486e75c4c850"

ARCHIVE=icfp-$(TEAM_ID).tar.gz

all: $(ARCHIVE)

PRODUCTION_FILES := $(wildcard production/*)
SCRIPT_FILES := $(wildcard scripts/*)

DEPLOYED_PRODUCTION_FILES = $(addprefix deploy/, $(PRODUCTION_FILES))
DEPLOYED_SCRIPT_FILES = $(addprefix deploy/, $(SCRIPT_FILES:scripts/%=%)

$(ARCHIVE): \
		deploy/python_tbd.xz \
		$(DEPLOYED_PRODUCTION_FILES) \
		$(DEPLOYED_SCRIPT_FILES)
	tar -C deploy -czf $@


# Python

deploy/python_tbd.xz: python_tbd.xz | deploy/
	cp $^ $@

python_tbd.xz:
	echo "Compile yourself" && exit 1


# production

$(DEPLOYED_PRODUCTION_FILES): $(PRODUCTION_FILES) | deploy/ deploy/production
	cp -rf $^ deploy/production/


# scripts and READMEs

$(DEPLOYED_SCRIPT_FILES): $(SCRIPT_FILES) | deploy/
	cp -rf $^ deploy


# stuff

deploy/ deploy/production/:
	mkdir -p $@


# clean

clean::
	rm -rf deploy $(ARCHIVE)
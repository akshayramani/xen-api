# makefile for v6 component in build system
include $(B_BASE)/common.mk
include $(B_BASE)/rpmbuild.mk

REPO = $(call git_loc,v6)
RPM_BINDIR = $(RPM_RPMSDIR)/$(DOMAIN0_ARCH_OPTIMIZED)

.PHONY: build
build: srpm $(MY_SOURCES)/MANIFEST
	$(RPMBUILD) --target $(DOMAIN0_ARCH_OPTIMIZED) -bb $(RPM_SPECSDIR)/v6d.spec
	$(RPMBUILD) --target $(DOMAIN0_ARCH_OPTIMIZED) -bb $(RPM_SPECSDIR)/v6testd.spec
	mkdir -p $(MY_OUTPUT_DIR) $(MY_XENSERVER_PACKAGES)
	# Deliberately omit the debuginfo RPM (v6d-debuginfo-0...)
	cp $(RPM_BINDIR)/v6d-0*.rpm $(MY_XENSERVER_PACKAGES)
	cp $(RPM_BINDIR)/v6testd-0-0.i686.rpm $(MY_OUTPUT_DIR)/v6-test.rpm
	cp $(RPM_BUILDDIR)/v6testd-0/v6testd $(MY_OUTPUT_DIR)

.PHONY: srpm
srpm:
	mkdir -p $(RPM_SRPMSDIR) $(RPM_SPECSDIR) $(RPM_SOURCESDIR) $(RPM_RPMSDIR)
	cd .. ; \
	git archive --prefix=v6d-0/ HEAD | bzip2 -c > $(RPM_SOURCESDIR)/v6d-0.tar.bz2 ; \
	git archive --prefix=v6testd-0/ HEAD | bzip2 -c > $(RPM_SOURCESDIR)/v6testd-0.tar.bz2 ; \
	cd mk
	install ../v6d.spec $(RPM_SPECSDIR)
	install ../v6testd.spec $(RPM_SPECSDIR)
	$(RPMBUILD) --target $(DOMAIN0_ARCH_OPTIMIZED) -bs $(RPM_SPECSDIR)/v6d.spec
	$(RPMBUILD) --target $(DOMAIN0_ARCH_OPTIMIZED) -bs $(RPM_SPECSDIR)/v6testd.spec

$(MY_SOURCES)/MANIFEST: $(MY_SOURCES_DIRSTAMP)
	rm -f $@
	/bin/sh ./srpms-to-manifest v6 $(MY_OUTPUT_DIR)/SRPMS > $@

.PHONY: clean
clean:
	rm -f $(OUTPUT)
	$(MAKE) -C $(REPO) clean

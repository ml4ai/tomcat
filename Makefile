PATCH_EXPERTS_DIR:=external/OpenFace/lib/local/LandmarkDetector/model/patch_experts

# Download patch experts for OpenFace
$(PATCH_EXPERTS_DIR)/cen_patches_0.25_of.dat:
	curl https://www.dropbox.com/s/7na5qsjzz8yfoer/$(@F) -o $@
$(PATCH_EXPERTS_DIR)/cen_patches_0.35_of.dat:
	curl https://www.dropbox.com/s/k7bj804cyiu474t/$(@F) -o $@
$(PATCH_EXPERTS_DIR)/cen_patches_0.50_of.dat:
	curl https://www.dropbox.com/s/ixt4vkbmxgab1iu/$(@F) -o $@
$(PATCH_EXPERTS_DIR)/cen_patches_1.00_of.dat:
	curl https://www.dropbox.com/s/2t5t1sdpshzfhpj/$(@F) -o $@

MODELS:=$(PATCH_EXPERTS_DIR)/cen_patches_0.25_of.dat\
		$(PATCH_EXPERTS_DIR)/cen_patches_0.35_of.dat\
		$(PATCH_EXPERTS_DIR)/cen_patches_0.50_of.dat\
		$(PATCH_EXPERTS_DIR)/cen_patches_1.00_of.dat

models: $(MODELS)

clean:
	rm -rf build

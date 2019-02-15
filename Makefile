MKFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MKFILE_DIR := $(dir $(MKFILE_PATH))
PROF_DIR := $(abspath git/profile-dotemacs)
PROF_FILE := $(addprefix $(PROF_DIR)/,profile-dotemacs.el)

profile: | $(PROF_FILE)
	emacs -Q -l $(PROF_FILE) \
	--eval "(progn (package-initialize) \
		(setq profile-dotemacs-file \
		(setq load-file-name \"$(abspath init.el)\")))" \
	-f profile-dotemacs

.ONESHELL:
$(PROF_FILE): | $(PROF_DIR)
ifeq (,$(wildcard $(PROF_FILE)))
	cd $(PROF_DIR) ; \
	curl "http://www.randomsample.de/profile-dotemacs.el" -o profile-dotemacs.el ; \
	cd $(MKFILE_DIR)
endif

$(PROF_DIR):
	mkdir -p $(PROF_DIR)

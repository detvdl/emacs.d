MKFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MKFILE_DIR := $(dir $(MKFILE_PATH))
PROF_DIR := $(abspath elisp/profile-dotemacs)
PROF_FILE := $(addprefix $(PROF_DIR)/,profile-dotemacs.el)

.PHONY: help
help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  profile    to profile the current init.el startup file"

.PHONY: profile
profile: | $(PROF_FILE)
	emacs -Q -l $(PROF_FILE) \
	--eval "(progn (package-initialize) \
		(setq profile-dotemacs-file \
		(setq load-file-name \"$(abspath init.el)\")))" \
	-f profile-dotemacs

$(PROF_FILE):
ifeq (,$(wildcard $(PROF_FILE)))
	mkdir -p $(PROF_DIR)
	wget -O $(PROF_FILE) "https://raw.githubusercontent.com/abo-abo/profile-dotemacs/master/profile-dotemacs.el"
endif

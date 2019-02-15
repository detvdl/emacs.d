profile:
	emacs -Q -l git/profile-dotemacs/profile-dotemacs.el \
	--eval "(progn (package-initialize) \
		(setq profile-dotemacs-file \
		(setq load-file-name \"$(abspath init.el)\")))" \
	-f profile-dotemacs

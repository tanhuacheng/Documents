(setq inhibit-startup-screen t) ; hide the welcome screen(*GNU Emacs*) when startup
(load-theme 'deeper-blue)

(global-linum-mode t)

(setq-default tab-width 4
              indent-tabs-mode nil)
(setq-default c-basic-offset 4
			  c-default-style "linux")

(add-hook 'after-init-hook 'global-company-mode) ; M-x list-packages => company-mode => install

(setq py-autopep8-options (list "--aggressive"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

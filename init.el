;; DO NOT EDIT THIS FILE, USE emacs.org INSTEAD.

(setq garbage-collection-messages t)
(package-initialize)
(require 'ob-tangle)


;; Run the elisp code in our main configuration file
(org-babel-load-file "~/.emacs.d/emacs.org")

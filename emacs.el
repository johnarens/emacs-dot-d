
;; Initialize the package module
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; (package-initialize)
 
;; Add all the package repositories that we want to use.
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Reloading the list of packages takes time, we will do it at most
;; once. This variable will be true if the list has already be
;; reloaded.
(setq my/package-refreshed nil)

;; Install a package if it is not already installed.
(defun my/install (package)
  ; install the package if it is not already installed
  (unless (package-installed-p package)

  ; make sure the package index is up-to-date
  (unless my/package-refreshed
    (package-refresh-contents)
    (setq my/package-refreshed t))

  (package-install package)))

(setq inhibit-startup-screen t)
(if (display-graphic-p)
    (tool-bar-mode -1))
(column-number-mode 1)
(show-paren-mode 1)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(setq display-time-24hr-format t)
(display-time)

;; (setq load-path (cons "/home/simplex/svn/trunk" load-path))
;; (autoload 'gtags-mode "gtags" "" t)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

(require 'ido)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".org" ".py" ".sv" ".txt" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
(ido-mode t)

(setq org-startup-with-inline-images t)
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.x?html?\\'" . "firefox %s")
        ("\\.pdf\\'" . "evince \"%s\"")
        ("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1")
        ("\\.pdf.xoj" . "xournal %s")))
(defun my/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)
;; (org-display-inline-images t)
;; (add-hook 'org-babel-after-execute-hook'org-display-inline-images)

(my/install 'plantuml-mode)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(setq org-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
(message "Plantuml done")

(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (plantuml . t)
   (python . t)
   (sh . t)
   ))
(setq org-confirm-babel-evaluate nil)

(setq org-latex-create-formula-image-program 'imagemagick)
(defvar org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")

(defun org-latex-fragment-toggle ()
  "Toggle a latex fragment image "
  (and (eq 'org-mode major-mode)
       (let* ((el (org-element-context))
              (el-type (car el)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            org-latex-fragment-last
            ;; and are on a fragment now
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type))
            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (= (org-element-property :begin el)
                    (org-element-property :begin org-latex-fragment-last))))
           ;; go back to last one and put image back
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; now remove current image
           (goto-char (org-element-property :begin el))
           (let ((ov (loop for ov in org-latex-fragment-image-overlays
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov)))
           ;; and save new fragment
           (setq org-latex-fragment-last el))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not (or
                  (eq 'latex-fragment el-type)
                  (eq 'latex-environment el-type)))
            ;; but we were on one
            org-latex-fragment-last)
           ;; put image back on
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; unset last fragment
           (setq org-latex-fragment-last nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not org-latex-fragment-last)
            ;; but now we are
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type)))
           (goto-char (org-element-property :begin el))
           ;; remove image
           (let ((ov (loop for ov in org-latex-fragment-image-overlays
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov)))
           (setq org-latex-fragment-last el))))))

(add-hook 'post-command-hook 'org-latex-fragment-toggle)

(setq org-export-latex-packages-alist '(("" "tikz")))

(setq org-latex-packages-alist '())
(add-to-list 'org-latex-packages-alist
             '("" "tikz" t))
(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

(my/install 'projectile)
(require 'projectile)
(projectile-global-mode)

(my/install 'python)
(my/install 'sphinx-doc)
(my/install 'sphinx-mode)

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

(my/install 'jedi)
(my/install 'jedi-direx)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "jedi-direx")
(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
(add-hook 'jedi-mode-hook 'jedi-direx:setup)

(setq jedi:server-args
      '("--sys-path" "~/anaconda2/lib/python2.7/site-packages"))

;; (my/install 'pymacs)
;; (setq py-load-pymacs-p nil)
(add-to-list 'load-path "~/.emacs.d/elisp/Pymacs")
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

(setq comint-prompt-read-only t)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(my/install 'color-theme-sanityinc-tomorrow)
;; (my/install 'solarized-theme)
;; (require 'solarized-theme)
;; 
;; (load-theme 'solarized-dark t)

(my/install 'magit)
(require 'magit)

(my/install 'git-gutter+)
(require 'git-gutter+)

; enable git-gutter everywhere by default
(global-git-gutter+-mode)

(my/install 'graphviz-dot-mode)

(my/install 'groovy-mode)

(require 'cl)
(require 'groovy-mode)

(setq js-indent-level 2)

(my/install 'auto-complete-nxml)
(my/install 'rnc-mode)

;; Keystroke to popup help about something at point.
(setq auto-complete-nxml-popup-help-key "C-:")

;; Keystroke to toggle on/off automatic completion.
(setq auto-complete-nxml-toggle-automatic-key "C-c C-t")

(global-set-key [home] (quote beginning-of-buffer))
(global-set-key [end] (quote end-of-buffer))
(global-set-key [f12] (quote repeat-complex-command))
;; (global-set-key [f5] (quote kmacro-end-and-call-macro))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(define-key org-mode-map (kbd "$")
  (lambda ()
    (interactive)
    (insert "$")
    (save-excursion
      (left-char 1)
      (if (org-inside-LaTeX-fragment-p)
          (progn
            (right-char 2)
            (org-preview-latex-fragment))))))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

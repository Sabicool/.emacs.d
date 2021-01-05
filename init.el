(setq straight-enable-use-package-integration t
      straight-use-package-by-default t
      use-package-always-defer t)

;; https://github.com/raxod502/straight.el#getting-started
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/raxod502/straight.el/"
                 "develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package general
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-override-mode)

  (eval-and-compile
    (defalias 'gsetq #'general-setq)
    (defalias 'gsetq-local #'general-setq-local)
    (defalias 'gsetq-default #'general-setq-default))

  (general-create-definer general-my/leader
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC"))

(gsetq create-lockfiles nil
       find-program "C:\\Users\\saabh\\scoop\\shims\\find.exe"
       sentence-end-double-space nil
       show-paren-delay 0
       vc-follow-symlinks t)

(show-paren-mode)

(require 'org)

(use-package ivy
  :init
  (ivy-mode)
  (general-def ivy-minibuffer-map
    "C-j" #'ivy-next-line
    "C-k" #'ivy-previous-line
    "C-l" #'ivy-scroll-down-command
    "C-h" #'ivy-scroll-up-command)
  )

(use-package counsel)

(use-package darkroom)

(use-package defrepeater :demand t)

(use-package evil
  :after ivy
  :init
  (gsetq evil-cross-lines t
         evil-overriding-maps nil
         evil-shift-width 2
         evil-split-window-below t
         evil-vsplit-window-right t
         evil-want-C-d-scroll t
         evil-want-C-u-scroll t
         evil-want-integration t
         evil-want-keybinding nil
         evil-want-Y-yank-to-eol t)

  (evil-mode)
  :config
  (defrepeater #'evil-window-increase-height)
  (defrepeater #'evil-window-decrease-height)
  (defrepeater #'evil-window-increase-width)
  (defrepeater #'evil-window-decrease-width)
  (general-def
    [remap evil-window-increase-height] #'evil-window-increase-height-repeat
    [remap evil-window-decrease-height] #'evil-window-decrease-height-repeat
    [remap evil-window-increase-width] #'evil-window-increase-width-repeat
    [remap evil-window-decrease-width] #'evil-window-decrease-width-repeat)

  (general-def '(normal visual) 'override
    "\\" #'evil-switch-to-windows-last-buffer
    "bs" #'ivy-switch-buffer
    "C-h" #'evil-window-left
    "C-j" #'evil-window-down
    "C-k" #'evil-window-up
    "C-l" #'evil-window-right
    "C-/" #'help-command)

  (general-my/leader
    "ff" #'counsel-find-file
    "fj" #'projectile-find-file
    "fp" #'projectile-switch-project
    "wv" #'evil-window-vsplit
    "wh" #'evil-window-split
    "wc" #'evil-window-delete
    "bl" #'ibuffer
    "t" #'treemacs
    "d" #'darkroom-tentative-mode))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package rainbow-delimiters
  :ghook
  'prog-mode-hook)

(use-package auctex
  :init
  (gsetq TeX-auto-save t)
  (gsetq TeX-parse-self t))

(use-package company
  :ghook
  'text-mode-hook
  'prog-mode-hook

  :config
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
 (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (gsetq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package projectile
  :init
  (projectile-mode +1))

(use-package yasnippet
  :init
  (gsetq yas-snippet-dirs
	 '("~/.emacs.d/snippets/"))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :init 
  (yas-reload-all))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (gsetq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay      0.5
	  treemacs-directory-name-transformer    #'identity
	  treemacs-display-in-side-window        t
	  treemacs-eldoc-display                 t
	  treemacs-file-event-delay              5000
	  treemacs-file-extension-regex          treemacs-last-period-regex-value
	  treemacs-file-follow-delay             0.2
	  treemacs-file-name-transformer         #'identity
	  treemacs-follow-after-init             t
	  treemacs-git-command-pipe              ""
	  treemacs-goto-tag-strategy             'refetch-index
	  treemacs-indentation                   2
	  treemacs-indentation-string            " "
	  treemacs-is-never-other-window         nil
	  treemacs-max-git-entries               5000
	  treemacs-missing-project-action        'ask
	  treemacs-move-forward-on-expand        nil
	  treemacs-no-png-images                 nil
	  treemacs-no-delete-other-windows       t
	  treemacs-project-follow-cleanup        nil
	  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                      'left
	  treemacs-read-string-input             'from-child-frame
	  treemacs-recenter-distance             0.1
	  treemacs-recenter-after-file-follow    nil
	  treemacs-recenter-after-tag-follow     nil
	  treemacs-recenter-after-project-jump   'always
	  treemacs-recenter-after-project-expand 'on-distance
	  treemacs-show-cursor                   nil
	  treemacs-show-hidden-files             t
	  treemacs-silent-filewatch              nil
	  treemacs-silent-refresh                nil
	  treemacs-sorting                       'alphabetic-asc
	  treemacs-space-between-root-nodes      t
	  treemacs-tag-follow-cleanup            t
	  treemacs-tag-follow-delay              1.5
	  treemacs-user-mode-line-format         nil
	  treemacs-user-header-line-format       nil
	  treemacs-width                         35
	  treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-load-theme "colorful")
    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package doom-themes
  :init
  (gsetq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (gsetq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package which-key
  :init
  (which-key-mode))

(use-package dashboard
  :init
  (gsetq dashboard-items '((recents  . 5)
			   (bookmarks . 5)
			   (projects . 5)
			   (agenda . 5))
	 dashboard-set-file-icons t
	 dashboard-set-heading-icons t
	 dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))

(use-package all-the-icons)

;; (gsetq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(global-visual-line-mode 1); Proper line wrapping
(global-hl-line-mode 1); Highlight current row
(gsetq calendar-week-start-day 1); Calender should start on Monday

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;(Add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

; (use-package latex-mode
;   :straight nil
;   :init
;   :config)

(add-hook 'org-mode-hook (lambda ()
			   "Beautify Org Checkbox Symbol"
			   (push '("[ ]" .  "☐") prettify-symbols-alist)
			   (push '("[X]" . "☑" ) prettify-symbols-alist)
			   (push '("[-]" . "❍" ) prettify-symbols-alist)
			   (push '("#+title: "        . "") prettify-symbols-alist)
			   (push '("#+subtitle: "     . "") prettify-symbols-alist)
			   (push '("#+author: "       . "- ") prettify-symbols-alist)
			   (prettify-symbols-mode)))

(gsetq TeX-PDF-mode t
       TeX-source-correlate-mode t
       TeX-source-correlate-method 'synctex
       TeX-view-program-list
       '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance -invert-colors" (mode-io-correlate " -forward-search %b %n ") " %o"))))

(eval-after-load 'tex
    '(progn
       (assq-delete-all 'output-pdf TeX-view-program-selection)
       (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

(server-start)

;(require 'sumatra-forward)

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
    "bl" #'ibuffer))

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
	 '("~/.emacs.d/snippets/latex-mode/"))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package doom-themes
  :init
  (gsetq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

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

(global-visual-line-mode 1); Proper line wrapping
(global-hl-line-mode 1); Highlight current row
(gsetq calendar-week-start-day 1); Calender should start on Monday

;(Add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

; (use-package latex-mode
;   :straight nil
;   :init
;   :config)

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

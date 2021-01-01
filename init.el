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

(use-package ivy
  :init
  (ivy-mode))

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

  (general-my/leader
    "ff" #'counsel-find-file))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package rainbow-delimiters
  :ghook
  'prog-mode-hook)

(use-package auctex)

(use-package company
  :ghook
  'text-mode-hook
  'prog-mode-hook)

(use-package projectile
  :init
  (projectile-mode +1))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package which-key
  :init
  (which-key-mode))

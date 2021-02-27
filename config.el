;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; misc. mac os x specific stuff -----------------------------------------------
(setq 
  ns-command-modifier 'meta
  ns-pop-up-frames    nil  ;; prevent the finder from opening a new frame
  )

;; upon drag-and-drop: find the file, w/shift insert filename; w/meta insert
;; file contents note that the emacs window must be selected (CMD-TAB) for the
;; modifiers to register
(define-key global-map [M-ns-drag-file] 'ns-insert-file)
(define-key global-map [S-ns-drag-file] 'ns-insert-filename)
(define-key global-map [ns-drag-file]   'ns-find-file-in-frame)

;; the default mode in os x triggers text mode menus this makes it cycle
;; through windows
(global-set-key (kbd "M-`" ) 'other-frame)
(global-set-key (kbd "M-^")  'enlarge-window)

;; to be used with dash application for function lookup
(global-set-key (kbd "C-c d") 'dash-at-point)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "steve ulrich"
;;       user-mail-address "sulrich@arista.com"
;; )

;; big defaults section ---------------------------------------------------------
(setq-default
  user-full-name    "steve ulrich"        ;; set name
  user-mail-address "sulrich@arista.com"  ;; set e-mail address
  query-user-mail-address nil
  delete-key-deletes-forward t ;; Make delete key work normally
  column-number-mode         t ;; Display line and column numbers
  line-number-mode           t
  ;; visible-bell               nil
  ;; inhibit-startup-message    t
  ;; initial-scratch-message    ""
  default-fill-column        '90 ;; wrap at N columns
  comment-column             '50
  ;; deal with the tabs vs. spaces religion
  tab-width                  2
  c-basic-indent             2
  indent-tabs-mode           nil

  ;; the following is to make shell operation a little more friendly
  ;; tramp stuff ------------------------------------------------------------------
  tramp-default-method       "ssh" ;; uses ControlMaster
  tramp-default-user         "sulrich"
  tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "JetBrains Mono" :size 12)
      doom-big-font (font-spec :family "JetBrains Mono" :size 16))

;; THEME CUSTOMIZATIONS
;;
;; bbatsov/solarized-theme overrides: https://github.com/bbatsov/solarized-emacs
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)
;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)
;; make the modeline high contrast
(setq solarized-high-contrast-mode-line nil)
;; use less bolding
(setq solarized-use-less-bold t)
;; use more italics - if t
(setq solarized-use-more-italic nil)
;; use less colors for indicators such as git:gutter, flycheck and similar
(setq solarized-emphasize-indicators nil)
;; don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)
;; avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; deft (note taking) config
(global-set-key [f8] 'deft)
(global-set-key (kbd "C-x C-g") 'deft-find-file)

(setq
  deft-extensions '("md")
  deft-text-mode 'markdown-mode
  deft-directory "~/.notes"
  deft-recursive t
  deft-markdown-mode-title-level -1
  deft-file-naming-rules
  '((noslash . "-")
    (nospace . "-")
    (case-fn . downcase))
  deft-use-filter-string-for-filename t
  deft-auto-save-interval 0
  deft-default-extension "md"
  )


;; -----------------------------------------------------------------------------
;; markdown-mode overrides
(setq 
  markdown-hide-urls t
  markdown-make-gfm-checkboxes-buttons nil
  markdown-list-indent-width  2
  markdown-fontify-code-blocks-natively t
  markdown-open-command "~/bin/marked2.sh"
)

(dolist (hook '(markdown-mode-hook))
  (add-hook hook (lambda ()
	           (flyspell-mode)
	           (visual-line-mode)
	           (set-fill-column 80)
	           ))
  ) ;; end markdown-mode hooks

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'virtual-auto-fill-mode))

;; minimap customization
(setq minimap-update-delay 0.75
      minimap-minimum-width 30
)

;; dealing with whitespace ------------------------------------------------------
(setq  whitespace-style
       '(face empty trailing tabs newline tab-mark newline-mark))

;; -----------------------------------------------------------------------------
;; python settings
;; (elpy-enable)
;; (require 'pyenv-mode-auto)
;; (require 'pyenv-mode)
;; (after! python
;;   (set-company-backend! 'elpy-mode
;;     '(elpy-company-backend :with company-files company-yasnippet)))

;; (after! python
;;         (set-company-backend! 'python-mode 'elpy-company-backend))

;; the following keybindings conflict with the markdown-mode bindings
;; this eliminates the conflicts.
;; (eval-after-load "pyenv-mode"
;;   '(define-key pyenv-mode-map (kbd "C-c C-s") nil))
;; (eval-after-load "pyenv-mode"
;;   '(define-key pyenv-mode-map (kbd "C-c C-u") nil))
;; 
;; source
;; ;; https://realpython.com/emacs-the-best-python-editor/#elpy-python-development
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ;; automatically run black on buffer save
;; (add-hook 'elpy-mode-hook
;;           '(lambda ()
;;              (when (eq major-mode 'python-mode)
;;                ;; (add-hook 'before-save-hook 'elpy-black-fix-code)
;;                (add-hook 'before-save-hook 'py-isort-before-save)
;;                )))
;; from: https://github.com/jorgenschaefer/elpy/issues/1550 this fixes
;; the^G in the output. i'm cool w/using ipython/jupyter console here.
(setq python-shell-interpreter "ipython"
      ;; (setq python-shell-interpreter "jupyter-console" ;
      ;; python-shell-interpreter-args "--simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
;; (setq elpy-shell-echo-output nil)

;; ~/.doom.d/config.el
(setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
(setq +python-jupyter-repl-args '("--simple-prompt"))


;; spelling
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=fast")
      ispell-list-command "--list")

;; multiple-cursors cofiguration
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-c m c")     'mc/edit-lines)

;; atomic-chrome configuration 
(atomic-chrome-start-server)
(setq atomic-chrome-url-major-mode-alist
      `(( "partnerissuetracker". markdown-mode)
        ("localhost\\:4567"    . markdown-mode)
        ("github\\.com"        . markdown-mode)
        ("gitlab\\.aristanetworks\\.com" . markdown-mode)))

;; full: open in the selected window.
;; split: open in the new window by splitting the selected window (default).
;; frame: create a new frame and window in it.
(setq atomic-chrome-buffer-open-style 'full)
;; only available if you're using a frame
;; (setq atomic-chrome-buffer-frame-height  '40)

;; yasnippets configuration 
(setq
  yas-snippet-dirs
  '("~/.doom.d/snippets")
  )

;; ------------------------------------------------------------------------------
;; vterm configuration
(global-set-key [f2] 'vterm-toggle)


;; force some mode settings based on the fiel extension
(setq auto-mode-alist
      (append (list (cons "draft/[0-9]+"      'mail-mode)
                    (cons ".letter"           'mail-mode)
                    (cons ".article"          'mail-mode)
                    (cons "/tmp/Re"           'mail-mode)
                    (cons "/tmp/snd\\."       'mail-mode)
                    (cons "mutt-.*-[0-9]+"    'mail-mode)
                    (cons "\\.m$"             'octave-mode)
                    (cons "\\.org$"           'org-mode)
                    (cons "\\.css$"           'css-mode)
                    (cons "\\.scss$"          'css-mode)
                    (cons "\\.j2$"            'jinja2-mode)
                    (cons "\\.markdown$"      'markdown-mode)
                    (cons "\\.md$"            'markdown-mode)
                    (cons "\\.mkd$"           'markdown-mode)
                    (cons "\\.yang$"          'yang-mode)
                    (cons "\\.go$"            'go-mode)
                    (cons "\\.p4$"            'p4-mode)
                    (cons "\\.pde$"           'processing-mode)
                    (cons "\\.sh$"            'sh-mode))
              auto-mode-alist))


;; ------------------------------------------------------------------------------
;; custom functions
;;
(defun unfill-paragraph ()
  "replace newline chars in the current paragraph by single spaces.  This is the reverse of 'fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil))
  )

(defun unfill-region (start end)
  "replace newline chars in a region with single spaces. This is the reverse of 'fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end))
  )

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "new name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "a buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;; backup file preferences ------------------------------------------------------
;; store all backup and autosave files in the ~/tmp/.auto-saves"" dir
;; (setq backup-directory-alist '(("" . "~/tmp/.emacs-backup")))

;; http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
;; make backup to a designated dir, mirroring the full path
(setq auto-save-default nil) ; stop creating #autosave# files
(defun my-backup-file-name (fpath)
  "return a new file path for a given file path. If the new directories do not exist, create them."
  (let* (
         (backupRootDir "~/.e/backup/")
         ;; remove windows driver letter in path, for example,
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) “C:”
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
         )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
    )
  )
(setq make-backup-file-name-function 'my-backup-file-name)

;;; rcf-mode - stub
(require 'generic-x)
(define-generic-mode
      'rcf-mode                    ;; name of the mode to create
      '("#")                       ;; comments start with '#'
      '("add"
        "all_in"
        "any"
        "as_path"
        "as_path_list"
        "community"
        "community_list"
        "else"
        "ext_community"
        "ext_community_list"
        "false"
        "function"
        "has_all"
        "has_any"
        "has_none"
        "if"
        "igp"
        "local_preference"
        "med"
        "next_hop"
        "origin"
        "permit"
        "prefix"
        "prefix_list_v4"
        "prefix_list_v6"
        "remove"
        "retain"
        "return"
        "true"
        )                     ;; some keywords
      '(
        ("=" . 'font-lock-operator)     ;; '=' is an operator
        (";" . 'font-lock-built-in)
        ("{" . 'font-lock-built-in)
        ("}" . 'font-lock-built-in)
        ("(" . 'font-lock-built-in)
        (")" . 'font-lock-builtin)
        )     ;; misc operators and built-ins
      '("\\.rcf$")                      ;; files for which to activate this mode
       nil                              ;; other functions to call
      "a mode for RCF files"            ;; doc string for this mode
    )

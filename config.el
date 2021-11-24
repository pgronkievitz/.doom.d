;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Patryk Gronkiewicz"
      user-mail-address "patryk@gronkiewicz.dev")

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
(setq doom-font (font-spec :family "FantasqueSansMono Nerd Font" :size 18)
      doom-big-font (font-spec :family "FantasqueSansMono Nerd Font" :size 36)
      doom-variable-pitch-font (font-spec :family "Ubuntu Nerd Font" :size 18)
      doom-serif-font (font-spec :family "UbuntuMono Nerd Font" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-acario-light)
(setq fancy-splash-image "~/Pictures/emacs.svg")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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
(setq calendar-week-start-day 1
      calendar-date-style 'iso)
(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))
(use-package! org-ol-tree
  :commands org-ol-tree)
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree
      )
(setq guess-language-languages '(en pl)
      guess-language-min-paragraph-length 45)
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
)
(setq
 org-pomodoro-keep-killed-pomodoro-time t)

(after! ox-hugo
  (setq org-blackfriday--org-element-string '((src-block . "Kod") (table . "Tabela") (figure . "Rysunek"))))

(use-package! websocket
  :after org-roam)
(use-package! org-roam-ui
  :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
(setq ispell-dictionary "pl")

(setq calendar-christian-all-holidays-flag t)
(setq calendar-holidays
'((holiday-fixed 1 1 "New Year's Day")
 (holiday-fixed 2 14 "Valentine's Day")
 (holiday-fixed 3 17 "St. Patrick's Day")
 (holiday-fixed 4 1 "April Fools' Day")
 (holiday-fixed 5 25 "Mother's Day")
 (holiday-fixed 5 2 "Flag Day")
 (holiday-fixed 6 0 "Father's Day")
 (holiday-fixed 11 11 "Independence Day")
 (holiday-fixed 5 1 "Labor Day")
 (holiday-easter-etc)
 (holiday-fixed 12 25 "Christmas")
 (if calendar-christian-all-holidays-flag
     (append
      (holiday-fixed 1 6 "Epiphany")
      (holiday-fixed 12 24 "Christmas Eve")
      ;; (holiday-julian 12 25 "Christmas (Julian calendar)")
      (holiday-fixed 8 15 "Assumption")
      (holiday-advent 0 "Advent")))
 (solar-equinoxes-solstices)
 (holiday-sexp calendar-daylight-savings-starts
               (format "Daylight Saving Time Begins %s"
                       (solar-time-string
                        (/ calendar-daylight-savings-starts-time
                           (float 60))
                        calendar-standard-time-zone-name)))
 (holiday-sexp calendar-daylight-savings-ends
               (format "Daylight Saving Time Ends %s"
                       (solar-time-string
                        (/ calendar-daylight-savings-ends-time
                           (float 60))
                        calendar-daylight-time-zone-name)))))

;;; org-roam

;;;; create notes without entering
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))
(map! :leader :desc "Create node without opening" "n r I" #'org-roam-node-insert-immediate)


;;; projectile
(setq projectile-project-search-path '(("~/Projects" . 2)))

;;; latex export
;; (setq org-latex-default-packages-alist
;;       '(("AUTO" "inputenc" t ("pdflatex"))
;;         ("T1" "fontenc" t ("pdflatex"))
;;         ("" "xcolor" nil) ; Generally useful
;;         ("AUTO" "babel" t ("pdflatex"))
;;         ("AUTO" "polyglossia" t ("xelatex" "lualatex"))
;;         ("AUTO" "graphicx" t nil)
;;         ("AUTO" "amsmath" t nil)
;;         ("" "hyperref" nil)))
(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t ("pdflatex"))
        ("T1" "fontenc" t ("pdflatex"))
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "amssymb" t)
        ("AUTO" "babel" t ("pdflatex"))
        ("AUTO" "polyglossia" t ("xelatex" "lualatex"))))
(defvar my/bibs '("~/Documents/biblio.bib"))

(use-package citar-org
  :no-require
  :bind ; optional
  (:map org-mode-map
        ("C-c b" . #'org-cite-insert)) ; Also bound to C-c C-x C-@
  :custom
  (org-cite-global-bibliography my/bibs)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))
        ("" "capt-of" nil)
        ("" "hyperref" nil))
)

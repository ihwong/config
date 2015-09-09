;; 언어와 인코딩 설정

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

;; 한글 입력기 설정
(global-set-key (kbd "<S-kana>") 'toggle-input-method)

;; visual-line-mode
(global-visual-line-mode 1)

;; 시작할 때 전체 화면
;; (add-hook 'after-init-hook '(lambda () (toggle-frame-fullscreen)))

;; 기본 시작 화면 disable
(setq inhibit-splash-screen t)

;; no beep
(setq ring-bell-function 'ignore)

;; Show line-number, column-number in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; mouse scroll: one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; keyboard scroll: one line at a time
(setq scroll-step 1)

;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; no jumping with up/down arrow and cursor out of screen(?)
;; http://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs
(setq scroll-conservatively 10000)

;; no scroll bar
(scroll-bar-mode -1)

;; line spacing (vertical margin between adjacent two lines)
(setq-default line-spacing 5)

;; disable menu bar
(menu-bar-mode -1)

(add-hook 'nxml-mode-hook 'turn-on-auto-fill)

;; Markdown setting

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; ============================================================================================================================================================================================

;; 한글 글꼴과 관련된 설정
;; (set-default-font "NanumGothic-12") ;; default font
;; (set-fontset-font "fontset-default" 'korean-ksc5601 "-outline-나눔고딕-normal-normal-normal-mono-*-*-*-*-p-*-iso10646-1")
;; 여기에서 "" 안의 내용은 M-x set-default-font 로 목록을 확인한 다음 입력해야 한다. 즉, 사용자의 컴퓨터 환경에 따라 달라질 수 있다. 예를 들어, FreeBSD 환경에서는 아래와 같을 수 있다:
;; (set-fontset-font "fontset-default" 'korean-ksc5601 "-unknown-나눔고딕-normal-normal-normal-*-*-*-*-*-d-0-iso10646-1")
;; (set-fontset-font "fontset-default" 'korean-ksc5601 "-unknown-나눔고딕-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")

;;; 유니코드 한글영역...NanumGothicCoding에다가 원하는폰트를 적는다
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("나눔고딕" . "unicode-bmp"))
;;;유니코드 사용자 영역
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)  '("나눔고딕" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'kana '("Meiryo" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'han '("Microsoft YaHei" . "unicode-bmp"))


;; external backup directory and etc ===============================
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 100 ; 100 newest
   kept-old-versions 100 ; 100 oldest
   version-control t)       ; use versioned backups

;; backup each save, not the only first one after saved launching emacs
(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)
;; ===================================================================

;; AUCTeX + DocView mode

(setq TeX-PDF-mode t)
(setq TeX-auto-save nil) ;; no auto directory
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; C-c =
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; tex-fold-mode
;; http://www.flannaghan.com/2013/01/11/tex-fold-mode
(add-hook 'LaTeX-mode-hook 
	  (lambda () 
	    (TeX-fold-mode 1)
	    (add-hook 'find-file-hook 'TeX-fold-buffer t t)
	    (add-hook 'after-change-functions 
		      (lambda (start end oldlen) 
			(when (= (- end start) 1)
			  (let ((char-point 
                                 (buffer-substring-no-properties 
                                  start end)))
			    (when (or (string= char-point "}")
				      (string= char-point "$"))
			      (TeX-fold-paragraph)))))
		      t t)))

;; DocView로 열어 놓은 파일에 갱신 사항이 있을 때 다시 열지 않고 바로 업데이트
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; C-c C-c 했을 때 뭘 할 건지 물어보지 않고 바로 컴파일하기
(setq TeX-command-force "LaTeX")

(setq TeX-save-query nil) ;;autosave before compiling

;; ----------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("d3fb4aae88370323fee25637c3ce5a878487fccede1e84e1bab5efc9bee87fcb" "6a91828b9c39d783070e894eb04bd71f045574b69196005b2ec02ac9f93b61b1" default)))
 '(delete-selection-mode nil)
 '(doc-view-continuous t)
 '(fringe-mode (quote (1 . 1)) nil (fringe))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

;; ----------------------------------------------------------

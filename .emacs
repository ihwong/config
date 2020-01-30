;; ----
;; .emacs by hwong (hwong@korea.ac.kr)
;; ----
;; Test :
;;   - OS : Debian GNU/Linux 10 (buster)
;;   - Emacs : GNU Emacs 26.1
;;   - Last update of this .emacs file : 2020-01-15
;;   - 나눔글꼴 설치 필요 (ubuntu/debian 기준 fonts-nanum*)
;; ----

;; ----
;; 이걸 설정해줘야 C-\ 를 통해 이맥스 내장 한/영 전환 기능 사용 가능
;; (중요) 한글 다룰 거면 다음의 set-language-environment를 .emacs의 제일 위에 놓으세요.
(set-language-environment "Korean")
;; Shift+Space를 통해 이맥스 내장 한/영 전환
(global-set-key (kbd "<S-kana>") 'toggle-input-method)
;; ----

;; ----
;; UTF-8을 기본으로 사용
(prefer-coding-system 'utf-8)
;; ----

;; ----
;; 글꼴 설정
;; 영문 2자가 한글 1자에 대응되는지, 한글 단어 입력할 때 세로로 흔들리지 않는지
(set-face-attribute 'default nil :font "NanumGothicCoding" :height 150)
(set-fontset-font t 'hangul "NanumGothicCoding")
;; ----

;; ----
;; Emacs를 실행할 때 전체화면으로 시작
;; (add-hook 'after-init-hook '(lambda () (toggle-frame-fullscreen)))
;; 기본 시작 화면 안 뜨게 하기
(setq inhibit-splash-screen t)
;; 처음 시작할 때 해당 파일 열기
;; (find-file "~/Dropbox/TODO.txt")
;; 처음 C-x C-f 로 파일 열 때 기본 디렉토리 설정
(setq default-directory "~/")
;; ----

;; ----
;; GUI Emacs의 제목 표시줄에 현재 편집하고 있는 파일 경로와 이름을 표시
(setq frame-title-format
      (list (format "%s %%S: %%j" (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;; ----

;; ----
;; 이 값을 -1 로 설정하면, 제목 표시줄 바로 아래에 메뉴 바가 사라집니다.
;; File  Edit  Options  Buffers  Tools  Emacs-Lisp  Help
(menu-bar-mode -1)
;; ----

;; ----
;; 이 값을 -1 로 설정하면, 상단의 툴바가 사라집니다. (그림 아이콘들을 뜻합니다.)
(tool-bar-mode -1)
;; ----

;; ----
;; 이 값이 -1 이면, 화면 오른쪽의 스크롤 바가 가려집니다.
(scroll-bar-mode -1)
;; ----

;; ----
;; fringe(창 가장 왼쪽/오른쪽 그리고 버퍼를 나누는 기준선 양쪽의 작은 여백)를 없앱니다.
(fringe-mode 0)
;; ----

;; ----
;; 화면 아래쪽 상태 표시줄에 현재 시각을 표시합니다.
(display-time-mode 1)
;; ----

;; ----
;; 화면 아래쪽 상태 표시줄에 현재 배터리 상태를 표시합니다.
(display-battery-mode 1)
;; ----

;; ----
;; 하단 상태 표시줄에 줄 번호(위에서 몇 번째 줄인지)뿐만 아니라 열 번호(왼쪽에서 몇 번째 글자인지)도 표시합니다.
(column-number-mode 1)
;; ----

;; ----
;; 키보드 방향키로 스크롤할 때 화면 맨 위/아래에서 더 스크롤하면 반 페이지씩 당기지 않고 한 줄씩 당깁니다.
;; http://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs
(setq scroll-conservatively 10000)
;; ----

;; ----
;; 마우스로 스크롤할 때 한 라인씩 움직입니다.
(setq mouse-wheel-scroll-amount '(1))
;; 마우스 스크롤을 가속하지 않습니다.
(setq mouse-wheel-progressive-speed nil)
;; ----

;; ----
;; doc-view로 pdf 파일을 볼 때 위 아래 스크롤을 통해 페이지를 이동할 수 있습니다.
(setq doc-view-continuous t)
;; ----

;; ----
;; 이웃한 두 줄 사이의 작은 간격(행간 여백)을 픽셀 단위로 조정합니다.
(setq-default line-spacing 4)
;; ----

;; ----
;; 괄호를 열고 닫을 때 해당하는 짝을 표시해줍니다.
(show-paren-mode t)
;; ----

;; ----
;; 어두운 색감의 wombat 테마를 설정합니다.
(load-theme 'wombat t)
;; ----

;; ----
;; tooltip-mode를 끕니다. (Emacs의 툴바 아이콘이나 하단 상태 표시줄 등에 마우스를 가져다댄 뒤 기다리면 나타나는 그래픽 툴팁을 없애고 창 가장 아래에 나타나는 텍스트 설명으로 대체합니다.)
(tooltip-mode nil)
;; ----

;; ----
;; visual-line-mode를 설정하면 줄바꿈으로 인해 단어가 중간에서 잘리지 않고, '해당 줄 맨 앞으로 이동' 등의 기능을 쓸 때 논리적인 맨 앞이 아니라 보이는 줄의 맨 앞으로 이동합니다.
(global-visual-line-mode 1)
;; ----

;; ----
;; 창을 snap할 때 크기를 픽셀 단위로 딱 들어맞게 설정
;; https://emacs.stackexchange.com/questions/30420/resize-emacs-gui-window-to-exactly-half-the-screen
(setq frame-resize-pixelwise t)
;; ----

;; ----
;; 파일 백업 관련 설정입니다. 백업 파일을 한 디렉토리에 모으고, 여러 버전을 관리합니다.
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 100 ; 100 newest
   kept-old-versions 100 ; 100 oldest
   version-control t)       ; use versioned backups

;; backup each save, not the only first one after saved launching emacs
(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)
;; ----

;; 커서 모양을 block에서 I 모양으로 바꿉니다.
(setq-default cursor-type 'bar)
;; ----

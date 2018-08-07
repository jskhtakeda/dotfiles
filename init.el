;;; パッケージの設定 
(package-initialize) 
(setq package-archives 
      '(("gnu" . "http://elpa.gnu.org/packages/") 
        ("melpa" . "http://melpa.org/packages/") 
        ("org" . "http://orgmode.org/elpa/"))) 
;;; 右から左に読む言語に対応させないことで描画高速化 
(setq-default bidi-display-reordering nil) 
;;; splash screenを無効にする 
(setq inhibit-splash-screen t) 
;;; 同じ内容を履歴に記録しないようにする 
(setq history-delete-duplicates t) 
;; C-u C-SPC C-SPC ...でどんどん過去のマークを遡る 
(setq set-mark-command-repeat-pop t) 
;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する 
(require 'uniquify) 
;; filename<dir> 形式のバッファ名にする 
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) 
(setq uniquify-ignore-buffers-re "[^*]+") 
;;; ファイルを開いた位置を保存する 
(require 'saveplace) 
(setq-default save-place t) 
(setq save-place-file (concat user-emacs-directory "places")) 
;;; 釣合う括弧をハイライトする 
(show-paren-mode 1) 
;;; 現在行に色をつける 
;;(global-hl-line-mode 1) 
;;; ミニバッファ履歴を次回Emacs起動時にも保存する 
(savehist-mode 1) 
;;; シェルに合わせるため、C-hは後退に割り当てる 
(global-set-key (kbd "C-h") 'delete-backward-char) 
;;; 行番号・桁番号を表示する 
(line-number-mode 1) 
(column-number-mode 1) 
;;; GCを減らして軽くする 
(setq gc-cons-threshold (* 10 gc-cons-threshold)) 
;;; ログの記録行数を増やす 
(setq message-log-max 10000) 
;;; 履歴をたくさん保存する 
(setq history-length 1000) 
;;; メニューバーとツールバーを消す 
(menu-bar-mode -1) 
(tool-bar-mode -1) 
;;; スタートアップメッセージを表示させない 
(setq inhibit-startup-message t) 
;;; バックアップファイルの作成場所を一ヶ所にまとめる 
(setq backup-directory-alist '((".*" . "~/.ehist"))) 
;;; カーソル位置を固定する 
;; (and 
;; (require 'centered-cursor-mode) 
;; (global-centered-cursor-mode +1)) 
;;; デバッグする際の行の移動 
(global-set-key (kbd "M-g") 'goto-line) 
;; 改行コードを表示する 
;(setq eol-mnemonic-dos "(CRLF)") 
;(setq eol-mnemonic-mac "(CR)") 
;(setq eol-mnemonic-unix "(LF)") 
;; スペース、タブなどを可視化する 
;(global-whitespace-mode 1) 
;; C-kで行全体を削除する 
(setq kill-whole-line t) 
;; "yes or no" の選択を "y or n" にする 
(fset 'yes-or-no-p 'y-or-n-p) 
;; auto-complete（自動補完） 
(require 'auto-complete-config) 
(ac-config-default) 
;; font 
(add-to-list 'default-frame-alist '(font . "ricty-12")) 
;; color theme 
;;(load-theme 'monokai t) 
;; tabサイズ 
(setq default-tab-width 4) 
;; タイトルにフルパス表示 
(setq frame-title-format "%f") 
;; 自動改行と字下げ 
;;(setq c-auto-newline t) 
;; がっつり削除 
;;(global-hungry-delete-mode 1) 
;; 自動インテンドしない改行を設定 
(global-set-key (kbd "C-;") 'eval-print-last-sexp) 
;; 改行時の自動インデント 
(electric-indent-mode +1) 
;; カッコを虹色に表示 
(require 'rainbow-delimiters) 
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 画面のスクロール
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 1)))
;(put 'set-goal-column 'disabled nil)

;; 拡張子に対するモードの対応のカスタマイズ
(setq auto-mode-alist
	  (append '(("\\.l$" . lisp-interaction-mode))
			  auto-mode-alist))
(put 'downcase-region 'disabled nil)

;; フルパスをheader-lineに表示
;;(path-headerline-mode +1)

;; copyをemacs-ubuntu間で共有
(if (eq system-type 'darwin)
	(setq x-select-enable-clipboard t)
  (defun xsel-cut-function (text &optional push)
	(with-temp-buffer
	  (insert text)
	  (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  (defun xsel-paste-function()
	(let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
	  (unless (string= (car kill-ring) xsel-output)
		xsel-output )))
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function))

;; M-x g goto-line
(global-set-key "\M-g" 'goto-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "color-45"))))
 '(comint-highlight-prompt ((t (:foreground "color-45")))))


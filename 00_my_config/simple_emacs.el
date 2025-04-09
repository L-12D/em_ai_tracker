;;; simple_emacs.el --- Мой минималистичный и понятный конфиг Emacs -*- lexical-binding: t; -*-

;; Этот конфиг создан как учебный и практический проект.
;; Он включает в себя все базовые настройки Emacs, которые делают работу проще и понятнее.
;; Я одновременно изучаю Emacs и упрощаю его, добавляя горячие клавиши и объяснения к ним.

;; ------------------------------
;; 📌 Список горячих клавиш
;; ------------------------------
;; C-c r     — обновить конфиг без перезапуска (simple/reload-config)
;; C-c c y   — открыть *scratch* буфер (simple/open-scratch)
;; C-c t o   — открыть файл задач tasks.org (simple/open-tasks-file)
;; C-c t i   — открыть файл задач inbox_tasks.org (simple/open-inbox-file)
;; C-c q     — закрыть все окна и буферы, кроме текущего (my/cleanup-workspace)
;; RET       — в строке чек-листа вставляет новую строку - [ ]
;; Delete    — удаляет символ под курсором (вместо backspace-подобного поведения)
;; Ctrl+S    — сохранить текущий буфер (save-buffer)
;; C-c m     — открыть файл со словарём хоткеев (my/open-shortcuts-dict) [добавим ниже]


;; ------------------------------
;; 1. Упрощение управления: CUA Mode
;; ------------------------------
(cua-mode 1)

;; Назначаем Ctrl+S для сохранения буфера (на случай, если отключено)
(global-set-key (kbd "C-s") #'save-buffer)

;; ------------------------------
;; 2. Жёсткий перенос строк
;; ------------------------------
(setq-default fill-column 100)
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'org-mode-hook  #'turn-on-auto-fill)
(require 'display-fill-column-indicator)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook  #'display-fill-column-indicator-mode)


;; ------------------------------
;; 2.1 ... и мягкий тоже 
;; ------------------------------

(add-hook 'org-mode-hook #'visual-line-mode)


;; ------------------------------
;; 3. Хранение бэкапов и автосейвов в отдельной папке
;; ------------------------------
;; 📦 Бэкапы в отдельную папку, без попытки изменить ACL
(setq backup-by-copying t) ;; <-- новая строка

(make-directory (expand-file-name "../../04_archive/backups" (file-name-directory load-file-name)) t)
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "../../04_archive/backups" (file-name-directory load-file-name)))))


;; 📦 Автосейвы в отдельную папку, без попытки изменить ACL
(make-directory (expand-file-name "../../04_archive/autosaves" (file-name-directory load-file-name)) t)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "../../04_archive/autosaves" (file-name-directory load-file-name)) t)))

;; ------------------------------
;; 4. Обновление конфига без перезапуска
;; ------------------------------
(defvar my-config-base-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Базовая директория для конфига.")

(defun simple/reload-config ()
  "Загружает init.el без перезапуска Emacs."
  (interactive)
  (let ((init-file (expand-file-name "../init.el" my-config-base-dir)))
    (load-file init-file)
    (message "✅ init.el обновлён: %s" init-file)))

(global-set-key (kbd "C-c r") #'simple/reload-config)

;; ------------------------------
;; 5. Шорткат для открытия *scratch*
;; ------------------------------
(defun simple/open-scratch ()
  "Открывает *scratch* буфер. Если его нет — создаёт."
  (interactive)
  (switch-to-buffer "*scratch*")
  (unless (eq major-mode 'lisp-interaction-mode)
    (lisp-interaction-mode)))

(global-set-key (kbd "C-c c y") #'simple/open-scratch)

;; ------------------------------
;; 6. Шорткат для открытия файла задач
;; ------------------------------

(defvar my-config-base-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Базовая директория для конфига.")

(global-set-key (kbd "C-c t o") (lambda ()
                                  (interactive)
                                  (let ((file-path (expand-file-name "../../task-tracker/tasks.org" my-config-base-dir)))
                                    (find-file file-path))))


;; ------------------------------
;; 6.1 Шорткат для открытия файла inbox
;; ------------------------------

(global-set-key (kbd "C-c t i") (lambda ()
                                  (interactive)
                                  (let ((file-path (expand-file-name "../../task-tracker/inbox_tasks.org" my-config-base-dir)))
                                    (find-file file-path))))


;; ------------------------------
;; 7. Умное поведение RET в чек-листах
;; ------------------------------
(defun simple/org-return-dwim ()
  "Если курсор в строке чек-листа, вставляет новую строку с '- [ ]'. Иначе вызывает стандартное поведение RET."
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (save-excursion
             (beginning-of-line)
             (looking-at "^\\s-*[-*] \\[.\\] ")))
      (progn
        (end-of-line)
        (insert "\n- [ ] "))
    (org-return)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "RET") #'simple/org-return-dwim))

;; ------------------------------
;; 8. Шорткат для очистки рабочей области
;; ------------------------------
(defun my/cleanup-workspace ()
  "Закрыть все окна и буферы, кроме текущего.
Полезно, когда открыто много всего, а хочется сосредоточиться на одной задаче."
  (interactive)
  (delete-other-windows)
  (kill-other-buffers))

(global-set-key (kbd "C-c q") #'my/cleanup-workspace)

;; ------------------------------
;; 9. Настройка Delete как forward-delete
;; ------------------------------
(global-set-key [delete] 'delete-char)

;; ------------------------------
;; 10. Формирование словаря шорт-катов
;; ------------------------------

(defun my/generate-shortcuts-dictionary (source-dir output-file)
  "Собирает описания из .el-файлов в SOURCE-DIR и пишет их в OUTPUT-FILE (Org-файл).
Описание берётся из начальных комментариев (до первой функции/настройки)."
  (interactive
   (list
    (read-directory-name "Директория с конфигами: " "~/.emacs.d/")
    (read-file-name "Файл для записи результатов: " "~/.emacs.d/" "dict_shortcat.org")))
  (let ((files (directory-files-recursively source-dir "\\.el$"))
        (found-entries '()))
    (dolist (file files)
      (let (this-file-header)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Собираем только начальные комментарии — строки, начинающиеся с ;
          ;; до первой "настоящей" строки кода
          (while (and (not (eobp))
                      (looking-at "^\\s-*\\(;\\|$\\)"))
            (let ((line (string-trim
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))))
              (when (and (not (string= line ""))
                         (string-prefix-p ";" line)
                         ;; Игнорируем технические директивы вроде lexical-binding, provide и т.п.
                         (not (string-match-p "lexical-binding\\|provide\\|\\*-" line))
                         ;; Игнорируем заголовки вроде "----"
                         (not (string-match-p "^;+\\s-*[-─—]+" line)))
                (push (string-trim (replace-regexp-in-string "^;+\\s-*" "" line))
                      this-file-header)))
            (forward-line 1)))
        (when this-file-header
          (push (list file (reverse this-file-header)) found-entries))))
    ;; Пишем в org-файл
    (with-temp-buffer
      (insert "#+TITLE: Словарь горячих клавиш\n")
      (insert (format "# Генерировано: %s\n\n" (current-time-string)))
      (dolist (entry (reverse found-entries))
        (let ((file-name (file-name-nondirectory (nth 0 entry)))
              (desc-lines (nth 1 entry)))
          (insert (format "* %s\n" file-name))
          (dolist (line desc-lines)
            (insert (format ": %s\n" line)))
          (insert "\n")))
      (write-region (point-min) (point-max) output-file))
    (message "✅ Словарь описаний сгенерирован: %s" output-file)))



(global-set-key (kbd "C-c m") #'my/generate-and-open-shortcuts-dict)

(provide 'simple_emacs)

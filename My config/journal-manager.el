;;; journal-manager.el --- Управление ежедневным дневником через org-journal -*- lexical-binding: t; -*-

;; Этот модуль управляет созданием и открытием ежедневных заметок в дневнике.
;; Мы не используем `org-journal-new-entry`, чтобы вручную контролировать структуру.
;; При первом запуске Emacs сам создает структуру дневника и подключает правильную директорию.
;; Поддерживается интеграция с org-agenda.

;; ------------------------------
;; ⚙️  Горячие клавиши
;; ------------------------------
;; C-c j     — Открыть или создать дневник за сегодня (my/journal-open-today)
;;
;; 🧾 Внутри файла:
;; - * Справочник → шорт-каты — C-c m

;;; 📎 Инструкция для агентов / автогенераторов конфигов
;; Чтобы ваша конфигурация поддерживалась моим генератором словаря шорткатов:
;; (global-set-key (kbd "C-c j") #'my/journal-open-today) ;; открыть дневник

;;; Код:

(require 'org)
(require 'org-journal)

;; ------------------------------
;; 1. Конфигурация локально здесь
;; ------------------------------

;; Явно задаём директорию для файлов дневника
(setq org-journal-dir "E:/YandexDisk/2 area/10 Emacs/New managment emacs/journal/")

;; Формат имени файла (по умолчанию YYYYMMDD)
(setq org-journal-file-format "%Y%m%d.org")

;; Отключаем вставку автозаголовков
(setq org-journal-file-header nil)

;; Интеграция с org-agenda (чтобы задачи из дневника попадали в повестку)
(setq org-journal-enable-agenda-integration t)

;; Переопределим директорию бэкапов, чтобы избежать ACL-проблем
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; Также добавим journal-dir в org-agenda-files, если он ещё не там
(unless (member org-journal-dir org-agenda-files)
  (add-to-list 'org-agenda-files org-journal-dir))


;; ------------------------------
;; 2. Создание и открытие дневника
;; ------------------------------

(defun my/journal-open-today ()
  "Открыть или создать дневник за сегодня.
Создаёт файл с заголовком, секцией * Справочник и * Notes, если файл не существует."
  (interactive)
  (let* ((filename (format-time-string org-journal-file-format))
         (full-path (expand-file-name filename org-journal-dir)))
    ;; Убедимся, что директория существует
    (unless (file-directory-p org-journal-dir)
      (make-directory org-journal-dir t))
    (if (file-exists-p full-path)
        (find-file full-path)
      (find-file full-path)
      (insert (format "#+title: %s\n\n" (format-time-string "%A, %d %B %Y")))
      (insert "* Справочник\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
              "- шорт-каты — C-c m\n\n"
              "* Notes\n")
      (save-buffer)
      (message "Создан новый дневник за сегодня"))))

(global-set-key (kbd "C-c j") #'my/journal-open-today) ;; открыть дневник

(provide 'journal-manager)

;;; journal-manager.el ends here

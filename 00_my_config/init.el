;;; init.el --- Основной конфигурационный файл Emacs

;; ------------------------------
;; 📁 Пути
;; ------------------------------

(add-to-list 'load-path "./")

;; ------------------------------
;; 📦 Установка и настройка use-package
;; ------------------------------

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (message "📦 Устанавливаю use-package...")
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ------------------------------
;; ⚙️ Базовые настройки Org
;; ------------------------------

(setq org-log-repeat 'time)
(setq org-todo-repeat-to-state "TODO")
(setq org-transclusion-read-only nil)
(require 'org-transclusion)

;; ------------------------------
;; 🧠 Модули задач (task manager)
;; ------------------------------

(load-file "task-manager/task_manager-create_task.el")
(load-file "task-manager/task_manager-organizing_tasks.el")
(load-file "task-manager/task_manager-inbox_task.el")

;; ------------------------------
;; 📓 Личный дневник и общие настройки
;; ------------------------------

(load "journal-manager")
(load "simple_emacs")

;; ------------------------------
;; 💾 Настройки, созданные через M-x customize
;; ------------------------------

(setq custom-file "custom.el")
(load custom-file 'noerror)

;; ------------------------------
;; 📅 Файлы для org-agenda
;; ------------------------------

(setq org-agenda-files
      (list
       "../task-tracker/tasks.org"))
(setq org-agenda-files (append org-agenda-files (directory-files-recursively "../03_journal/" "\\.org$")))

;; ------------------------------
;; 📦 Пакеты (если используешь use-package)
;; ------------------------------

;; (require 'package)
;; (package-initialize)

;; (use-package vertico :ensure t)
;; (use-package orderless :ensure t)
;; (use-package org-journal :ensure t)
;; (use-package org-transclusion :ensure t)
;; (use-package which-key :ensure t)
;; (use-package markdown-mode :ensure t)
;; (use-package telega :ensure t)
;; (use-package modus-themes :ensure t)
;; (use-package evil :ensure t)

;;; init.el ends here

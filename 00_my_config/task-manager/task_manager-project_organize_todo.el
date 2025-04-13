;;; task_manager-project_organize.el --- Организация задач по проектам для трекера -*- lexical-binding: t; -*-

;;; Комментарий:
;; Этот модуль предоставляет механизм переноса задач из INBOX в файл `tasks.org`
;; с автоматическим выбором проекта. Поддерживаются сценарии:
;; - Перенос задачи в существующий проект
;; - Создание нового проекта
;; - Использование задачи как проекта (опция «Без проекта»)
;;
;; Задача получает статус TODO и добавляется под нужный заголовок проекта.
;; Это позволяет структурировать backlog по активным направлениям.

;; ------------------------------
;; 🔧 Конфигурируемые переменные
;; ------------------------------
;; Эти переменные должны быть определены в общем конфиге или другом модуле:
;; - `my/tasks-file` — путь к `tasks.org`
;; - `my/inbox-file` — путь к `inbox_tasks.org`

;; ------------------------------
;; ⚙️ Панель горячих клавиш
;; ------------------------------
;; (global-set-key (kbd "C-c t v") 'my/move-inbox-to-todo) ;; переместить INBOX-задачу в TODO с выбором проекта

;; ------------------------------
;; 📎 Инструкция для агентов / автогенераторов конфигов
;; ------------------------------
;; Чтобы ваша конфигурация поддерживалась моим генератором словаря шорткатов:
;;
;; 1. Горячие клавиши должны быть записаны в одной строке:
;;    (global-set-key (kbd "C-c t v") 'my/move-inbox-to-todo) ;; переместить INBOX-задачу в TODO с проектом
;;
;; 2. Или через define-key:
;;    (define-key some-map (kbd "C-c t v") 'my/move-inbox-to-todo)
;;
;; 3. Регулярное выражение для поиска таких строк:
;;    ^\\s-*\\((\\(global-set-key\\|define-key\\).*\\)$
;;
;; 4. Комментарий (если он есть) желательно добавлять в той же строке:
;;    ;; краткое описание действия шортката

;; ------------------------------
;; 1. Получение проектов из tasks.org
;; ------------------------------

(defun my/get-projects ()
  "Получить список всех проектов из `tasks.org` (заголовки вида '* Проект: ...')."
  (with-current-buffer (find-file-noselect my/tasks-file)
    (goto-char (point-min))
    (let (projects)
      (while (re-search-forward "^\\* Проект: \\(.+\\)$" nil t)
        (push (match-string 1) projects))
      (nreverse projects))))

;; ------------------------------
;; 2. Перемещение задачи в TODO
;; ------------------------------

(defun my/move-inbox-to-todo ()
  "Перевести задачу из INBOX в TODO и переместить её из `inbox_tasks.org` в `tasks.org` с выбором проекта.
Пользователь может выбрать:
- Существующий проект;
- Новый проект;
- Или 'Без проекта' — в этом случае заголовок проекта создаётся на основе названия задачи."
  (interactive)
  (org-back-to-heading t)
  (let ((task-start (point))
        (task-end   (save-excursion (org-end-of-subtree) (point)))
        (inbox-buffer (current-buffer))
        (tasks-buffer (find-file-noselect my/tasks-file)))
    (if (not (string= (org-get-todo-state) "INBOX"))
        (message "❌ Ошибка: задача уже не в статусе INBOX")
      (let* ((projects (append (my/get-projects) '("Без проекта" "Новый проект")))
             (selected-project (completing-read "Выберите проект: " projects nil nil)))
        ;; Вырезаем задачу
        (kill-region task-start task-end)
        ;; Перемещаем в tasks.org
        (with-current-buffer tasks-buffer
          (goto-char (point-min))
          (cond
           ;; Новый проект
           ((string= selected-project "Новый проект")
            (let ((new-project-name (read-string "Введите название нового проекта: ")))
              (goto-char (point-max))
              (insert (format "\n* Проект: %s\n" new-project-name))
              (insert (replace-regexp-in-string "^\\* INBOX" "* TODO" (current-kill 0)))))

           ;; Без проекта — проект называется как задача
           ((string= selected-project "Без проекта")
            (goto-char (point-max))
            (let ((task-title
                   (replace-regexp-in-string
                    "^\\* INBOX \\(\\[#[A-C]\\] \\)?\\(.+?\\)\\( :.+:\\)?$"
                    "\\2" (current-kill 0))))
              (insert (format "\n* Проект: %s\n" task-title))
              (insert (replace-regexp-in-string "^\\* INBOX" "* TODO" (current-kill 0)))))

           ;; Существующий проект
           (t
            (if (re-search-forward (format "^\\* Проект: %s$" (regexp-quote selected-project)) nil t)
                (progn
                  (forward-line 1)
                  (insert (replace-regexp-in-string "^\\* INBOX" "* TODO" (current-kill 0))))
              ;; Если проект по какой-то причине не найден — создаём заново
              (goto-char (point-max))
              (insert (format "\n* Проект: %s\n" selected-project))
              (insert (replace-regexp-in-string "^\\* INBOX" "* TODO" (current-kill 0))))))
          (save-buffer))
        (with-current-buffer inbox-buffer
          (save-buffer))
        (message "✅ Задача переведена в TODO и перемещена в tasks.org")))))

;; ------------------------------
;; 3. Горячая клавиша
;; ------------------------------
(global-set-key (kbd "C-c t v") 'my/move-inbox-to-todo) ;; переместить задачу в TODO с проектом

(provide 'task_manager-project_organize)
;;; task_manager-project_organize.el ends here

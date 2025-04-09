;;; task_manager-move_tasks.el --- Перенос задач между INBOX и TODO -*- lexical-binding: t; -*-
;;; Комментарий:
;; Этот модуль предоставляет функции для перевода задач между статусом INBOX и TODO,
;; а также перемещения их между соответствующими файлами `inbox_tasks.org` и `tasks.org`.
;; Идея в том, чтобы иметь единый хоткей и «умную» команду, которая сама решает, куда нужно
;; переместить задачу (из INBOX в TODO или наоборот).
;; 📌 В будущем этот файл будет расширен:
;; - поддержкой выбора проекта при переносе задач из INBOX в tasks.org;
;; - автоподстановкой заголовков проектов и возможной фильтрацией задач по ним.

;; ------------------------------
;; 🔧 Конфигурируемые переменные
;; ------------------------------

(defconst my/tasks-file
  "E:\\YandexDisk\\2 area\\10 Emacs\\New managment emacs\\task-tracker\\tasks.org"
  "Путь к файлу tasks.org для задач в статусе TODO.")

;; ------------------------------
;; ⚙️ Панель горячих клавиш
;; ------------------------------
;; (global-set-key (kbd "C-c t v") 'my/move-task-smart) ;; умное перенесение задачи

;; ------------------------------
;; 📎 Инструкция для агентов / автогенераторов конфигов
;; ------------------------------
;; Чтобы ваша конфигурация поддерживалась моим генератором словаря шорткатов:
;;
;; 1. Горячие клавиши должны быть записаны в одной строке:
;;    (global-set-key (kbd "C-c t v") 'my/move-task-smart) ;; умное перенесение задачи
;;
;; 2. Или через define-key:
;;    (define-key some-map (kbd "C-c t v") 'my/move-task-smart) ;; умное перенесение задачи
;;
;; 3. Регулярное выражение для поиска таких строк:
;;    ^\\s-*\\((\\(global-set-key\\|define-key\\).*\\)$
;;
;; 4. Комментарий (если он есть) желательно добавлять в той же строке:
;;    ;; короткое описание назначения сочетания клавиш

;; ------------------------------
;; 1. Функции переноса задач
;; ------------------------------

(defun my/move-inbox-to-todo ()
  "Перевести задачу из INBOX в TODO и переместить её из `inbox_tasks.org` в `tasks.org`.
- Проверяет, что задача действительно имеет статус INBOX.
- Вырезает поддерево текущего заголовка и вставляет его в секцию
  \"* Tasks\" файла `tasks.org`, меняя заголовок с \"* INBOX\" на \"* TODO\".
- Если раздел \"* Tasks\" отсутствует, он автоматически добавляется."
  (interactive)
  (org-back-to-heading t)
  (let ((task-start (point))
        (task-end   (save-excursion (org-end-of-subtree) (point)))
        (inbox-buf  (current-buffer))
        (tasks-buf  (find-file-noselect my/tasks-file)))
    ;; Проверяем статус задачи
    (if (not (string= (org-get-todo-state) "INBOX"))
        (message "❌ Ошибка: задача уже не в статусе INBOX")
      ;; Вырезаем поддерево задачи
      (kill-region task-start task-end)
      ;; Переключаемся в буфер с TODO-задачами и вставляем
      (with-current-buffer tasks-buf
        (goto-char (point-min))
        (if (re-search-forward "^\\* Tasks" nil t)
            (forward-line 1)
          ;; Если секции нет, создаём
          (goto-char (point-min))
          (insert "* Tasks\n"))
        ;; Меняем заголовок с INBOX на TODO и вставляем
        (insert
         (replace-regexp-in-string
          "^\\* INBOX" "* TODO" (current-kill 0)))
        (save-buffer))
      ;; Возвращаемся в исходный буфер и сохраняем его
      (with-current-buffer inbox-buf
        (save-buffer))
      (message "✅ Задача переведена в TODO и перемещена в tasks.org"))))

(defun my/move-todo-to-inbox ()
  "Перевести задачу из любого статуса (кроме INBOX) в INBOX
и переместить её из `tasks.org` в `inbox_tasks.org`.
- Проверяет, что задача не в статусе INBOX.
- Вырезает поддерево текущего заголовка и вставляет его в секцию
  \"* Tasks\" файла `inbox_tasks.org`, меняя заголовок на \"* INBOX\".
- Если раздел \"* Tasks\" отсутствует, он автоматически добавляется."
  (interactive)
  (org-back-to-heading t)
  (let ((task-start (point))
        (task-end   (save-excursion (org-end-of-subtree) (point)))
        (tasks-buf  (current-buffer))
        (inbox-buf  (find-file-noselect my/inbox-file)))
    (if (string= (org-get-todo-state) "INBOX")
        (message "❌ Ошибка: задача уже в статусе INBOX")
      ;; Вырезаем поддерево
      (kill-region task-start task-end)
      ;; Переключаемся в буфер INBOX и вставляем
      (with-current-buffer inbox-buf
        (goto-char (point-min))
        (if (re-search-forward "^\\* Tasks" nil t)
            (forward-line 1)
          ;; Если секции нет, создаём
          (goto-char (point-min))
          (insert "* Tasks\n"))
        (insert
         (replace-regexp-in-string
          "^\\* [A-Z]+"
          "* INBOX"
          (current-kill 0)))
        (save-buffer))
      (with-current-buffer tasks-buf
        (save-buffer))
      (message "✅ Задача переведена в INBOX и перемещена в inbox_tasks.org"))))

(defun my/move-task-smart ()
  "Умная функция для переключения статуса задачи между INBOX и TODO.
Если задача уже INBOX, тогда вызывает `my/move-inbox-to-todo`.
Иначе — вызывает `my/move-todo-to-inbox`."
  (interactive)
  (if (string= (org-get-todo-state) "INBOX")
      (my/move-inbox-to-todo)
    (my/move-todo-to-inbox)))

;; Пример привязки к хоткею C-c t v
(global-set-key (kbd "C-c t v") 'my/move-task-smart) ;; умное перенесение задачи

(provide 'task_manager-move_tasks)

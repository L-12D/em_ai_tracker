;;; task_manager-create_task.el --- Создание задач в tasks.org с ID, DEADLINE и статусами -*- lexical-binding: t; -*-

;; 📌 Цель:
;; Упрощённое создание задач в GTD-системе.
;; Поддерживает ID, DEADLINE, SCHEDULED, приоритеты, чек-листы и выбор тегов.
;; Сохраняет задачи в секцию * Tasks файла tasks.org.

;; ------------------------------
;; ⚙️  Keybindings
;; ------------------------------
;; C-c t c — открыть список всех статусов (через org-todo)
;; C-c t 1 — установить статус INBOX
;; C-c t 2 — установить статус TODO
;; C-c t 3 — установить статус DONE
;; C-c t 4 — установить статус LIFE (ручной выбор, вне основного цикла)
;; C-c t t — создать полноценную задачу с дедлайнами и тегами (my/create-task)
;; C-c t e — быстрое добавление INBOX-задачи (my/quick-capture-task)

;; ------------------------------
;; 📎 Инструкция для агентов / автогенераторов конфигов
;; ------------------------------
;; (global-set-key (kbd "C-c t c") 'org-todo)
;; (global-set-key (kbd "C-c t 1") (lambda () (interactive) (org-todo "INBOX")))
;; (global-set-key (kbd "C-c t 2") (lambda () (interactive) (org-todo "TODO")))
;; (global-set-key (kbd "C-c t 3") (lambda () (interactive) (org-todo "DONE")))
;; (global-set-key (kbd "C-c t 4") (lambda () (interactive) (org-todo "LIFE")))
;; (global-set-key (kbd "C-c t t") 'my/create-task)
;; (global-set-key (kbd "C-c t e") 'my/quick-capture-task)

;; ------------------------------
;; ✅ Шаг 0: Определение пользовательских статусов задач
;; ------------------------------

(setq org-todo-keywords
      '((sequence "INBOX" "TODO" "DONE")
        (sequence "LIFE"))) ;; LIFE теперь не участвует в автоматическом цикле C-c C-t

(setq org-log-repeat 'time)              ;; логирование времени при повторе
(setq org-todo-repeat-to-state "TODO")   ;; при повторении статус возвращается в TODO

;; ------------------------------
;; 🗂️ Пути к файлам
;; ------------------------------

(defvar my/inbox-task-file
  "../../task-tracker/inbox_tasks.org"
  "Основной файл, в который добавляются задачи.")

(defvar my/tags-file
  (expand-file-name "tags.org" (file-name-directory my/inbox-task-file))
  "Файл с доступными тегами.")

;; ------------------------------
;; 🔢 Шаг 1: Генерация уникального ID
;; ------------------------------

(defun my/generate-task-id ()
  "Сгенерировать уникальный ID на основе текущей даты и времени."
  (format-time-string "%Y%m%d-%H%M%S"))

;; ------------------------------
;; 🏷️ Шаг 2: Работа с тегами
;; ------------------------------

(defun my/read-tags-from-file ()
  "Прочитать список тегов из файла `tags.org`."
  (when (file-exists-p my/tags-file)
    (with-temp-buffer
      (insert-file-contents my/tags-file)
      (let ((tags '()))
        (org-mode)
        (org-map-entries
         (lambda ()
           (push (org-get-heading t t t t) tags)))
        (reverse tags)))))

(defun my/add-tag-to-file (tag)
  "Добавить TAG в файл `tags.org`, если его ещё нет."
  (unless (member tag (my/read-tags-from-file))
    (with-current-buffer (find-file-noselect my/tags-file)
      (goto-char (point-max))
      (insert "* " tag "\n")
      (save-buffer))))

(defun my/select-tags ()
  "Выбрать один или несколько тегов с возможностью добавить новый."
  (let ((selected '())
        (available (my/read-tags-from-file))
        (done nil))
    (while (not done)
      (let* ((choice (completing-read
                      (format "🏷️ Теги выбраны: %s\nВыбери тег (или введи новый, Enter — завершить): "
                              (if selected (string-join selected ", ") "—"))
                      available nil nil)))
        (if (string-empty-p choice)
            (setq done t)
          (progn
            (add-to-list 'selected choice)
            (my/add-tag-to-file choice)))))
    selected))

;; ------------------------------
;; ✍️ Шаг 3: Основная функция создания задачи
;; ------------------------------
(defun my/create-task ()
  "Создать новую задачу, где DEADLINE и SCHEDULED идут в одну строку,
с русским днём недели, и приоритет записывается как [#A] (и #+PRIORITY: A).
Пользователь может выбрать, устанавливать ли DEADLINE и SCHEDULED через '1' или '2'.
Для SCHEDULED поддерживается повторяемость (например, ++1w)."
  (interactive)

  ;; 1) Ввод названия и приоритета (А/Б/С/Г)
  (let* ((title-raw (read-string "Название задачи (с приоритетом А/Б/В/Г в конце): "))
         (priority-char (when (string-match "[[:space:]]\\([АБВГабвг]\\)$" title-raw)
                          (upcase (match-string 1 title-raw))))
         (priority-code
          (pcase priority-char
            ("А" "A") ("Б" "B") ("В" "B") ("С" "B") ("Г" "C") (_ nil)))
         (title (if priority-code
                    (string-trim-right title-raw "[[:space:]][АБВГабвг]$")
                  title-raw))
         (bracket-prio (if priority-code (format " [#%s]" priority-code) ""))
         (priority-prop (if priority-code (format "#+PRIORITY: %s\n" priority-code) ""))

         ;; 2) Выбор установки DEADLINE и SCHEDULED через '1' или '2'
         (set-deadline (eq (read-char-choice "Установить DEADLINE? (1 - да, 2 - нет): " '(?1 ?2)) ?1))
         (deadline-time (if set-deadline (org-read-date nil t nil "Выберите DEADLINE: ") nil))
         (set-scheduled (eq (read-char-choice "Установить SCHEDULED? (1 - да, 2 - нет): " '(?1 ?2)) ?1))
         (scheduled-time (if set-scheduled (org-read-date nil t nil "Выберите SCHEDULED: ") nil))
         ;; Добавляем запрос повторяемости для SCHEDULED
         (scheduled-repeat (if set-scheduled
                               (let ((raw (read-string "Повторяемость (например 1w или 3d, Enter — без повторения): ")))
                                 (if (string-empty-p raw) ""
                                   (if (string-match-p "^\\+\\+" raw) raw (concat "++" raw))))
                             ""))

         ;; Список русских дней недели
         (rus-days '("Пн" "Вт" "Ср" "Чт" "Пт" "Сб" "Вс"))

         ;; Форматируем DEADLINE и SCHEDULED в одну строку с учетом повторяемости
         (deadline-sched-line
          (cond
           ((and (null deadline-time) (null scheduled-time)) "")
           ((and deadline-time (null scheduled-time))
            (format "DEADLINE: <%s %s>"
                    (format-time-string "%Y-%m-%d" deadline-time)
                    (nth (1- (nth 6 (decode-time deadline-time))) rus-days)))
           ((and (null deadline-time) scheduled-time)
            (format "SCHEDULED: <%s %s%s>"
                    (format-time-string "%Y-%m-%d" scheduled-time)
                    (nth (1- (nth 6 (decode-time scheduled-time))) rus-days)
                    (if (string-empty-p scheduled-repeat) "" (concat " " scheduled-repeat))))
           (t (format "DEADLINE: <%s %s> SCHEDULED: <%s %s%s>"
                      (format-time-string "%Y-%m-%d" deadline-time)
                      (nth (1- (nth 6 (decode-time deadline-time))) rus-days)
                      (format-time-string "%Y-%m-%d" scheduled-time)
                      (nth (1- (nth 6 (decode-time scheduled-time))) rus-days)
                      (if (string-empty-p scheduled-repeat) "" (concat " " scheduled-repeat))))))

         ;; 3) Генерируем ID и статус
         (id (my/generate-task-id))
         (status "INBOX")

         ;; 4) Чек-лист
         (checklist-lines '())
         (item "")
         (_ (while (not (string-empty-p
                         (setq item (read-string "📋 Пункт чек-листа (Enter — завершить): "))))
              (push (format "- [ ] %s" item) checklist-lines)))
         (checklist (if checklist-lines (concat (string-join (reverse checklist-lines) "\n") "\n") ""))

         ;; 5) Теги
         (chosen-tags (my/select-tags))
         (tag-string (if chosen-tags (format ":%s:" (string-join chosen-tags ":")) ""))

         ;; 6) Итоговая сборка
         (entry (concat "* " status bracket-prio " " title " " tag-string "\n"
                        (unless (string-empty-p deadline-sched-line) (concat deadline-sched-line "\n"))
                        ":PROPERTIES:\n"
                        ":ID:       " id "\n"
                        ":END:\n"
                        priority-prop
                        "#+DESCRIPTION:\n#+LINKS:\n"
                        checklist)))

    ;; 7) Вставка в файл
    (with-current-buffer (find-file-noselect my/inbox-task-file)
      (goto-char (point-min))
      (if (re-search-forward "^\\* Tasks" nil t)
          (forward-line 1)
        (goto-char (point-min))
        (insert "* Tasks\n"))
      (insert entry "\n")
      (save-buffer))

    (message "✅ Задача добавлена: %s (ID: %s)" title id)))

;; ------------------------------
;; 🏃‍♂️ Шаг 4: Быстрое создание задачи через C-c t e
;; ------------------------------

(defun my/quick-capture-task ()
  "Быстрое создание INBOX-задачи. Минимум полей: только название (и приоритет).\nМожно при желании указать SCHEDULED (одну дату), и чек-лист. Без повторяемости."  
  (interactive)
  (let* ((title-raw (read-string "Название задачи (опц. приоритет А/Б/В/Г в конце): "))
         ;; Используем более надёжный шаблон [[:space:]]
         (priority-char (when (string-match "[[:space:]]\([АБВГабвг]\)$" title-raw)
                          (upcase (match-string 1 title-raw))))
         (priority-code (pcase priority-char
                          ("А" "A") ("Б" "B") ("В" "C") ("Г" "D") (_ nil)))
         (title (if priority-code
                    (string-trim-right title-raw "[[:space:]][АБВГабвг]$")
                  title-raw))
         (priority (if priority-code
                       (format "#+PRIORITY: %s\n" priority-code)
                     ""))

         ;; SCHEDULED (одна дата, без повторов)
         (scheduled-date (org-read-date nil nil nil "SCHEDULED (или Enter, если не нужно):"))
         (scheduled (if (string-empty-p scheduled-date)
                        ""
                      (format "SCHEDULED: <%s>\n" scheduled-date)))

         ;; Генерация ID
         (id (my/generate-task-id))

         ;; Чек-лист (опциональный)
         (checklist-lines '())
         (item "")
         (_ (while (not (string-empty-p
                         (setq item (read-string "\U0001F4CB Пункт чек-листа (Enter — завершить): "))))
              (push (format "- [ ] %s" item) checklist-lines)))
         (checklist (if checklist-lines
                        (concat (string-join (reverse checklist-lines) "\n") "\n")
                      ""))

         ;; Сборка записи
         (entry (concat "* INBOX " title "\n"
                        ":PROPERTIES:\n"
                        ":ID:       " id "\n"
                        ":END:\n"
                        priority
                        scheduled
                        "#+DESCRIPTION:\n#+LINKS:\n"
                        checklist)))

    ;; Вставка в файл
    (with-current-buffer (find-file-noselect my/inbox-task-file)
      (goto-char (point-min))
      (if (re-search-forward "^\\* Tasks" nil t)
          (forward-line 1)
        (goto-char (point-min))
        (insert "* Tasks\n"))
      (insert entry "\n")
      (save-buffer))

    (message "\u26A1 Быстрая задача добавлена: %s" title)))



;; ------------------------------
;; 🔗 Горячие клавиши
;; ------------------------------

(global-set-key (kbd "C-c t c") 'org-todo)
(global-set-key (kbd "C-c t 1") (lambda () (interactive) (org-todo "INBOX")))
(global-set-key (kbd "C-c t 2") (lambda () (interactive) (org-todo "TODO")))
(global-set-key (kbd "C-c t 3") (lambda () (interactive) (org-todo "DONE")))
(global-set-key (kbd "C-c t 4") (lambda () (interactive) (org-todo "LIFE")))
(global-set-key (kbd "C-c t t") 'my/create-task)
(global-set-key (kbd "C-c t e") 'my/quick-capture-task)

(provide 'task_manager-create_task)

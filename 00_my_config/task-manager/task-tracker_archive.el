;;; task-tracker.el --- Трекер задач с дневником через org-transclusion -*- lexical-binding: t; -*-

;;; Комментарии:
;; Этот модуль реализует локальный трекер задач, привязанный к ежедневному дневнику.
;; Используется org-transclusion для отображения задач с DEADLINE/SCHEDULED = сегодня
;; прямо в файле дневника.
;;
;; Основной сценарий:
;; 1. Все задачи хранятся в отдельном файле `tasks.org` (переменная `task-tracker-tasks-file`).
;; 2. Задачи создаются с уникальным ID, DEADLINE, SCHEDULED и (при желании) чек-листом.
;; 3. В файл дневника можно вставить задачи на сегодня как org-transclusion-блоки.
;; 4. Трансклюзии редактируемы через `org-transclusion-live-sync-start`.

;; Новое поведение (апрель 2025):
;; - DONE-задачи автоматически переносятся в `task-archive.org`
;; - задачи без DEADLINE получают статус INBOX
;; - задачи со статусом LIFE переносятся в `life-task.org`
;; - лягушки (отложенные TODO >2 дней) получают тег :frog:
;; - D-приоритет считается support (идёт в отдельный блок 'Support Tasks')
;; - есть приоритеты A/B/C/D с иконками ✦✿•○
;; - можно указывать статус/приоритет в названии задачи ('A i' → приоритет A, статус INBOX)
;; - при создании задачи заполняются поля `#+DESCRIPTION:` и `#+LINKS:`
;; - сортировка и форматирование задач при запуске Emacs и вручную (C-c t s):
;;   1) :planning:
;;   2) :blog:, :fitness:, :relationships: и т.д.
;;   3) прочие
;;   внутри групп: сначала повторяющиеся, потом близкий дедлайн

;; ------------------------------
;; ⚙️ Горячие клавиши
;; ------------------------------
;; C-c t t  — Создать новую задачу (task-tracker-insert-task)
;; C-c t d  — Переназначить дату задачи (task-tracker-reschedule-task)
;; C-c t a  — Открыть org-agenda (org-agenda)
;; C-c t m  — Вставить задачи на сегодня в текущий буфер (task-tracker-sync-daily-view)
;; C-c t s  — Отсортировать и пересохранить задачи (task-tracker-sort-and-format-tasks)
;;
;; Статусы:
;; C-c t 1  — Установить статус INBOX
;; C-c t 2  — Установить статус TODO
;; C-c t 3  — Установить статус DONE
;; C-c t 4  — Установить статус LIFE

(require 'org)
(require 'org-id)
(require 'org-journal)
(require 'org-transclusion)
(require 'cl-lib)  ;; для cl-remove-if, cl-remove-if-not

(setq org-transclusion-read-only nil)

;;; === ФАЙЛЫ с задачами ===

;;; Файлы:
(defvar task-tracker-dir
  "E:/YandexDisk/2 area/10 Emacs/New managment emacs/task-tracker/"
  "Базовая директория, где лежат все org-файлы трекера.")

(defvar task-tracker-tasks-file
  (concat task-tracker-dir "tasks.org")
  "Путь к основному файлу задач.")

(defvar task-tracker-archive-file
  (concat task-tracker-dir "task-archive.org")
  "Путь к файлу архивированных задач (DONE).")

(defvar task-tracker-life-file
  (concat task-tracker-dir "life-task.org")
  "Путь к файлу задач со статусом LIFE.")

;;; === TODO-ключевые слова (статусы) ===
(setq org-todo-keywords
      '((sequence "INBOX(i)" "TODO(t)" "LIFE(l)" "|" "DONE(d)")))

;;; === Приоритеты ===
(setq org-highest-priority ?A)
(setq org-lowest-priority ?D)
(setq org-default-priority ?C)

(setq org-priority-faces
      '((?A . (:foreground "firebrick" :weight bold))
        (?B . (:foreground "dark orange"))
        (?C . (:foreground "forest green"))
        (?D . (:foreground "gray50" :slant italic))))

;;; ============================================================
;;; 1. Создание задач
;;; ============================================================

(defun task-tracker-read-checklist ()
  "Запросить у пользователя чек-лист.
Возвращает строку с пунктами вида '- [ ] Текст'. Завершается пустым вводом."
  (let ((items '())
        (input ""))
    (while (not (string= (setq input (read-string "Пункт чек-листа (ENTER для завершения): ")) ""))
      (push (format "- [ ] %s" input) items))
    (if items
        (mapconcat #'identity (nreverse items) "\n")
      "")))

(defun task-tracker--read-repeat ()
  "Запросить у пользователя интервал повторения задачи.
Возвращает строку вроде '+1w' или пустую строку."
  (let* ((options '(("Нет" . "")
                    ("Каждый день" . "+1d")
                    ("Каждую неделю" . "+1w")
                    ("Каждый месяц" . "+1m")
                    ("Каждый год" . "+1y")))
         (choice (completing-read "Повторять задачу? " (mapcar #'car options) nil t)))
    (cdr (assoc choice options))))

(defun task-tracker--read-priority-from-title (title)
  "Извлечь приоритет A–D из заголовка задачи, если он указан в конце (после пробела).
Возвращает (TITLE . CHAR-PRIORITY). Если не найдено, приоритет = org-default-priority."
  (let ((parts (split-string title " ")))
    (if-let ((last (car (last parts)))
             (pri (and (= (length last) 1)
                       (member last '("A" "B" "C" "D")))))
        (cons (mapconcat #'identity (butlast parts) " ") (string-to-char pri))
      (cons title org-default-priority))))

(defun task-tracker--detect-status-suffix (taskname)
  "Проверить, стоит ли в конце имени задачи 'i' (INBOX) или 'l' (LIFE)."
  (let ((parts (split-string taskname " ")))
    (if (null parts)
        "TODO"
      (let ((last (car (last parts))))
        (cond
         ((string= last "i") "INBOX")
         ((string= last "l") "LIFE")
         (t "TODO"))))))

(defun task-tracker--read-tags-from-file ()
  "Собрать все уникальные теги из `task-tracker-tasks-file`."
  (with-current-buffer (find-file-noselect task-tracker-tasks-file)
    (let ((tags '()))
      (org-map-entries
       (lambda ()
         (setq tags (append (org-get-tags) tags)))
       nil 'file)
      (delete-dups tags))))

(defun task-tracker--select-tags ()
  "Предложить пользователю выбрать теги для задачи.
Возвращает список строк (тегов)."
  (let* ((existing (task-tracker--read-tags-from-file))
         (options (append existing '("[Новый тег]")))
         (selected (completing-read-multiple "Выбери теги (через ,): " options)))
    (if (member "[Новый тег]" selected)
        (append (remove "[Новый тег]" selected)
                (list (read-string "Введи новый тег: ")))
      selected)))

(defun task-tracker-insert-task ()
  "Создать новую задачу в `task-tracker-tasks-file`.
Запрашивает название, DEADLINE, SCHEDULED, чек-лист, теги.
Присваивает уникальный ID. Если в конце названия 'i' — статус INBOX,
'l' — статус LIFE, иначе 'TODO'."
  (interactive)
  (message "Матрица Эйзенхауэра:
A — важно и срочно    → ✦ кризисы, дедлайны
B — важно, не срочно  → ✿ обучение, планирование
C — срочно, не важно  → • чужие просьбы, звонки
D — support task      → ○ мелочи, паузы между задачами

Пример: \"Сделать отчет A i\" → приоритет A, статус INBOX.")
  (let* ((raw-title (read-string "Новая задача (в конце можно указать A/B/C/D + i/l): "))
         (split-pri (task-tracker--read-priority-from-title raw-title))
         (base-title (car split-pri))
         (priority   (cdr split-pri))
         ;; Определить статус (INBOX/LIFE/TODO)
         (detected-status (task-tracker--detect-status-suffix base-title))
         ;; Удалить суффикс из названия, если он есть
         (final-title (replace-regexp-in-string "\\( i\\| l\\)$" "" base-title))
         (status (completing-read
                  "Статус задачи: "
                  '("INBOX" "TODO" "LIFE")
                  nil t nil nil
                  detected-status))
         (use-deadline (yes-or-no-p "Установить дедлайн? "))
         (deadline (if use-deadline
                       (org-read-date nil nil nil "Дедлайн (ENTER — без даты): ")
                     ""))
         (scheduled (org-read-date nil nil nil "Дата начала (SCHEDULED, ENTER — без даты): "))
         (repeat (task-tracker--read-repeat))
         (checklist (if (yes-or-no-p "Добавить чек-лист? ")
                        (task-tracker-read-checklist)
                      ""))
         (tags-list (task-tracker--select-tags))
         (unicode-icon (alist-get priority '((?A . "✦") (?B . "✿") (?C . "•") (?D . "○"))))
         (tags-str (if tags-list
                       (concat ":" (mapconcat #'identity tags-list ":") ":")
                     ""))
         (entry (concat (format "** %s [%c] %s %s %s\n"
                                status priority unicode-icon final-title tags-str)
                        (unless (string-empty-p deadline)
                          (format "   DEADLINE: <%s>\n" deadline))
                        (unless (string-empty-p scheduled)
                          (format "   SCHEDULED: <%s%s>\n" scheduled repeat))
                        (unless (string-empty-p checklist)
                          (concat checklist "\n"))
                        "\n#+DESCRIPTION: \n"
                        "#+LINKS: \n")))
    (with-current-buffer (find-file-noselect task-tracker-tasks-file)
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Tasks" nil t)
        (goto-char (point-max))
        (insert "* Tasks\n"))
      (re-search-forward "^\\* Tasks" nil t)
      (org-end-of-subtree t t)
      (insert "\n" entry "\n")
      (backward-paragraph)
      (org-id-get-create)
      (save-buffer))
    (message "Задача «%s» добавлена с приоритетом %c, статус: %s." final-title priority status)))

;;; Горячие клавиши для статусов
(global-set-key (kbd "C-c t 1") (lambda () (interactive) (org-todo "INBOX")))
(global-set-key (kbd "C-c t 2") (lambda () (interactive) (org-todo "TODO")))
(global-set-key (kbd "C-c t 3") (lambda () (interactive) (org-todo "DONE")))
(global-set-key (kbd "C-c t 4") (lambda () (interactive) (org-todo "LIFE")))

(global-set-key (kbd "C-c t t") #'task-tracker-insert-task)

;;; ============================================================
;;; 2. Переназначение даты
;;; ============================================================

(defun task-tracker-reschedule-task ()
  "Изменить дату (SCHEDULED) текущей задачи.
Если дата пуста, SCHEDULED удаляется."
  (interactive)
  (when (org-at-heading-p)
    (let ((date (org-read-date nil nil nil "Новая дата (ENTER — удалить): ")))
      (if (string-empty-p date)
          (org-schedule '(4))
        (org-schedule nil date))
      (message "Дата изменена на %s" date))))

(global-set-key (kbd "C-c t d") #'task-tracker-reschedule-task)

;;; ============================================================
;;; 3. Просмотр всех задач
;;; ============================================================

(global-set-key (kbd "C-c t a") #'org-agenda)

;;; ============================================================
;;; 4. Вставка задач на сегодня в дневнике (daily view)
;;; ============================================================

(defun my/unlock-org-transclusion ()
  "Снять защиту с трансклюзий, чтобы их можно было редактировать."
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'org-transclusion)
      (overlay-put ov 'read-only nil))))

(defun task-tracker--is-frog-task-p (hl)
  "Определить, нужно ли пометить задачу как 'лягушку'. Если SCHEDULED просрочен более чем на 2 дня — добавить тег :frog:."
  (let* ((scheduled (org-element-property :scheduled hl))
         (todo      (org-element-property :todo-keyword hl))
         (tags      (org-element-property :tags hl)))
    (when (and scheduled (string= todo "TODO"))
      (let* ((time-str (org-element-property :raw-value scheduled))
             (time     (org-time-string-to-time time-str))
             (days-passed (floor (/ (float-time (time-subtract (current-time) time)) 86400.0))))
        (when (>= days-passed 2)
          (save-excursion
            (goto-char (org-element-property :begin hl))
            (unless (member "frog" tags)
              (org-set-tags (cons "frog" tags))
              (save-buffer)))
          t)))))

(defun task-tracker--reschedule-past-tasks-to-today ()
  "Найти все задачи с просроченным SCHEDULED или DEADLINE и перенести их на сегодня."
  (let ((today (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect task-tracker-tasks-file)
      (org-mode)
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (let* ((todo (org-get-todo-state))
                (deadline (org-entry-get (point) "DEADLINE"))
                (scheduled (org-entry-get (point) "SCHEDULED")))
           (when (and (member todo '("TODO" "INBOX"))
                      (or (and deadline (time-less-p (org-time-string-to-time deadline) (current-time)))
                          (and scheduled (time-less-p (org-time-string-to-time scheduled) (current-time)))))
             (org-schedule nil today))))))))

(add-hook 'emacs-startup-hook #'task-tracker--reschedule-past-tasks-to-today)


(defun task-tracker--collect-tasks-scheduled-for (date-str)
  "Вернуть список задач (:title :id :priority :frog :tags),
у которых DEADLINE или SCHEDULED <= DATE-STR (сравнение по дате),
и статус не DONE/LIFE.
Использует функцию `task-tracker--is-frog-task-p` для проверки просрочки."
  (with-current-buffer (find-file-noselect task-tracker-tasks-file)
    (let ((results '()))
      (org-mode)
      (goto-char (point-min))
      (let ((parsed (org-element-parse-buffer 'headline))
            (target-day (format-time-string "%Y-%m-%d"
                                            (org-time-string-to-time date-str))))
        (org-element-map parsed 'headline
          (lambda (hl)
            (let* ((todo      (org-element-property :todo-keyword hl))
                   (title     (org-element-property :raw-value hl))
                   (id        (org-element-property :ID hl))
                   (priority  (org-element-property :priority hl))
                   (tags      (org-element-property :tags hl))
                   (deadline  (org-element-property :deadline hl))
                   (scheduled (org-element-property :scheduled hl))
                   (frog      (task-tracker--is-frog-task-p hl)))
              (unless (or (null id)
                          (member todo '("DONE" "LIFE")))
                (let ((deadline-ok nil)
                      (scheduled-ok nil))
                  ;; Проверить DEADLINE <= target-day
                  (when deadline
                    (let ((dl-day (format-time-string "%Y-%m-%d"
                                                      (org-time-string-to-time
                                                       (org-element-property :raw-value deadline)))))
                      (when (string<= dl-day target-day)
                        (setq deadline-ok t))))
                  ;; Проверить SCHEDULED <= target-day
                  (when scheduled
                    (let ((sch-day (format-time-string "%Y-%m-%d"
                                                       (org-time-string-to-time
                                                        (org-element-property :raw-value scheduled)))))
                      (when (string<= sch-day target-day)
                        (setq scheduled-ok t))))
                  (when (or deadline-ok scheduled-ok)
                    (push (list :title title
                                :id id
                                :priority (or priority ?C)
                                :frog frog
                                :tags tags)
                          results)))))))
        (setq results
              (sort results
                    (lambda (a b)
                      (let ((pa (plist-get a :priority))
                            (pb (plist-get b :priority))
                            (fa (plist-get a :frog))
                            (fb (plist-get b :frog)))
                        (if (/= pa pb)
                            (< pa pb)
                          (and fa (not fb))))))))
      results)))

(defun task-tracker-sync-daily-view ()
  "Вставить в текущий буфер задачи с DEADLINE = сегодня как трансклюзии.
Не включает задачи со статусом DONE."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (tasks (task-tracker--collect-tasks-scheduled-for today))
         (main-tasks (cl-remove-if (lambda (t)
                                     (= (plist-get t :priority) ?D))
                                   tasks))
         (support-tasks (cl-remove-if-not (lambda (t)
                                            (= (plist-get t :priority) ?D))
                                          tasks)))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (when (re-search-forward "^\\* Today's Tasks" nil t)
          (org-cut-subtree))
        (goto-char (point-max))
        (insert "\n* Today's Tasks\n")
        (if (null main-tasks)
            (insert "Сегодня задач нет.\n")
          (dolist (task main-tasks)
            (let ((id (plist-get task :id)))
              (insert (format "#+transclude: [[id:%s]]\n\n" id)))))
        (when support-tasks
          (insert "\n** Support Tasks [15–30 минут]\n")
          (dolist (task support-tasks)
            (let ((id (plist-get task :id)))
              (insert (format "#+transclude: [[id:%s]]\n\n" id)))))
        (save-buffer)))
    (org-transclusion-mode 1)
    (my/unlock-org-transclusion)))

(global-set-key (kbd "C-c t m") #'task-tracker-sync-daily-view)

;;; ============================================================
;;; 5. Сортировка, архивация DONE, перенос LIFE
;;; ============================================================

;; Автосортировка при запуске
(add-hook 'emacs-startup-hook #'task-tracker-sort-and-format-tasks)

(defun task-tracker--tag-priority (tags)
  "Вернуть числовой приоритет группы тегов.
0 = :planning:, 1 = :blog:/:relationships:/:fitness:/etc, 2 = прочие."
  (cond
   ((member "planning" tags) 0)
   ((cl-intersection '(\"blog\" \"relationships\" \"fitness\" \"work\" \"awareness\") tags :test #'string=) 1)
   (t 2)))

(defun task-tracker--is-repeating (scheduled)
  "Проверить, содержит ли строка SCHEDULED повторение типа +1d, +1w и т.д."
  (and scheduled (string-match-p "\\+\\([0-9]+[dwmy]\\)" scheduled)))

(defun task-tracker--parse-tasks-buffer ()
  "Вернуть список заголовков задач с метаданными в текущем файле.
Использовать внутри `narrow-to-region` для блока * Tasks."
  (let ((parsed (org-element-parse-buffer 'headline)))
    (org-element-map parsed 'headline
      (lambda (hl)
        (let ((title (org-element-property :raw-value hl))
              (tags  (org-element-property :tags hl))
              (scheduled (org-element-property :scheduled hl))
              (deadline  (org-element-property :deadline hl))
              (priority  (org-element-property :priority hl))
              (todo      (org-element-property :todo-keyword hl))
              (begin     (org-element-property :begin hl))
              (content   (buffer-substring-no-properties
                          (org-element-property :begin hl)
                          (org-element-property :end hl))))
          (list :tags tags
                :scheduled scheduled
                :deadline deadline
                :todo todo
                :priority (or priority ?C)
                :begin begin
                :content content
                :repeating (task-tracker--is-repeating (org-element-property :raw-value scheduled))
                :sort-deadline (if deadline
                                   (org-time-string-to-time (org-element-property :raw-value deadline))
                                 nil)))))))

(defun task-tracker--sort-tasks (tasks)
  "Отсортировать список задач TACHES по тегам, повторяемости и дедлайну, игнорируя DONE и LIFE.
Но сначала мы вынесем DONE/LIFE через другие функции."
  (sort tasks
        (lambda (a b)
          (let ((ta (task-tracker--tag-priority (plist-get a :tags)))
                (tb (task-tracker--tag-priority (plist-get b :tags)))
                (ra (plist-get a :repeating))
                (rb (plist-get b :repeating))
                (da (plist-get a :sort-deadline))
                (db (plist-get b :sort-deadline)))
            (cond
             ((/= ta tb) (< ta tb))
             ((and ra (not rb)) t)
             ((and rb (not ra)) nil)
             ((and da db) (time-less-p da db))
             (t nil))))))

(defun task-tracker--move-life-tasks-to-separate-file ()
  "Переместить все задачи со статусом LIFE в `task-tracker-life-file`."
  (with-current-buffer (find-file-noselect task-tracker-tasks-file)
    (goto-char (point-min))
    (let ((life-headlines '()))
      (org-map-entries
       (lambda ()
         (let ((todo (org-get-todo-state)))
           (when (string= todo "LIFE")
             (push (org-copy-subtree) life-headlines)
             (org-cut-subtree))))
       nil 'file)
      (when life-headlines
        (with-current-buffer (find-file-noselect task-tracker-life-file)
          (goto-char (point-max))
          (insert "\n* Moved from tasks.org\n")
          (dolist (entry life-headlines)
            (insert entry "\n"))
          (save-buffer))
        (save-buffer)))))

(defun task-tracker--move-done-tasks-to-archive ()
  "Переместить все задачи со статусом DONE в `task-tracker-archive-file`."
  (with-current-buffer (find-file-noselect task-tracker-tasks-file)
    (goto-char (point-min))
    (let ((done-headlines '()))
      (org-map-entries
       (lambda ()
         (let ((todo (org-get-todo-state)))
           (when (string= todo "DONE")
             (push (org-copy-subtree) done-headlines)
             (org-cut-subtree))))
       nil 'file)
      (when done-headlines
        (with-current-buffer (find-file-noselect task-tracker-archive-file)
          (goto-char (point-max))
          (insert "\n* Moved from tasks.org\n")
          (dolist (entry done-headlines)
            (insert entry "\n"))
          (save-buffer))
        (save-buffer)))))

(defun task-tracker--set-inbox-status-for-no-deadline ()
  "Задачи без DEADLINE получают статус INBOX (если они не LIFE/DONE)."
  (org-map-entries
   (lambda ()
     (let* ((todo (org-get-todo-state))
            (deadline (org-entry-get (point) "DEADLINE"))
            (status (cond
                     ((string= todo "DONE") nil)
                     ((string= todo "LIFE") nil)
                     ((string-empty-p (or deadline "")) "INBOX"))))
       (when status
         (org-todo status))))
   nil 'file))

(defun task-tracker-sort-and-format-tasks ()
  "Пересортировать задачи в `tasks.org` по тегам и дедлайнам,
перенеся DONE => `task-tracker-archive-file` и LIFE => `task-tracker-life-file`.
Затем задачи без DEADLINE => INBOX.
И в конце пересоздать содержимое под * Tasks."
  (interactive)
  ;; 1. Переносим LIFE
  (task-tracker--move-life-tasks-to-separate-file)
  ;; 2. Переносим DONE
  (task-tracker--move-done-tasks-to-archive)
  ;; 3. Теперь сортируем оставшиеся
  (with-current-buffer (find-file-noselect task-tracker-tasks-file)
    (goto-char (point-min))
    (when (re-search-forward "^\\* Tasks" nil t)
      (let* ((start (point))
             (end (progn (org-end-of-subtree t t) (point)))
             (inhibit-read-only t))
        (save-restriction
          (narrow-to-region start end)
          (org-mode)
          ;; Присваиваем статус INBOX всем задачам без DEADлайна, кроме DONE/LIFE
          (task-tracker--set-inbox-status-for-no-deadline)
          (goto-char (point-min))
          (let ((tasks (task-tracker--parse-tasks-buffer)))
            (setq tasks (cl-remove-if (lambda (x)
                                        (member (plist-get x :todo) '("DONE" "LIFE")))
                                      tasks))
            (let ((sorted (task-tracker--sort-tasks tasks)))
              ;; Удаляем всё внутри * Tasks
              (delete-region (point-min) (point-max))
              (goto-char (point-min))
              (insert "* Tasks\n")
              (dolist (entry sorted)
                (insert (plist-get entry :content) "\n"))))))))
    (save-buffer))

(global-set-key (kbd "C-c t s") #'task-tracker-sort-and-format-tasks)

;;; === Автоматический перенос LIFE и DONE при старте ===
(add-hook 'emacs-startup-hook #'task-tracker--move-life-tasks-to-separate-file)
(add-hook 'emacs-startup-hook #'task-tracker--move-done-tasks-to-archive)

(provide 'task-tracker)

;;; task-tracker.el ends here

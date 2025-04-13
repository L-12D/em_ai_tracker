;;; journal-manager.el --- Управление дневником + сортировка, минималистичная ссылка, защита от дубли -*- lexical-binding: t; -*-

;;; Комментарий:
;; Этот модуль:
;; 1) Открывает или создаёт дневник за сегодня
;; 2) Добавляет просроченные/сегодняшние TODO-задачи, сортируя их по приоритету
;; 3) Вставляет компактную ссылку `[🔗]`, ведущую поиск по заголовку
;; 4) Создаёт директиву `#+transclude: [[id:...]]` для «живого» отображения при включённом org-transclusion-mode
;; 5) Автоматически регистрирует уже существующие ID
;; 6) **Не добавляет повторно** ту же задачу (определяем по ID), если она уже есть в дневнике

(require 'org)
(require 'org-journal)
(require 'org-transclusion nil :noerror) ;; если пакет не установлен, не упадём

;; ------------------------------
;; 1. Настройки директорий и файлов
;; ------------------------------

(defconst my/journal-dir
  (expand-file-name "../journal" (file-name-directory (or load-file-name buffer-file-name)))
  "Путь к папке, где хранятся файлы дневника (org-journal).")

(defconst my/journal-task-files
  (list
   (expand-file-name "../task-tracker/tasks.org"
                     (file-name-directory (or load-file-name buffer-file-name)))
   (expand-file-name "../task-tracker/inbox_tasks.org"
                     (file-name-directory (or load-file-name buffer-file-name))))
  "Список Org-файлов, откуда берём задачи для дневника.")

(setq org-journal-dir my/journal-dir
      org-journal-file-format "%Y%m%d.org"
      org-journal-file-header nil
      org-journal-enable-agenda-integration t)

;; ------------------------------
;; ⚙️ 2. Горячие клавиши (пример)
;; ------------------------------
;; (global-set-key (kbd "C-c j") 'my/journal-open-today-and-update)
;; (global-set-key (kbd "C-c t u") 'my/update-journal-with-tasks)
;; (global-set-key (kbd "C-c t w") 'org-transclusion-mode)

;; ------------------------------
;; 📎 Инструкция для агентов / автогенераторов конфигов
;; ------------------------------
;; Чтобы поддерживалась генерация словаря шорткатов:
;; (global-set-key (kbd "C-c j") 'my/journal-open-today-and-update) ;; открыть дневник + обновить
;; (global-set-key (kbd "C-c t u") 'my/update-journal-with-tasks)   ;; обновить дневник
;; (global-set-key (kbd "C-c t w") 'org-transclusion-mode)          ;; включить/выключить трансклюзию

;; ------------------------------
;; 3. Автоматическая регистрация ID (чтобы org-transclusion по [[id:...]] работало)
;; ------------------------------

(defun my/register-all-ids-in-file ()
  "Прочитать все заголовки текущего Org-файла и добавить их :ID: в org-id-locations.
Полезно для использования собственного формата ID (YYYYMMDD-HHMMSS)."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (let ((this-id (org-entry-get nil "ID")))
           (when (and this-id (not (string-empty-p this-id)))
             (org-id-add-location this-id (buffer-file-name)))))
       nil 'file))))

(add-hook 'org-mode-hook #'my/register-all-ids-in-file)

;; ------------------------------
;; 4. Открыть дневник (с автодобавлением + обновление)
;; ------------------------------

(defun my/journal-open-today-and-update ()
  "Открыть/создать дневник за сегодня и сразу обновить список задач."
  (interactive)
  (my/journal-open-today)
  (my/update-journal-with-tasks))

(defun my/journal-open-today ()
  "Открыть или создать файл дневника (org-journal) за сегодня.
Если файл не существует, добавим секции * Справочник и * Notes."
  (interactive)
  (unless (file-directory-p my/journal-dir)
    (make-directory my/journal-dir t))
  (let* ((fname (format-time-string org-journal-file-format))
         (full-path (expand-file-name fname my/journal-dir)))
    (find-file full-path)
    (unless (file-exists-p full-path)
      (insert (format \"#+title: %s\\n\\n\" (format-time-string \"%A, %d %B %Y\")))
      (insert \"* Справочник\\n:PROPERTIES:\\n:VISIBILITY: folded\\n:END:\\n\"
              \"- шорт-каты — C-c m\\n\\n\"
              \"* Notes\\n\")
      (save-buffer)
      (message \"Создан новый дневник за сегодня.\"))))

;; ------------------------------
;; 5. Обновление дневника (с защитой от дубли)
;; ------------------------------

(defun my/update-journal-with-tasks ()
  \"Добавить в дневник просроченные/сегодняшние TODO-задачи (со ссылкой и трансклюзией).
- Сортируем по приоритету
- Ссылка - минималистичная '[🔗]', ведущая `::*Заголовок`
- Директива '#+transclude: [[id:...]]', если org-transclusion доступен
- Игнорируем задачи, чей ID уже есть в секции 'Задачи на сегодня и просроченные', чтобы не дублировать.\"
  (interactive)
  (let* ((today (format-time-string \"<%Y-%m-%d>\"))
         (journal-file (expand-file-name (format-time-string org-journal-file-format)
                                         my/journal-dir))
         (org-files (append (directory-files my/journal-dir t \"\\\\.org$\")
                            my/journal-task-files))
         (tasks '()))
    ;; 1. Сканируем todos
    (dolist (file org-files)
      (with-current-buffer (find-file-noselect file t)
        (org-map-entries
         (lambda ()
           (unless (org-entry-get nil \"ID\")
             (org-id-get-create))
           (let ((scheduled  (org-entry-get nil \"SCHEDULED\"))
                 (deadline   (org-entry-get nil \"DEADLINE\"))
                 (todo-state (org-get-todo-state))
                 (prio       (org-get-priority (org-get-heading t t)))
                 (my-id      (org-entry-get nil \"ID\"))
                 (heading    (org-get-heading t t)))
             (when (and (equal todo-state \"TODO\")
                        (or (and scheduled
                                 (or (string= scheduled today)
                                     (string-lessp scheduled today)))
                            (and deadline
                                 (or (string= deadline today)
                                     (string-lessp deadline today)))))
               (push (list prio file heading my-id) tasks))))
         nil 'file)))
    ;; 2. Сортируем по приоритету
    (setq tasks (sort tasks (lambda (a b) (< (car a) (car b)))))

    (with-current-buffer (find-file-noselect journal-file t)
      ;; 3. Найдём секцию, запомним уже имеющиеся ID
      (goto-char (point-min))
      (let ((existing-ids '()))
        (when (re-search-forward \"^\\\\* Задачи на сегодня и просроченные\" nil t)
          (let ((sec-start (point))
                (sec-end (or (save-excursion
                                (re-search-forward \"^\\\\*\" nil t))
                              (point-max))))
            ;; Собираем ID из директив `#+transclude: [[id:XXXX]]`
            (save-excursion
              (goto-char sec-start)
              (while (re-search-forward \"^#\\\\+transclude: \\\\[\\\\\\[id:\\([^]]+\\)\\\\]\\]\" sec-end t)
                (push (match-string 1) existing-ids)))) )

        ;; 4. Если секция есть, удалим её, чтобы пересоздать
        (goto-char (point-min))
        (when (re-search-forward \"^\\\\* Задачи на сегодня и просроченные\" nil t)
          (let ((start (match-beginning 0))
                (end   (or (save-excursion (re-search-forward \"^\\\\*\" nil t))
                           (point-max))))
            (delete-region start end)))

        ;; 5. Создаём заново заголовок
        (goto-char (point-min))
        (insert \"* Задачи на сегодня и просроченные\\n\\n\")

        (dolist (task (reverse tasks))
          (let* ((my-id     (nth 3 task)))
            ;; Если этот ID уже есть, пропускаем
            (unless (member my-id existing-ids)
              (let* ((prio      (nth 0 task))
                     (file      (nth 1 task))
                     (heading   (nth 2 task))
                     (rel-file  (file-relative-name file my/journal-dir))
                     ;; Лаконичная ссылка [🔗]
                     (link (format \"[[file:%s::*%s][🔗]]\" rel-file heading))
                     ;; Трансклюзия
                     (transclusion (format \"#+transclude: [[id:%s]]\" my-id)))
                (insert (format \"** %s %s\\n\" heading link))
                (when (featurep 'org-transclusion)
                  (insert (format \"%s\\n\\n\" transclusion))))))))

      (save-buffer))
    (message \"Дневник обновлён задачами на сегодня.\")))
\n(provide 'journal-manager)\n;;; journal-manager.el ends here

;;; task-tracker-sync.el --- Минимальный модуль синхронизации задач с дневником -*- lexical-binding: t; -*-

;;; Комментарии:
;; Этот модуль предназначен только для того, чтобы:
;; 1) Собирать из tasks.org все задачи со статусами TODO или INBOX,
;;    у которых DEADLINE или SCHEDULED <= сегодня.
;; 2) Показывать их в текущем дневном файле через org-transclusion,
;;    используя команду (C-c t m).
;;
;; При необходимости сортировки, переноса DONE/LIFE, повторной
;; организации задач в tasks.org — используйте отдельный модуль.

(require 'org)
(require 'org-id)
(require 'org-transclusion)
(require 'cl-lib)

;;; Настройка: файл, где хранятся задачи
(defvar task-tracker-tasks-file
  "E:/YandexDisk/2 area/10 Emacs/New managment emacs/My config/tasks.org"
  "Путь к основному файлу задач. Из него будут выбираться задачи для дневника.")

;;; --------------------------------------------------
;;; 1. Проверка «лягушек» (необязательно, можно убрать)
;;; --------------------------------------------------

(defun task-tracker--is-frog-task-p (hl)
  "Если у задачи TODO просрочен SCHEDULED более чем на 2 дня, пометить тегом :frog:."
  (let* ((scheduled (org-element-property :scheduled hl))
         (todo      (org-element-property :todo-keyword hl))
         (tags      (org-element-property :tags hl)))
    (when (and scheduled (string= todo "TODO"))
      (let* ((time-str (org-element-property :raw-value scheduled))
             (time     (org-time-string-to-time time-str))
             (days-passed (floor (/ (float-time (time-subtract (current-time) time))
                                    86400.0))))
        (when (>= days-passed 2)
          (save-excursion
            (goto-char (org-element-property :begin hl))
            (unless (member "frog" tags)
              (org-set-tags (cons "frog" tags))
              (save-buffer)))
          t)))))

;;; --------------------------------------------------
;;; 2. Сбор задач для «сегодня» (или более ранних)
;;; --------------------------------------------------

(defun task-tracker--collect-tasks-scheduled-for (date-str)
  "Вернуть список задач (:title :id :priority :frog :tags),
у которых DEADLINE или SCHEDULED <= DATE-STR (YYYY-MM-DD),
и статус не DONE/LIFE.
Использует `task-tracker--is-frog-task-p` для отметки просроченных задач."
  (with-current-buffer (find-file-noselect task-tracker-tasks-file)
    (let ((results '()))
      (org-mode)
      (goto-char (point-min))
      (let ((parsed (org-element-parse-buffer 'headline))
            ;; Превращаем указанную дату (обычно сегодня) в строку "YYYY-MM-DD"
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
                  (when deadline
                    (let ((dl-day (format-time-string
                                   "%Y-%m-%d"
                                   (org-time-string-to-time
                                    (org-element-property :raw-value deadline))))))
                      (when (string<= dl-day target-day)
                        (setq deadline-ok t))))
                  (when scheduled
                    (let ((sch-day (format-time-string
                                    "%Y-%m-%d"
                                    (org-time-string-to-time
                                     (org-element-property :raw-value scheduled))))))
                      (when (string<= sch-day target-day)
                        (setq scheduled-ok t))))
                  (when (or deadline-ok scheduled-ok)
                    (push (list :title title
                                :id id
                                :priority (or priority ?C)
                                :frog frog
                                :tags tags)
                          results)))))))
        ;; Возвращаем tasks, можно при желании добавить сортировку
        results))
;;; --------------------------------------------------
;;; 3. Вставка задач на сегодня в текущий буфер
;;; --------------------------------------------------

(defun my/unlock-org-transclusion ()
  "Снять защиту с трансклюзий, чтобы их можно было редактировать."
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'org-transclusion)
      (overlay-put ov 'read-only nil))))

(defun task-tracker-sync-daily-view ()
  "Вставить в текущий буфер все задачи (TODO/INBOX), которые имеют
DEADLINE или SCHEDULED <= сегодня, используя org-transclusion.

1) Собирает список задач из `task-tracker-tasks-file`,
2) Создаёт заголовок * Today's Tasks,
3) Под каждую задачу вставляет #+transclude: [[id:...]],
4) Активирует org-transclusion-mode и делает их редактируемыми."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (tasks (task-tracker--collect-tasks-scheduled-for today)))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (when (re-search-forward "^\\* Today's Tasks" nil t)
          (org-cut-subtree))
        (goto-char (point-max))
        (insert "\n* Today's Tasks\n")
        (if (null tasks)
            (insert "Сегодня задач нет.\n")
          (dolist (task tasks)
            (let ((id (plist-get task :id)))
              (insert (format "#+transclude: [[id:%s]]\n\n" id)))))
        (save-buffer)))
    (org-transclusion-mode 1)
    (my/unlock-org-transclusion))

;;; Горячая клавиша для просмотра задач в дневнике
(global-set-key (kbd "C-c t m") #'task-tracker-sync-daily-view)

(provide 'task-tracker-sync)

;;; task-tracker-sync.el ends here

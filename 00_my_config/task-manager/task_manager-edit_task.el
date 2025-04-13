;;; task_manager-edit_task.el --- Расширенное редактирование задач -*- lexical-binding: t; -*-

;;; Комментарий:
;; Данный модуль предоставляет интерфейс для интерактивного редактирования задач в Org-mode.
;; Пользователь может выбрать, какие свойства задачи отредактировать: DEADLINE, SCHEDULED,
;; повторяемость (через свойство REPEAT), теги или добавить чек-лист.
;;
;; 🔮 В будущем планируется:
;; - Возможность менять заголовок и приоритет
;; - Редактирование через transient/hydra меню
;; - Поддержка многократного редактирования без выхода из функции

;; ------------------------------
;; ⚙️ Панель горячих клавиш
;; ------------------------------
;; (global-set-key (kbd "C-c e") 'my/edit-task) ;; редактирование задачи (DEADLINE, теги, чеклист и др.)

;; ------------------------------
;; 📎 Инструкция для агентов / автогенераторов конфигов
;; ------------------------------
;; Чтобы ваша конфигурация поддерживалась моим генератором словаря шорткатов:
;;
;; 1. Горячие клавиши должны быть записаны в одной строке:
;;    (global-set-key (kbd "C-c e") 'my/edit-task) ;; редактирование задачи
;;
;; 2. Или через define-key:
;;    (define-key some-map (kbd "C-c e") 'my/edit-task) ;; редактирование задачи
;;
;; 3. Регулярное выражение для поиска таких строк:
;;    ^\s-*\((\(global-set-key\|define-key\).*\)$
;;
;; 4. Комментарий (если он есть) желательно добавлять в той же строке:
;;    ;; короткое описание назначения сочетания клавиш

;; ------------------------------
;; 1. Функция редактирования задачи
;; ------------------------------

(defun my/edit-task ()
  "Редактировать задачу: DEADLINE, SCHEDULED, повторяемость, теги, чек-лист.
Позволяет выбрать одно из свойств и изменить его через диалог."
  (interactive)
  (org-back-to-heading t)
  (let ((choice (read-char-choice
                 "Выберите действие: [d] DEADLINE, [s] SCHEDULED, [r] Повторяемость, [t] Теги, [c] Чек-лист: "
                 '(?d ?s ?r ?t ?c))))
    (cond
     ;; DEADLINE
     ((eq choice ?d)
      (let ((set-deadline (y-or-n-p "Установить DEADLINE? ")))
        (when set-deadline
          (let ((deadline-time (org-read-date nil t nil "Выберите DEADLINE: ")))
            (org-deadline 'set deadline-time)))))

     ;; SCHEDULED
     ((eq choice ?s)
      (let ((set-scheduled (y-or-n-p "Установить SCHEDULED? ")))
        (when set-scheduled
          (let ((scheduled-time (org-read-date nil t nil "Выберите SCHEDULED: ")))
            (org-schedule 'set scheduled-time)))))

     ;; Повторяемость (через свойство REPEAT)
     ((eq choice ?r)
      (let ((repeat (read-string "Введите повторяемость (например, ++1w): ")))
        (when (and repeat (not (string-empty-p repeat)))
          (org-set-property "REPEAT" repeat))))

     ;; Теги
     ((eq choice ?t)
      (let ((new-tags (read-string "Введите теги через запятую: ")))
        (when new-tags
          (org-set-tags (split-string new-tags ",[[:space:]]*")))))

     ;; Чек-лист
     ((eq choice ?c)
      (let ((item "")
            (checklist-lines '()))
        (while (not (string-empty-p
                     (setq item (read-string "Введите пункт чек-листа (Enter — завершить): "))))
          (push (format "- [ ] %s" item) checklist-lines))
        (when checklist-lines
          (save-excursion
            (org-end-of-subtree)
            (insert "\n** Чек-лист\n")
            (insert (string-join (reverse checklist-lines) "\n") "\n")))))))
  ) ;; ← Закрывает let и defun


;; Привязка к удобной комбинации клавиш
(global-set-key (kbd "C-c e") 'my/edit-task) ;; редактировать текущую задачу

(provide 'task_manager-edit_task)
;;; task_manager-edit_task.el ends here

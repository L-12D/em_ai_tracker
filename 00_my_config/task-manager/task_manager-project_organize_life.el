;;; task_manager-project_organize_life.el --- Организация задач по проектам в life-tasks.org -*- lexical-binding: t; -*-

;;; Комментарий:
;; Модуль предназначен для перемещения задач в файл `life-tasks.org`,
;; где они группируются по проектам, связанным с личной жизнью, хобби и целями вне рабочего контекста.
;;
;; Поддерживаются три сценария:
;; - Перемещение в существующий проект
;; - Создание нового проекта при переносе
;; - Создание проекта с тем же названием, что у задачи ("Без проекта")
;;
;; Задача получает статус LIFE и сохраняется в структуре выбранного проекта.

;; ------------------------------
;; 🔧 Конфигурируемые переменные
;; ------------------------------

(defconst my/life-tasks-file
  (expand-file-name "../../task-tracker/life-tasks.org" (file-name-directory (or load-file-name buffer-file-name)))
  "Относительный путь к файлу life-tasks.org для задач, связанных с личной жизнью.")

;; ------------------------------
;; ⚙️ Панель горячих клавиш
;; ------------------------------
;; (global-set-key (kbd "C-c t l") 'my/move-to-life-tasks) ;; переместить задачу в life-tasks.org

;; ------------------------------
;; 📎 Инструкция для агентов / автогенераторов конфигов
;; ------------------------------
;; Чтобы ваша конфигурация поддерживалась моим генератором словаря шорткатов:
;;
;; 1. Горячие клавиши должны быть записаны в одной строке:
;;    (global-set-key (kbd "C-c t l") 'my/move-to-life-tasks) ;; переместить задачу в life-tasks.org
;;
;; 2. Или через define-key:
;;    (define-key some-map (kbd "C-c t l") 'my/move-to-life-tasks) ;; переместить задачу в life-tasks.org
;;
;; 3. Регулярное выражение для поиска таких строк:
;;    ^\\s-*\\((\\(global-set-key\\|define-key\\).*\\)$
;;
;; 4. Комментарий (если он есть) желательно добавлять в той же строке:
;;    ;; короткое описание назначения сочетания клавиш

;; ------------------------------
;; 1. Получение списка проектов
;; ------------------------------

(defun my/get-life-projects ()
  "Получить список всех проектов из файла `life-tasks.org`.
Ищет заголовки вида `* Проект: ...` и возвращает названия проектов."
  (with-current-buffer (find-file-noselect my/life-tasks-file)
    (goto-char (point-min))
    (let (projects)
      (while (re-search-forward "^\\* Проект: \\(.+\\)$" nil t)
        (push (match-string 1) projects))
      (nreverse projects))))

;; ------------------------------
;; 2. Перемещение задачи в life-tasks.org
;; ------------------------------

(defun my/move-to-life-tasks ()
  "Переместить текущую задачу в файл `life-tasks.org` в статусе LIFE с выбором проекта.
Задача будет вставлена под нужным проектом. Возможен выбор:
- существующего проекта;
- создание нового проекта;
- или 'Без проекта' — тогда проект создается на основе названия задачи."
  (interactive)
  (org-back-to-heading t)
  (let ((task-start (point))
        (task-end   (save-excursion (org-end-of-subtree) (point)))
        (source-buffer (current-buffer))
        (life-tasks-buffer (find-file-noselect my/life-tasks-file)))
    (let* ((projects (append (my/get-life-projects) '("Без проекта" "Новый проект")))
           (selected-project (completing-read "Выберите проект для life-tasks: " projects nil nil)))
      (kill-region task-start task-end)
      (with-current-buffer life-tasks-buffer
        (goto-char (point-min))
        (cond
         ;; Новый проект
         ((string= selected-project "Новый проект")
          (let ((new-project-name (read-string "Введите название нового проекта: ")))
            (goto-char (point-max))
            (insert (format "\n* Проект: %s\n" new-project-name))
            (insert (replace-regexp-in-string "^\\* \\w+" "* LIFE" (current-kill 0)))))

         ;; Без проекта — создаём проект с названием задачи
         ((string= selected-project "Без проекта")
          (goto-char (point-max))
          (let ((task-title (replace-regexp-in-string
                             "^\\* \\w+ \\(\\[#[A-C]\\] \\)?\\(.+?\\)\\( :.+:\\)?$"
                             "\\2" (current-kill 0))))
            (insert (format "\n* Проект: %s\n" task-title))
            (insert (replace-regexp-in-string "^\\* \\w+" "* LIFE" (current-kill 0)))))

         ;; Существующий проект
         (t
          (if (re-search-forward (format "^\\* Проект: %s$" (regexp-quote selected-project)) nil t)
              (progn
                (forward-line 1)
                (insert (replace-regexp-in-string "^\\* \\w+" "* LIFE" (current-kill 0))))
            ;; Если вдруг проект не найден (хотя выбран), создаём его в конце
            (goto-char (point-max))
            (insert (format "\n* Проект: %s\n" selected-project))
            (insert (replace-regexp-in-string "^\\* \\w+" "* LIFE" (current-kill 0))))))
        (save-buffer))
      (with-current-buffer source-buffer
        (save-buffer))
      (message "✅ Задача перемещена в life-tasks.org в статусе LIFE"))))

;; ------------------------------
;; 3. Горячая клавиша
;; ------------------------------
(global-set-key (kbd "C-c t l") 'my/move-to-life-tasks) ;; переместить в life-tasks.org

(provide 'task_manager-project_organize_life)
;;; task_manager-project_organize_life.el ends here

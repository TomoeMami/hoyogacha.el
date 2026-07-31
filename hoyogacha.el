;;; 历史记录变量
(defvar hoyogacha-path-history-saved nil
  "历史记录：第一个路径（必填）")
(defvar hoyogacha-path-history-import nil
  "历史记录：第二个路径（可选，用于合并）")

;;; 读取指定目录下所有符合条件的 JSON 数据
(defun hoyogacha-read-json-files (dir-path)
  "读取 DIR-PATH 目录下所有 .json 文件，返回符合版本要求的数据列表。
版本要求：info.version >= v4.1"
  (let ((json-files (directory-files (expand-file-name dir-path) t "\\.json$"))
        (records nil)
        (json-array-type 'list))
    (dolist (file json-files)
      (when (file-regular-p file)
        (condition-case err
            (let* ((json-data (json-read-file file))
                   (info (cdr (assq 'info json-data)))
                   (version (cdr (assq 'version info)))
                   (nap (cdr (assq 'nap json-data))))
              ;; 版本检查
              (when (and version (stringp version)
                         (string-match "^v?\\([0-9]+\\)\\.\\([0-9]+\\)" version))
                (let ((major (string-to-number (match-string 1 version)))
                      (minor (string-to-number (match-string 2 version))))
                  (when (or (> major 4) (and (= major 4) (>= minor 1)))
                    ;; 遍历 nap 数组，提取每个 list 中的记录
                    (dolist (nap-item nap)
                      (let ((list (cdr (assq 'list nap-item))))
                        (when (listp list)
                          (setq records (append records list)))))))))
          (error (message "跳过文件 %s: %s" file (error-message-string err))))))
    records))

;;; 合并两个目录的数据（若第二个目录为空，则只使用第一个）
(defun hoyogacha-merge-data (dir1 &optional dir2)
  "从 DIR1 和 DIR2 读取数据并合并（去重暂不处理）。"
  (let* ((records1 (and (not (string-empty-p dir1))
                    (hoyogacha-read-json-files dir1)))
         (records2 (and dir2 (not (string-empty-p dir2))
                    (hoyogacha-read-json-files dir2)))
         (all-records (append records1 records2)))
    (cl-remove-duplicates all-records :key (lambda (r) (cdr (assq 'id r))) :test 'equal)))

;;; 生成统计 buffer
(defun hoyogacha-stats-buffer (records)
  "根据 DATA 生成只读统计 buffer，并显示目录信息。"
  (let ((buf (get-buffer-create "*抽卡统计*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "📊 抽卡数据分析报告\n\n"))
      (insert (format "总抽卡记录数: %d\n\n" (length records)))

      ;; 示例：按 rank_type 分组统计
      (let ((rank-groups (cl-loop for r in records
                                  for rank = (cdr (assq 'rank_type r))
                                  when rank
                                  collect rank)))
        (insert "按星级 (rank_type) 统计：\n")
        (dolist (rank '("5" "4" "3"))  ; 常见星级
          (let ((count (cl-count rank rank-groups :test 'equal)))
            (when (> count 0)
              (insert (format "  ★%s 星: %d 个\n" rank count))))))

      (insert "\n（更多详细统计可按需扩展）\n")
      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer buf)))



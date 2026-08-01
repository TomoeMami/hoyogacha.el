;;; hoyogacha.el --- Emacs 中管理米哈游抽卡记录的插件 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 TomoeMami

;; Author: TomoeMami <trembleafterme@outlook.com>
;; Created: 2026.08

;; URL: https://github.com/TomoeMami/hoyogacha.el

;; Package-Requires: ((emacs "25.1") (plz "0.9"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 在Emacs中管理米哈游抽卡记录的插件。

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'plz)   ; 使用 plz.el 替代内置 url
(require 'map)

;; ------------------------------------------------------------
;; 获取抽卡记录链接
;; ------------------------------------------------------------

(defvar hoyogacha-last-warp-url nil
  "最近定位到的抽卡记录链接。")

(defcustom hoyogacha-default-game nil
  "默认游戏：'hsr 或 'zzz。nil 时自动检测。"
  :type '(choice (const :tag "自动检测" nil)
                 (const :tag "崩坏：星穹铁道" hsr)
                 (const :tag "绝区零" zzz))
  :group 'hoyogacha)

(defvar hoyogacha-games
  '((hsr
     :locallow "崩坏：星穹铁道"
     :log-prefix "Loading player data from "
     :log-suffix "data.unity3d")
    (zzz
     :locallow "绝区零"
     :log-prefix "[Subsystems] Discovering subsystems at path "
     :log-suffix "UnitySubsystems"))
  "支持的游戏配置。")

(defun hoyogacha--locallow-dir (game)
  "返回 GAME 在 LocalLow/miHoYo 下的完整路径。"
  (let ((appdata (getenv "APPDATA")))
    (if appdata
        (expand-file-name
         (concat "LocalLow/miHoYo/" (map-elt (cdr (map-elt hoyogacha-games game)) :locallow))
         (expand-file-name ".." appdata))
      (user-error "未找到 APPDATA 环境变量"))))

(defun hoyogacha--detect-game ()
  "自动检测已安装的游戏，返回 'hsr 或 'zzz。"
  (let ((hsr-dir (ignore-errors (hoyogacha--locallow-dir 'hsr)))
        (zzz-dir (ignore-errors (hoyogacha--locallow-dir 'zzz))))
    (cond
     ((and hsr-dir (file-directory-p hsr-dir)
           zzz-dir (file-directory-p zzz-dir))
      ;; 两者都装，默认 hsr
      'hsr)
     ((and hsr-dir (file-directory-p hsr-dir)) 'hsr)
     ((and zzz-dir (file-directory-p zzz-dir)) 'zzz)
     (t (user-error "未检测到已安装的星穹铁道或绝区零")))))

(defun hoyogacha--read-log-lines (log-file)
  "读取 LOG-FILE 并返回行列表。"
  (with-temp-buffer
    (insert-file-contents log-file)
    (let ((content (buffer-string)))
      (when (string-prefix-p "\ufeff" content)
        (setq content (substring content 1)))
      (split-string content "\r?\n" t))))

(defun hoyogacha--game-dir-from-log-file (log-file game)
  "从 LOG-FILE 中按 GAME 的配置提取游戏目录。"
  (let* ((config (cdr (map-elt hoyogacha-games game)))
         (prefix (map-elt config :log-prefix))
         (suffix (map-elt config :log-suffix))
         (line (cl-loop for line in (hoyogacha--read-log-lines log-file)
                        repeat 10
                        when (string-prefix-p prefix line)
                        return line)))
    (when line
      (let* ((start (length prefix))
             (end (string-match (regexp-quote suffix) line start)))
        (when end
          (let ((dir (substring line start end)))
            (unless (string-empty-p (string-trim dir))
              (file-name-as-directory (string-trim dir)))))))))

(defun hoyogacha-find-game-dir (&optional game)
  "确定游戏目录；GAME 为 'hsr 或 'zzz，nil 时自动检测。"
  (let ((game (or game hoyogacha-default-game (hoyogacha--detect-game))))
    (let ((locallow (hoyogacha--locallow-dir game))
          (log-file (expand-file-name "Player.log" (hoyogacha--locallow-dir game))))
      (or (when (file-exists-p log-file)
            (hoyogacha--game-dir-from-log-file log-file game))
          (let ((prev-log-file (expand-file-name "Player-prev.log" locallow)))
            (when (file-exists-p prev-log-file)
              (hoyogacha--game-dir-from-log-file prev-log-file game)))))))

(defun hoyogacha--version-number (version)
  "将 6.4.0.0 转为 6400 以比较大小。"
  (string-to-number
   (mapconcat #'identity (split-string version "\\.") "")))

(defun hoyogacha--latest-cache-file (game-dir)
  "在 GAME-DIR/webCaches 下寻找最新版本的 data_2 文件。"
  (let* ((web-caches (expand-file-name "webCaches" game-dir))
         (base-file (expand-file-name "Cache/Cache_Data/data_2" web-caches))
         ;; 优先匹配版本号目录（如 6.4.0.0）
         (version-dirs
          (when (file-directory-p web-caches)
            (cl-loop for dir in (directory-files web-caches t)
                     for name = (file-name-nondirectory (directory-file-name dir))
                     when (and (file-directory-p dir)
                               (string-match-p
                                "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" name))
                     collect (cons (hoyogacha--version-number name) dir))))
         (version-files
          (mapcar (lambda (entry)
                    (expand-file-name "Cache/Cache_Data/data_2" (cdr entry)))
                  (sort version-dirs
                        (lambda (a b) (> (car a) (car b))))))
         (candidate-files
          (append version-files (list base-file))))
    (or (cl-loop for file in candidate-files
                 when (file-exists-p file)
                 return file)
        ;; 若没有版本号目录，则按修改时间选最新
        (when (file-directory-p web-caches)
          (let* ((all-dirs
                  (cl-loop for dir in (directory-files web-caches t)
                           when (file-directory-p dir)
                           collect dir))
                 (latest-dir
                  (car (cl-sort all-dirs
                                (lambda (a b)
                                  (time-less-p
                                   (file-attribute-modification-time (file-attributes b))
                                   (file-attribute-modification-time (file-attributes a)))))))
            (let ((file (expand-file-name "Cache/Cache_Data/data_2" latest-dir)))
              (when (file-exists-p file)
                file))))))))

(defun hoyogacha--read-cache-file (cache-file)
  "复制 CACHE-FILE 到临时文件后读取，避免文件被占用。"
  (let ((temp-file (make-temp-file "hoyogacha-cache-" nil ".dat")))
    (unwind-protect
        (progn
          (copy-file cache-file temp-file t)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally temp-file)
            (string-to-multibyte (buffer-string))))
      (ignore-errors (delete-file temp-file)))))

(defun hoyogacha--before-null (string)
  "返回 STRING 中第一个空字符之前的内容。"
  (let ((pos (cl-position 0 string)))
    (if pos (substring string 0 pos) string)))

(defun hoyogacha--extract-warp-url (cache-data)
  "从缓存内容中提取抽卡记录 URL。"
  (let ((segments (split-string cache-data "1/0/" t)))
    (cl-loop for segment in (reverse segments)
             for maybe-url = (hoyogacha--before-null segment)
             when (and maybe-url
                       (string-prefix-p "http" maybe-url)
                       (or (string-match-p "getGachaLog" maybe-url)
                           (string-match-p "getLdGachaLog" maybe-url)))
             return maybe-url)))

(defun hoyogacha--clean-warp-url (url)
  "只保留抽卡链接的必要查询参数。"
  (let* ((qpos (string-match-p "\\?" url))
         (base (if qpos (substring url 0 qpos) url))
         (query (if qpos (substring url (1+ qpos)) ""))
         (kept
          (cl-loop for pair in (split-string query "&" t)
                   for key = (and (string-match "\\`[^=]+" pair)
                                  (match-string 0 pair))
                   when (member key '("authkey" "authkey_ver"
                                      "sign_type" "game_biz"
                                      "lang"))
                   collect pair)))
    (if kept
        (concat base "?" (mapconcat #'identity kept "&"))
      base)))

(defun hoyogacha--warp-url-valid-p (url game)
  "请求 URL，检查 retcode 是否为 0。
使用 plz.el 同步请求，返回 t 表示有效，否则 nil。
GAME 为当前游戏符号，用于错误消息。"
  (condition-case err
      (let* ((data (plz 'get url
                        :headers '(("User-Agent" . "Mozilla/5.0")
                                   ("Accept" . "application/json"))
                        :as #'json-read
                        :timeout 10)))
        (equal (map-elt data 'retcode) 0))
    (plz-error
     (message "[%s] 抽卡链接验证失败：%s"
              (symbol-name game) (error-message-string err))
     nil)))

(defun hoyogacha-get-warp-url (&optional game-or-path)
  "定位并返回抽卡记录链接。

GAME-OR-PATH 可以是：
- 目录路径（字符串） —— 直接作为游戏安装目录；
- 符号 'hsr 或 'zzz —— 指定游戏；
- 字符串 \"hsr\" 或 \"zzz\" —— 指定游戏；
- nil —— 自动检测。

返回值为清理后的链接，同时存入 `hoyogacha-last-warp-url' 并复制到 kill-ring。"
  (interactive)
  (let* ((game nil)
         (game-dir nil)
         ;; 用于在错误消息中显示游戏名
         (game-label (lambda ()
                       (if (symbolp game)
                           (symbol-name game)
                         "unknown"))))
    (cond
     ;; 符号或已知游戏名
     ((and game-or-path (or (symbolp game-or-path)
                            (and (stringp game-or-path)
                                 (member (downcase game-or-path) '("hsr" "zzz" "星穹铁道" "绝区零")))))
      (setq game (if (symbolp game-or-path)
                     game-or-path
                   (intern (replace-regexp-in-string
                            "星穹铁道" "hsr"
                            (replace-regexp-in-string "绝区零" "zzz"
                                                      (downcase game-or-path))))))
      (unless (map-elt hoyogacha-games game)
        (user-error "[%s] 未知游戏：%s" (funcall game-label) game))
      (setq game-dir (hoyogacha-find-game-dir game)))

     ;; 目录路径
     ((and (stringp game-or-path) (file-directory-p game-or-path))
      (setq game-dir (file-name-as-directory game-or-path))
      ;; 自动检测游戏（仅用于日志定位时的配置，实际缓存读取不需要）
      (setq game (hoyogacha--detect-game)))

     ;; nil：自动检测
     ((null game-or-path)
      (setq game (or hoyogacha-default-game (hoyogacha--detect-game)))
      (setq game-dir (hoyogacha-find-game-dir game)))

     (t (user-error "[%s] 无法识别的参数：%S" (funcall game-label) game-or-path)))

    (unless game-dir
      (user-error "[%s] 无法定位游戏目录，请手动传入路径或检查游戏是否安装"
                  (funcall game-label)))

    ;; 定位缓存文件
    (let ((cache-file (hoyogacha--latest-cache-file game-dir)))
      (unless cache-file
        (user-error "[%s] 未找到 webCaches 下的 data_2 缓存文件"
                    (funcall game-label)))

      (let* ((cache-data (hoyogacha--read-cache-file cache-file))
             (raw-url (hoyogacha--extract-warp-url cache-data)))
        (unless raw-url
          (user-error "[%s] 未在缓存中找到抽卡链接；请先在游戏中打开抽卡记录页面"
                      (funcall game-label)))

        (if (hoyogacha--warp-url-valid-p raw-url game)
            (let ((url (hoyogacha--clean-warp-url raw-url)))
              (setq hoyogacha-last-warp-url url)
              (kill-new url)
              url)
          (user-error "[%s] 找到的抽卡链接已失效，请重新在游戏中打开抽卡记录"
                      (funcall game-label)))))))

;; ------------------------------------------------------------
;; 根据抽卡记录链接拉取记录
;; ------------------------------------------------------------


(defcustom hoyogacha-hsr-gacha-types '(1 2 11 12 21 22)
  "HSR 的 gacha_type 列表。"
  :type '(repeat integer)
  :group 'hoyogacha)

(defun hoyogacha--build-url (url &rest params)
  "在 URL 上附加查询参数 PARAMS，返回新 URL。
PARAMS 是键值交替的列表，如 (\"gacha_type\" \"11\" \"page\" \"1\")。"
  (let ((query-string (mapconcat
                        (lambda (pair)
                          (format "%s=%s" (car pair) (cdr pair)))
                        (cl-loop for (key val) on params by #'cddr
                                 collect (cons key val))
                        "&")))
    (concat url "&" query-string)))

(defun hoyogacha--request-gacha-page (url &optional gacha-type page size)
  "请求抽卡日志的一页，返回解析后的 JSON alist。
出错时信号带 [HSR] 前缀的错误。"
  (let* ((params (when gacha-type (list "gacha_type" (number-to-string gacha-type))))
         (params (if page (append params (list "page" (number-to-string page))) params))
         (params (if size (append params (list "size" (number-to-string size))) params))
         (full-url (apply #'hoyogacha--build-url url params))
         (response (condition-case err
                       (plz 'get full-url
                            :headers '(("User-Agent" . "Mozilla/5.0"))
                            :as #'json-read
                            :timeout 15)
                     (plz-error
                      (error "[HSR] 抽卡日志请求失败：%s"
                             (error-message-string err))))))
    (unless (equal (map-elt response 'retcode) 0)
      (error "[HSR] 抽卡日志返回错误：%s"
             (map-elt response 'message )))
    response))

(defun hoyogacha-fetch-gacha-records-from-url (url &optional gacha-types)
  "从 URL 拉取全部 HSR 抽卡记录，返回去重后的记录列表。
GACHA-TYPES 可覆盖默认的 =hoyogacha-hsr-gacha-types'。"
  (interactive "s抽卡日志 URL: ")
  (let ((gacha-types (or gacha-types hoyogacha-hsr-gacha-types))
        (records-list '())
        (seen (make-hash-table :test #'equal)))
    (dolist (type gacha-types)
      (message "[HSR] 拉取 gacha_type=%s ..." type)
      (let ((page 1)
            (keep-p t))
        (while keep-p
          (let* ((response (hoyogacha--request-gacha-page url type page 20))
                 (data (map-elt response 'data))
                 (records (map-elt data 'list))
                 (count (length records)))
            (unless records
              (error "[HSR] 响应中缺少 data.list 字段"))
            (message "[HSR] gacha_type=%s page=%s 获取 %d 条"
                     type page count)
            (dolist (record records)
              (let ((id (map-elt record 'id)))
                (unless (map-elt seen id)
                  (map-put! seen id t)
                  (push record records-list))))
            (if (< count 20)
                (setq keep-p nil)
              (setq page (1+ page)))))))
    (nreverse records-list)))


;;; 历史记录变量
(defvar hoyogacha-path-data-saved nil
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
                   (version (cdr (map-nested-elt json-data '(info version))))
                   (nap (cdr (map-elt json-data 'nap))))
              ;; 版本检查
              (when (and version (stringp version)
                         (string-match "^v?\\([0-9]+\\)\\.\\([0-9]+\\)" version))
                (let ((major (string-to-number (match-string 1 version)))
                      (minor (string-to-number (match-string 2 version))))
                  (when (or (> major 4) (and (= major 4) (>= minor 1)))
                    ;; 遍历 nap 数组，提取每个 list 中的记录
                    (dolist (nap-item nap)
                      (let ((list (cdr (map-elt nap-item 'list))))
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
    (cl-remove-duplicates all-records :key (lambda (r) (cdr (map-elt r 'id))) :test 'equal)))

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
                                  for rank = (cdr (map-elt r 'rank_type))
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



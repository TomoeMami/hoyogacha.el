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
;; 相关代码的映射
;; ------------------------------------------------------------

(defvar hoyogacha-games
  '((hsr
     :locallow "崩坏：星穹铁道"
     :log-prefix "Loading player data from "
     :log-suffix "data.unity3d"
     :data-key hkrpg
     :gacha-types (1 2 11 12 21 22)
     :gacha-type-names (("1"  . "常驻跃迁")
                        ("2"  . "新手跃迁")
                        ("11" . "角色活动跃迁")
                        ("12" . "光锥活动跃迁")
                        ("21" . "角色联动跃迁")
                        ("22" . "光锥联动跃迁"))
     :rank-type-names (("3" . "三星")
                       ("4" . "四星")
                       ("5" . "五星")))
    (zzz
     :locallow "绝区零"
     :log-prefix "[Subsystems] Discovering subsystems at path "
     :log-suffix "UnitySubsystems"
     :data-key nap
     :gacha-types (1 2 3 5 102 103)
     :gacha-type-names (("1"   . "常驻频段")
                        ("2"   . "独家频段")
                        ("3"   . "音擎频段")
                        ("5"   . "邦布频段")
                        ("102" . "独家重映")
                        ("103" . "音擎回响"))
     :rank-type-names (("2" . "B")
                       ("3" . "A")
                       ("4" . "S"))))
  "支持的游戏配置。
每个条目包含：
- :data-key      导出 JSON 中对应的顶层 key（符号）
- :display-name  显示名称
- :gacha-types   该游戏可能的 gacha_type 值（数字列表，用于拉取记录）
- :gacha-type-names gacha_type 代码到显示名称的 alist（key 为字符串）
- :rank-type-names  rank_type 代码到显示名称的 alist（key 为字符串）")

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

;; ------------------------------------------------------------
;; 导入 UIGF json 文件
;; ------------------------------------------------------------

(defcustom hoyogacha-data-save-file nil
  "历史记录保存文件路径 (.json)。用于跨 Emacs 会话保存合并后的抽卡数据。"
  :type '(choice (const :tag "未设置" nil) file)
  :group 'hoyogacha)

(defcustom hoyogacha-data-import-dir nil
  "可选：要自动扫描并合并的 UIGF JSON 文件目录。"
  :type '(choice (const :tag "未设置" nil) directory)
  :group 'hoyogacha)

(defvar hoyogacha-merged-data nil
  "合并后的 UIGF 数据。关闭统计 buffer 时自动保存到 `hoyogacha-data-save-file'。")

(defconst hoyogacha--uigf-game-keys '(hkrpg nap)
  "UIGF 顶层游戏数据 key 列表。")

(defun hoyogacha--uigf-version-ok-p (version)
  "Return non-nil if VERSION (string) is UIGF v4.1 or later."
  (and version (stringp version)
       (string-match "\\`v?\\([0-9]+\\)\\.\\([0-9]+\\)" version)
       (let ((major (string-to-number (match-string 1 version)))
             (minor (string-to-number (match-string 2 version))))
         (or (> major 4) (and (= major 4) (>= minor 1))))))

(defun hoyogacha--read-uigf-file (file)
  "Read FILE as UIGF JSON and return its alist if valid, else nil.
Only files with info.version >= v4.1 are accepted."
  (condition-case err
      (let ((data (json-read-file file)))
        (when (and (map-elt data 'info)
                   (hoyogacha--uigf-version-ok-p
                    (map-nested-elt data '(info version))))
          data))
    (error (message "跳过文件 %s: %s" file (error-message-string err))
           nil)))

(defun hoyogacha-read-json-files (dir-path)
  "读取 DIR-PATH 下所有符合 UIGF 的 .json 文件，返回 UIGF alist 列表。
版本要求：info.version >= v4.1。"
  (let ((files (directory-files (expand-file-name dir-path) t "\\.json\\'" t)))
    (delq nil
          (mapcar (lambda (file)
                    (when (file-regular-p file)
                      (hoyogacha--read-uigf-file file)))
                  files))))

(defun hoyogacha--blank-uigf-data ()
  "返回一个空白 UIGF 数据的 alist。"
  (list (cons 'info
              (list (cons 'version "v4.2")
                    (cons 'export_app "hoyogacha.el")
                    (cons 'export_app_version "0.1")
                    (cons 'export_timestamp
                          (string-to-number (format-time-string "%s")))))
        (cons 'hkrpg [])
        (cons 'nap [])))

(defun hoyogacha--ensure-save-file (&optional file)
  "确保 FILE 存在；若不存在则创建空白 UIGF 文件。
FILE 为 nil 时使用 `hoyogacha-data-save-file'。"
  (let ((file (or file hoyogacha-data-save-file)))
    (unless file
      (user-error "未设置 hoyogacha-data-save-file"))
    (unless (file-exists-p file)
      (let ((dir (file-name-directory file)))
        (when (and dir (not (file-directory-p dir)))
          (make-directory dir t)))
      (with-temp-file file
        (insert (json-encode (hoyogacha--blank-uigf-data)))))))

(defun hoyogacha--dedupe-list (records &optional seen)
  "从 RECORDS（vector）中按 (gacha_type . id) 去重，返回去重后的 vector。
SEEN 为可选的哈希表，用于存储已出现的 key；若未提供则创建新的。"
  (let ((seen (or seen (make-hash-table :test #'equal)))
        (unique '()))
    (cl-loop for r across records do
      (let* ((id (map-elt r 'id))
             (gacha-type (map-elt r 'gacha_type))
             (key (cons gacha-type id)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push r unique))))
    (apply #'vector (nreverse unique))))
;;; 从 UIGF 数据中提取所有记录（用于统计）
;;; data 是 UIGF alist；hkrpg/nap 字段是 vector，list 字段也是 vector
(defun hoyogacha--uigf-records (data)
  "从 UIGF DATA（alist）中提取所有抽卡记录，返回列表。"
  (let (records)
    (dolist (game-key hoyogacha--uigf-game-keys)
      (let ((game-data (and data (map-elt data game-key))))
        (when (vectorp game-data)
          (cl-loop for entry across game-data do
            (let ((list-vec (map-elt entry 'list)))
              (when (vectorp list-vec)
                (cl-loop for r across list-vec do
                  (push r records))))))))
    (nreverse records)))

;;; 修改后的合并函数
(defun hoyogacha--merge-uigf-sources (&rest sources)
  "合并多个 UIGF 数据（alist），按 (game, uid, gacha_type, id) 去重，返回合并后的 UIGF alist。"
  (let* ((blank (hoyogacha--blank-uigf-data))
         (info (or (and sources (map-elt (car sources) 'info))
                   (map-elt blank 'info)))
         ;; 收集表：key 为 (game-key . uid)（uid 保留原类型），value 为记录 vector
         (collected (make-hash-table :test #'equal))
         ;; 元信息表：key 同上，value 为 (timezone . lang)
         (meta (make-hash-table :test #'equal)))
    ;; 遍历所有来源
    (dolist (data sources)
      (dolist (game-key hoyogacha--uigf-game-keys)
        (let ((game-data (and data (map-elt data game-key))))
          (when (vectorp game-data)
            (cl-loop for entry across game-data do
              (let* ((uid (map-elt entry 'uid))
                     (key (and game-key uid (cons game-key uid)))
                     (list-vec (map-elt entry 'list)))
                (when (and key (vectorp list-vec))
                  ;; 收集该 (game, uid) 的所有遍历记录
                  (let ((old (gethash key collected)))
                    (puthash key (if old (vconcat old list-vec) (copy-sequence list-vec))
                             collected))
                  ;; 保存元信息（优先第一次出现的）
                  (unless (gethash key meta)
                    (puthash key (cons (map-elt entry 'timezone)
                                       (map-elt entry 'lang))
                             meta)))))))))
    ;; 构建结果
    (let ((result (list (cons 'info (copy-tree info)))))
      (dolist (game-key hoyogacha--uigf-game-keys)
        (let (entries)
          (maphash
           (lambda (key records)
             (when (eq (car key) game-key)
               (let* ((uid (cdr key))
                      (meta-info (gethash key meta))
                      (tz (car meta-info))
                      (lang (cdr meta-info))
                      (unique-list (hoyogacha--dedupe-list records)))
                 (push (list (cons 'uid uid)
                             (cons 'timezone tz)
                             (cons 'lang lang)
                             (cons 'list unique-list))
                       entries))))
           collected)
          (push (cons game-key (apply #'vector entries)) result)))
      (nreverse result))))

(defun hoyogacha-merge-data (&optional save-file import-path)
  "从 SAVE-FILE 和 IMPORT-PATH 下所有 UIGF JSON 文件合并抽卡数据。
SAVE-FILE 或 IMPORT-PATH 为 nil 时使用 `hoyogacha-data-save-file' 和
`hoyogacha-data-import-dir'。如果 SAVE-FILE 不存在，则先创建空白 UIGF 文件。
合并结果存入 `hoyogacha-merged-data' 并返回。"
  (interactive)
  (setq save-file (or save-file hoyogacha-data-save-file)
        import-path (or import-path hoyogacha-data-import-dir))
  (unless save-file
    (user-error "未指定 hoyogacha-data-save-file"))
  ;; 同步全局变量，便于后续自动保存
  (setq hoyogacha-data-save-file save-file)
  (when import-path
    (setq hoyogacha-data-import-dir import-path))
  ;; 确保保存文件存在
  (hoyogacha--ensure-save-file save-file)
  (let ((all-sources (list (hoyogacha--read-uigf-file save-file))))
    ;; 读取导入目录
    (when (and import-path (not (string-empty-p import-path)))
      (setq all-sources (nconc (hoyogacha-read-json-files import-path)
                               all-sources)))
    (setq hoyogacha-merged-data
          (apply #'hoyogacha--merge-uigf-sources
                 (delq nil all-sources)))
    hoyogacha-merged-data))

(defun hoyogacha--save-merged-data ()
  "将 =hoyogacha-merged-data' 保存到 =hoyogacha-data-save-file'。
保存时强制写入 hoyogacha.el 自己的 info，不继承导入源的元数据。"
  (when (and hoyogacha-data-save-file hoyogacha-merged-data)
    (let ((file hoyogacha-data-save-file)
          (data (copy-tree hoyogacha-merged-data)))
      ;; 替换 info 为插件自己的信息
      (map-put! data 'info (map-elt (hoyogacha--blank-uigf-data) 'info))
      (when (and (file-name-directory file)
                 (not (file-directory-p (file-name-directory file))))
        (make-directory (file-name-directory file) t))
      (with-temp-file file
        (insert (json-encode data))))))

(defun hoyogacha--uigf-records (data)
  "从 UIGF DATA（alist）中提取所有抽卡记录，返回列表。"
  (let (records)
    (dolist (game-key hoyogacha--uigf-game-keys)
      (let ((game-data (and data (map-elt data game-key))))
        (when (vectorp game-data)
          (dotimes (i (length game-data))
            (let* ((entry (aref game-data i))
                   (list-vec (map-elt entry 'list)))
              (when (vectorp list-vec)
                (setq records (append records (append list-vec nil)))))))))
    records))

(defun hoyogacha-stats-buffer (records &optional data)
  "根据 RECORDS 生成只读统计 buffer，并显示目录信息。
DATA 为合并后的 UIGF alist；若提供则更新 `hoyogacha-merged-data'。
生成的 buffer 关闭时会自动保存 `hoyogacha-merged-data' 到
`hoyogacha-data-save-file'。"
  (when data
    (setq hoyogacha-merged-data data))
  (let ((buf (get-buffer-create "*抽卡统计*")))
    (with-current-buffer buf
      (remove-hook 'kill-buffer-hook #'hoyogacha--save-merged-data t)
      (when hoyogacha-merged-data
        (add-hook 'kill-buffer-hook #'hoyogacha--save-merged-data nil t)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "📊 抽卡数据分析报告\n\n"))
      (insert (format "总抽卡记录数: %d\n\n" (length records)))

      ;; 按 rank_type 分组统计
      (let ((rank-groups (cl-loop for r in records
                                  for rank = (map-elt r 'rank_type)
                                  when rank
                                  collect rank)))
        (insert "按星级 (rank_type) 统计：\n")
        (dolist (rank '("5" "4" "3" "2"))
          (let ((count (cl-count rank rank-groups :test 'equal)))
            (when (> count 0)
              (insert (format "  ★%s 星: %d 个\n" rank count))))))

      (insert "\n（更多详细统计可按需扩展）\n")
      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer buf)))

(defun hoyogacha-show ()
  "导入并合并抽卡数据，然后显示统计 buffer。"
  (interactive)
  (let ((data (hoyogacha-merge-data)))
    (hoyogacha-stats-buffer (hoyogacha--uigf-records data) data)))


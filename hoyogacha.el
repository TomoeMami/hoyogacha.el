;;; hoyogacha.el --- Emacs 中管理米哈游抽卡记录的插件 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 TomoeMami

;; Author: TomoeMami <trembleafterme@outlook.com>
;; Created: 2026.08

;; URL: https://github.com/TomoeMami/hoyogacha.el
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.1") (plz "0.9"))

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
;; 异步请求基础设施
;; ------------------------------------------------------------

(defvar hoyogacha--fetch-process nil
  "当前进行中的 plz 异步请求进程，用于取消操作。")

(defvar hoyogacha--fetch-canceled-p nil
  "非 nil 表示用户已请求取消当前后台拉取任务。")

(defvar hoyogacha--fetching-p nil
  "非 nil 表示当前有一个后台拉取任务正在进行（用于防止重入）。")

(defun hoyogacha--plz-error-string (err)
  "返回 ERR（plz-error 结构体）的可读描述字符串。"
  (or (plz-error-message err)
      (let ((resp (plz-error-response err)))
        (when resp (format "HTTP %s" (plz-response-status resp))))
      (let ((curl (plz-error-curl-error err)))
        (when curl (format "curl %s: %s" (car curl) (cdr curl))))
      "未知错误"))

(defun hoyogacha--after-delay (seconds fn)
  "SECONDS 秒后调用 FN；SECONDS 小于等于 0 时立即调用。"
  (if (<= seconds 0)
      (funcall fn)
    (run-at-time seconds nil fn)))

;; ------------------------------------------------------------
;; 相关代码的映射
;; ------------------------------------------------------------

(defvar hoyogacha-games
  '((hsr
     :locallow "崩坏：星穹铁道"
     :log-prefix "Loading player data from "
     :log-suffix "data.unity3d"
     :data-key hkrpg
     :gacha-types ("1" "2" "11" "12" "21" "22")
     :gacha-type-names (("1"  . "常驻跃迁")
                        ("2"  . "新手跃迁")
                        ("11" . "角色活动跃迁")
                        ("12" . "光锥活动跃迁")
                        ("21" . "角色联动跃迁")
                        ("22" . "光锥联动跃迁"))
     :rank-type-names (("3" . "三星")
                       ("4" . "四星")
                       ("5" . "五星"))
     :high-rank-code "5"
     :permanent-gacha-types ("1" "2"))
    (zzz
     :locallow "绝区零"
     :log-prefix "[Subsystems] Discovering subsystems at path "
     :log-suffix "UnitySubsystems"
     :data-key nap
     :gacha-types ("1" "2" "3" "5" "102" "103")
     :gacha-type-names (("1"   . "常驻频段")
                        ("2"   . "独家频段")
                        ("3"   . "音擎频段")
                        ("5"   . "邦布频段")
                        ("102" . "独家重映")
                        ("103" . "音擎回响"))
     :rank-type-names (("2" . "B")
                       ("3" . "A")
                       ("4" . "S"))
     :high-rank-code "4"
     :character-item-types ("代理人" "角色")
     :permanent-gacha-types ("1" "5")))
  "支持的游戏配置。
每个条目包含：
- :data-key      导出 JSON 中对应的顶层 key（符号）
- :display-name  显示名称
- :gacha-types   该游戏可能的 gacha_type 值（字符串列表，用于拉取记录）
- :gacha-type-names gacha_type 代码到显示名称的 alist（key 为字符串）
- :rank-type-names  rank_type 代码到显示名称的 alist（key 为字符串）
- :high-rank-code      最高稀有度对应的 rank_type 字符串
- :permanent-gacha-types 常驻卡池的 gacha_type 字符串列表")

;; ------------------------------------------------------------
;; 获取抽卡记录链接
;; ------------------------------------------------------------

(defvar hoyogacha-last-warp-url nil
  "最近定位到的抽卡记录链接。")

(defcustom hoyogacha-zzz-install-dir nil
  "绝区零的游戏数据目录（包含 webCaches 文件夹的目录）。
例如：\"D:/Apps/ZenlessZoneZero Game/ZenlessZoneZero_Data\"。
设置后，获取绝区零抽卡链接时会直接使用此目录。"
  :type '(choice (const :tag "未设置" nil) directory)
  :group 'hoyogacha)

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
         (concat "LocalLow/miHoYo/" (map-elt (hoyogacha--game-config game) :locallow))
         (expand-file-name ".." appdata))
      (user-error "未找到 APPDATA 环境变量"))))

(defun hoyogacha--detect-game ()
  "自动检测已安装的游戏，返回 'hsr 或 'zzz。"
  (let ((hsr-dir (ignore-errors (hoyogacha--locallow-dir 'hsr)))
        (zzz-dir (and hoyogacha-zzz-install-dir
                      (file-directory-p hoyogacha-zzz-install-dir)
                      hoyogacha-zzz-install-dir)))
    (cond
     ((and hsr-dir (file-directory-p hsr-dir) zzz-dir)
      'hsr)  ; 两者都有，默认 hsr
     (hsr-dir 'hsr)
     (zzz-dir 'zzz)
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
  (let* ((config (hoyogacha--game-config game))
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
    (if (eq game 'zzz)
        ;; 绝区零：只使用自定义安装目录
        (when (and hoyogacha-zzz-install-dir
                   (file-directory-p hoyogacha-zzz-install-dir))
          (file-name-as-directory hoyogacha-zzz-install-dir))
      ;; 星穹铁道：继续使用 LocalLow 定位
      (let ((locallow (hoyogacha--locallow-dir game))
            (log-file (expand-file-name "Player.log"
                                        (hoyogacha--locallow-dir game))))
        (or (when (file-exists-p log-file)
              (hoyogacha--game-dir-from-log-file log-file game))
            (let ((prev-log-file (expand-file-name "Player-prev.log" locallow)))
              (when (file-exists-p prev-log-file)
                (hoyogacha--game-dir-from-log-file prev-log-file game))))))))

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
                     collect (cons name dir))))
         ;; 按版本号从大到小排序
         (version-files
          (mapcar (lambda (entry)
                    (expand-file-name "Cache/Cache_Data/data_2" (cdr entry)))
                  (sort version-dirs
                        (lambda (a b) (version< (car b) (car a))))))
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
                                   (file-attribute-modification-time (file-attributes a))))))))
            (let ((file (expand-file-name "Cache/Cache_Data/data_2" latest-dir)))
              (when (file-exists-p file)
                file)))))))

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

(defun hoyogacha--warp-url-valid-async (url game ok fail)
  "异步请求 URL 并检查 retcode 是否为 0。
有效时调用 (funcall OK)；否则调用 (funcall FAIL MSG)。
GAME 为当前游戏符号，用于错误消息。"
  (setq hoyogacha--fetch-process
        (plz 'get url
          :headers '(("User-Agent" . "Mozilla/5.0")
                     ("Accept" . "application/json"))
          :as #'json-read
          :timeout 10
          :then (lambda (data)
                  (if (equal (map-elt data 'retcode) 0)
                      (funcall ok)
                    (funcall fail
                             (format "[%s] 找到的抽卡链接已失效，请重新在游戏中打开抽卡记录"
                                     (symbol-name game)))))
          :else (lambda (err)
                  (funcall fail
                           (format "[%s] 抽卡链接验证失败：%s"
                                   (symbol-name game)
                                   (hoyogacha--plz-error-string err)))))))

(defun hoyogacha--locate-raw-warp-url (game-or-path)
  "定位并返回 (GAME . RAW-URL)，不进行网络验证。
GAME-OR-PATH 的取值规则同 `hoyogacha-get-warp-url'。"
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
      (unless (hoyogacha--game-config game)
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
        (cons game raw-url)))))

(defun hoyogacha-get-warp-url (&optional game-or-path)
  "定位并返回抽卡记录链接（同步，会阻塞 Emacs）。

GAME-OR-PATH 可以是：
- 目录路径（字符串） —— 直接作为游戏安装目录；
- 符号 'hsr 或 'zzz —— 指定游戏；
- 字符串 \"hsr\" 或 \"zzz\" —— 指定游戏；
- nil —— 自动检测。

返回值为清理后的链接，同时存入 `hoyogacha-last-warp-url' 并复制到 kill-ring。
交互场景推荐使用非阻塞的 `hoyogacha-get-warp-url-async'。"
  (let* ((loc (hoyogacha--locate-raw-warp-url game-or-path))
         (game (car loc))
         (raw-url (cdr loc)))
    (if (hoyogacha--warp-url-valid-p raw-url game)
        (let ((url (hoyogacha--clean-warp-url raw-url)))
          (setq hoyogacha-last-warp-url url)
          (kill-new url)
          url)
      (user-error "[%s] 找到的抽卡链接已失效，请重新在游戏中打开抽卡记录"
                  (symbol-name game)))))

(defun hoyogacha-get-warp-url-async (game-or-path ok &optional fail)
  "异步定位并返回抽卡记录链接（不阻塞 Emacs）。

成功时调用 (funcall OK URL)，URL 已清理、存入 `hoyogacha-last-warp-url'
并复制到 kill-ring；失败时调用 (funcall FAIL MSG)，FAIL 缺省时 message 报错。"
  (let* ((loc (hoyogacha--locate-raw-warp-url game-or-path))
         (game (car loc))
         (raw-url (cdr loc))
         (fail (or fail (lambda (msg) (message "%s" msg)))))
    (hoyogacha--warp-url-valid-async
     raw-url game
     (lambda ()
       (let ((url (hoyogacha--clean-warp-url raw-url)))
         (setq hoyogacha-last-warp-url url)
         (kill-new url)
         (funcall ok url)))
     fail)))

;; ------------------------------------------------------------
;; 根据抽卡记录链接拉取记录
;; ------------------------------------------------------------


(defun hoyogacha--build-url (url &rest params)
  "在 URL 上附加查询参数 PARAMS，返回新 URL。
PARAMS 是键值交替的列表，如 (\"gacha_type\" \"11\" \"page\" \"1\")。"
  (let ((query-string
         (mapconcat (lambda (pair)
                      (format "%s=%s" (car pair) (cdr pair)))
                    (cl-loop for (key val) on params by #'cddr
                             collect (cons key val))
                    "&")))
    (concat url
            (if (string-match-p "\\?" url) "&" "?")
            query-string)))

(defun hoyogacha--build-gacha-request-url (url game gacha-type page size end-id)
  "根据抽卡链接 URL 构建 GAME 的一页请求 URL。
GACHA-TYPE、PAGE、SIZE、END-ID 为查询参数；END-ID 为 nil 时不附带 end_id。"
  (let* ((type-str (and gacha-type (format "%s" gacha-type)))
         (page-str (and page (format "%s" page)))
         (size-str (and size (format "%s" size)))
         ;; 关键：end-id 为 nil 时，end-id-str 也为 nil，不会加入 params
         (end-id-str (and end-id (format "%s" end-id)))
         ;; HSR 联动池使用单独的接口
         (target-url (if (member type-str '("21" "22"))
                         (replace-regexp-in-string
                          "\\(/getGachaLog\\)\\([?]\\|\\'\\)"
                          "/getLdGachaLog\\2"
                          url)
                       url))
         (params (append (and type-str (if (eq game 'zzz)
                                           (list "real_gacha_type" type-str)
                                         (list "gacha_type" type-str)))
                         (and page-str (list "page" page-str))
                         (and size-str (list "size" size-str))
                         (and end-id-str (list "end_id" end-id-str)))))
    (apply #'hoyogacha--build-url target-url params)))

(defun hoyogacha--request-gacha-page (url game &optional gacha-type page size end-id)
  "请求 GAME 的抽卡日志的一页（同步，会阻塞 Emacs）。
GACHA-TYPE、PAGE、SIZE、END-ID 为查询参数。
END-ID 为 nil 时不附带 end_id 参数。"
  ;; 限速：每次请求前固定等待 1 秒
  (sleep-for 1)
  (let* ((full-url (hoyogacha--build-gacha-request-url
                    url game gacha-type page size end-id))
         (response (condition-case-unless-debug err
                       (plz 'get full-url
                            :headers '(("User-Agent" . "Mozilla/5.0"))
                            :as #'json-read
                            :timeout 15)
                     (plz-error
                      (error "[%s] 抽卡日志请求失败：%s"
                             (symbol-name game)
                             (error-message-string err))))))
    (unless (equal (map-elt response 'retcode) 0)
      (error "[%s] 抽卡日志返回错误：%s"
             (symbol-name game)
             (or (map-elt response 'message) "未知错误")))
    response))

(defun hoyogacha--request-gacha-page-async (url game gacha-type page end-id ok fail)
  "异步请求 GAME 的抽卡日志的一页（不阻塞 Emacs）。
成功（retcode=0）时调用 (funcall OK RESPONSE)；否则调用 (funcall FAIL MSG)。
不包含限速延迟，由调用方负责。"
  (let ((full-url (hoyogacha--build-gacha-request-url
                   url game gacha-type page 20 end-id)))
    (setq hoyogacha--fetch-process
          (plz 'get full-url
            :headers '(("User-Agent" . "Mozilla/5.0"))
            :as #'json-read
            :timeout 15
            :then (lambda (response)
                    (if (equal (map-elt response 'retcode) 0)
                        (funcall ok response)
                      (funcall fail
                               (format "[%s] 抽卡日志返回错误：%s"
                                       (symbol-name game)
                                       (or (map-elt response 'message) "未知错误")))))
            :else (lambda (err)
                    (funcall fail
                             (format "[%s] 抽卡日志请求失败：%s"
                                     (symbol-name game)
                                     (hoyogacha--plz-error-string err))))))))

(defun hoyogacha--local-gacha-ids (data game uid &optional gacha-type)
  "Return a hash table of record IDs for GAME and UID in DATA.
If GACHA-TYPE is non-nil, only include records with that gacha_type.
UID is compared as a string."
  (let ((ids (make-hash-table :test #'equal))
        (game-key (map-elt (map-elt hoyogacha-games game) :data-key))
        (type-str (and gacha-type (format "%s" gacha-type)))
        (uid-str (format "%s" uid)))
    (when data
      (let ((game-vec (map-elt data game-key)))
        (when (vectorp game-vec)
          (dotimes (i (length game-vec))
            (let* ((entry (aref game-vec i))
                   (entry-uid (map-elt entry 'uid))
                   (list-vec (map-elt entry 'list)))
              ;; 仅处理 UID 匹配的条目
              (when (and entry-uid
                         (string= (format "%s" entry-uid) uid-str)
                         (vectorp list-vec))
                (dotimes (j (length list-vec))
                  (let ((rec (aref list-vec j)))
                    (when (and (or (null type-str)
                                   (and (map-elt rec 'gacha_type)
                                        (string= (format "%s" (map-elt rec 'gacha_type))
                                                 type-str)))
                               (map-elt rec 'id))
                      (puthash (format "%s" (map-elt rec 'id)) t ids))))))))))
    ids))

(defun hoyogacha--make-uigf-source (game uid records)
  "Return a UIGF alist containing RECORDS for GAME under UID.
UID 应为字符串；RECORDS 是 record alist 列表。"
  (list (cons 'info (map-elt (hoyogacha--blank-uigf-data) 'info))
        (cons (map-elt (map-elt hoyogacha-games game) :data-key)
              (vector
               (list (cons 'uid uid)
                     (cons 'timezone nil)
                     (cons 'lang nil)
                     (cons 'list (vconcat records)))))))

(defun hoyogacha--fetch-and-merge-gacha-data (data game url)
  "Fetch gacha records for GAME from URL and merge into DATA.

返回 (DATA . INSERTED-COUNT)。
每个池子只与该池子内、同一 UID 下已有的 ID 比对，遇到重复即停止该池。"
  (let* ((config (map-elt hoyogacha-games game))
         (gacha-types (map-elt config :gacha-types))
         (new-records '())
         (inserted 0))
    (dolist (type gacha-types)
      (message "[%s] 拉取 gacha_type=%s ..." (symbol-name game) type)
      (let ((page 1)
            (end-id nil)                ; 第一页不传 end_id
            (empty-pages 0)
            (prev-first-id nil)
            (uid nil)                   ; 拉到第一页后确定
            (pool-local-ids nil)        ; 确定 UID 后构建
            (stop nil))
        (while (not stop)
          (let ((attempt 0)
                (success nil))
            (while (and (not success) (< attempt 3))
              (setq attempt (1+ attempt))
              (condition-case err
                  (let* ((response (hoyogacha--request-gacha-page
                                    url game type page 20 end-id))
                         (data-node (map-elt response 'data))
                         (raw-list (and data-node (map-elt data-node 'list)))
                         (records (cond ((null raw-list) nil)
                                        ((vectorp raw-list) (append raw-list nil))
                                        (t raw-list))))
                    ;; 处理本页记录
                    (if (null records)
                        (progn
                          (setq empty-pages (1+ empty-pages))
                          (if (>= empty-pages 2)
                              (setq stop t)
                            (setq page (1+ page)))
                          (setq success t))   ; 空页也视为成功
                      ;; 非空页
                      (setq empty-pages 0)
                      ;; 如果是第一页，获取 UID 并初始化该 UID 的本地 ID 表
                      (unless uid
                        (setq uid (map-elt (car records) 'uid))
                        (unless uid
                          (error "[%s] 获取的记录缺少 uid" (symbol-name game)))
                        (setq uid (format "%s" uid))
                        (setq pool-local-ids (hoyogacha--local-gacha-ids data game uid type)))
                      ;; 循环保护：若本页第一条 id 与上一页相同，强制停止
                      (let* ((first-id (map-elt (car records) 'id))
                             (first-id-str (and first-id (format "%s" first-id))))
                        (when (and prev-first-id first-id-str
                                   (string= first-id-str prev-first-id))
                          (setq stop t))
                      (unless stop
                        (let ((pool-done
                               (catch 'hoyogacha--pool-done
                                 (dolist (rec records)
                                   (let ((id (map-elt rec 'id)))
                                     (when id
                                       (setq id (format "%s" id))
                                       (when (gethash id pool-local-ids)
                                         (throw 'hoyogacha--pool-done t))
                                       (puthash id t pool-local-ids)
                                       (push rec new-records)
                                       (setq inserted (1+ inserted)))))
                                 nil)))
                          (cond
                           (pool-done
                            (setq stop t))
                           ;; 注意：不能用「本页不足 20 条」判断结束——
                           ;; 绝区零(nap) API 每页固定返回 5 条（无视 size=20），
                           ;; 按 20 判断会提前停止，漏掉后续记录。
                           ;; 终止条件依靠：pool-done、连续空页、end_id 循环保护。
                           (t
                            (let* ((last-rec (car (last records)))
                                   (last-id (map-elt last-rec 'id)))
                              (if (or (null last-id)
                                      ;; 如果上一页已经有 end-id，且本页最后 id 与之一致
                                      (and end-id
                                           (string= (format "%s" last-id) end-id)))
                                  (setq stop t)
                                (setq end-id (format "%s" last-id))
                                (setq page (1+ page))))))
                          (setq prev-first-id first-id-str)))
                      (setq success t))))   ; 成功完成本页处理
                (error
                 (message "[%s] gacha_type=%s 第 %d 页请求失败（尝试 %d/3）：%s"
                          (symbol-name game) type page attempt
                          (error-message-string err))
                 (when (= attempt 3)
                   (setq stop t)))))))))
    (if new-records
        (let* ((first-rec (car new-records))
               (uid (map-elt first-rec 'uid)))
          (unless uid
            (error "[%s] 获取的记录缺少 uid" (symbol-name game)))
          (let* ((uid-str (format "%s" uid))
                 (source (hoyogacha--make-uigf-source
                          game uid-str (nreverse new-records))))
            (cons (hoyogacha--merge-uigf-sources data source) inserted)))
      (cons data inserted))))

(defun hoyogacha--fetch-and-merge-gacha-data-async (data game url done)
  "异步抓取 GAME 的抽卡记录并合并进 DATA（不阻塞 Emacs）。

抓取全部完成后调用 (funcall DONE NEW-DATA INSERTED-COUNT)；中途取消时
同样以已有结果调用 DONE。每次请求间保持 1 秒限速，每页失败最多重试 3 次。"
  (setq hoyogacha--fetch-canceled-p nil)
  (let* ((config (map-elt hoyogacha-games game))
         (gacha-types (map-elt config :gacha-types))
         (new-records '())
         (inserted 0)
         (types-left gacha-types))
    (cl-labels
        ((finish ()
           ;; 收尾：合并新增记录并回调 DONE（异步回调内不能抛出未捕获错误）
           (if new-records
               (let* ((first-rec (car new-records))
                      (uid (map-elt first-rec 'uid)))
                 (if uid
                     (let* ((uid-str (format "%s" uid))
                            (source (hoyogacha--make-uigf-source
                                     game uid-str (nreverse new-records))))
                       (funcall done
                                (hoyogacha--merge-uigf-sources data source)
                                inserted))
                   (message "[%s] 获取的记录缺少 uid，本次新增记录未保存"
                            (symbol-name game))
                   (funcall done data 0)))
             (funcall done data inserted)))
         (run-type ()
           (if hoyogacha--fetch-canceled-p
               (finish)
             (if (null types-left)
                 (finish)
               (let* ((type (pop types-left))
                      (state (list :type type
                                   :page 1 :end-id nil :empty-pages 0
                                   :prev-first-id nil :uid nil
                                   :pool-local-ids nil :attempt 0 :stop nil)))
                 (message "[%s] 拉取 gacha_type=%s ..." (symbol-name game) type)
                 (fetch-page state)))))
         (fetch-page (state)
           (if (map-elt state :stop)
               (run-type)
             (hoyogacha--after-delay
              1
              (lambda ()
                (if hoyogacha--fetch-canceled-p
                    (finish)
                  (hoyogacha--request-gacha-page-async
                   url game (map-elt state :type) (map-elt state :page)
                   (map-elt state :end-id)
                   (lambda (response) (handle-page state response))
                   (lambda (msg) (handle-error state msg))))))))
         (handle-page (state response)
           (condition-case err
               (let* ((data-node (map-elt response 'data))
                      (raw-list (and data-node (map-elt data-node 'list)))
                      (records (cond ((null raw-list) nil)
                                     ((vectorp raw-list) (append raw-list nil))
                                     (t raw-list))))
                 (if (null records)
                     ;; 空页：连续两次则停止该池
                     (progn
                       (setf (map-elt state :empty-pages)
                             (1+ (map-elt state :empty-pages)))
                       (if (>= (map-elt state :empty-pages) 2)
                           (setf (map-elt state :stop) t)
                         (setf (map-elt state :page) (1+ (map-elt state :page))))
                       (setf (map-elt state :attempt) 0)
                       (fetch-page state))
                   ;; 非空页
                   (setf (map-elt state :empty-pages) 0)
                   ;; 如果是第一页，获取 UID 并初始化该 UID 的本地 ID 表
                   (unless (map-elt state :uid)
                     (let ((uid (map-elt (car records) 'uid)))
                       (unless uid
                         (error "[%s] 获取的记录缺少 uid" (symbol-name game)))
                       (setf (map-elt state :uid) (format "%s" uid))
                       (setf (map-elt state :pool-local-ids)
                             (hoyogacha--local-gacha-ids
                              data game (map-elt state :uid)
                              (map-elt state :type)))))
                   ;; 循环保护：若本页第一条 id 与上一页相同，强制停止
                   (let* ((first-id (map-elt (car records) 'id))
                          (first-id-str (and first-id (format "%s" first-id))))
                     (when (and (map-elt state :prev-first-id) first-id-str
                                (string= first-id-str
                                         (map-elt state :prev-first-id)))
                       (setf (map-elt state :stop) t))
                     (unless (map-elt state :stop)
                       (let ((pool-done
                              (catch 'hoyogacha--pool-done
                                (dolist (rec records)
                                  (let ((id (map-elt rec 'id)))
                                    (when id
                                      (setq id (format "%s" id))
                                      (when (gethash id (map-elt state :pool-local-ids))
                                        (throw 'hoyogacha--pool-done t))
                                      (puthash id t (map-elt state :pool-local-ids))
                                      (push rec new-records)
                                      (setq inserted (1+ inserted)))))
                                nil)))
                         (cond
                          (pool-done
                           (setf (map-elt state :stop) t))
                          ;; 注意：不能用「本页不足 20 条」判断结束——
                          ;; 绝区零(nap) API 每页固定返回 5 条（无视 size=20），
                          ;; 按 20 判断会提前停止，漏掉后续记录。
                          ;; 终止条件依靠：pool-done、连续空页、end_id 循环保护。
                          (t
                           (let* ((last-rec (car (last records)))
                                  (last-id (map-elt last-rec 'id)))
                             (if (or (null last-id)
                                     ;; 如果上一页已经有 end-id，且本页最后 id 与之一致
                                     (and (map-elt state :end-id)
                                          (string= (format "%s" last-id)
                                                   (map-elt state :end-id))))
                                 (setf (map-elt state :stop) t)
                               (setf (map-elt state :end-id) (format "%s" last-id))
                               (setf (map-elt state :page)
                                     (1+ (map-elt state :page))))))))
                       (setf (map-elt state :prev-first-id) first-id-str)))
                   (setf (map-elt state :attempt) 0)
                   (fetch-page state)))
             (error (handle-error state (error-message-string err)))))
         (handle-error (state msg)
           (if hoyogacha--fetch-canceled-p
               (finish)
             (let ((attempt (1+ (map-elt state :attempt))))
               (setf (map-elt state :attempt) attempt)
               (message "[%s] gacha_type=%s 第 %d 页请求失败（尝试 %d/3）：%s"
                        (symbol-name game) (map-elt state :type)
                        (map-elt state :page) attempt msg)
               (if (>= attempt 3)
                   (progn
                     (setf (map-elt state :stop) t)
                     (run-type))
                 (fetch-page state))))))
      (run-type))))

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
             ;; 归一化为字符串：不同来源可能把 id/gacha_type 存成数字或字符串，
             ;; 不归一化会导致同一条记录被重复保留。
             (key (cons (format "%s" gacha-type) (format "%s" id))))
        (unless (gethash key seen)
          (puthash key t seen)
          (push r unique))))
    (apply #'vector (nreverse unique))))

;;; 修改后的合并函数
(defun hoyogacha--merge-uigf-sources (&rest sources)
  "合并多个 UIGF 数据（alist），按 (game, uid, gacha_type, id) 去重，返回合并后的 UIGF alist。"
  (let* ((blank (hoyogacha--blank-uigf-data))
         (info (or (and sources (map-elt (car sources) 'info))
                   (map-elt blank 'info)))
         ;; 收集表：key 为 (game-key . uid)，uid 归一化为字符串
         ;;（导入的 UIGF 文件可能把 uid 存成数字，与本插件写入的字符串
         ;; 不一致会导致同一账号被拆成两条记录）
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
                     (uid-str (and uid (format "%s" uid)))
                     (key (and game-key uid-str (cons game-key uid-str)))
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

;; ------------------------------------------------------------
;; 数据分析与展示
;; ------------------------------------------------------------

(defcustom hoyogacha-name-abbreviations
  '((hsr . nil)
    (zzz . nil))
  "角色名称缩写表。
格式：((hsr . ((\"全名\" . \"缩写\") ...))
        (zzz . ((\"全名\" . \"缩写\") ...)))
用于表格对齐，未收录的名字按原名显示。"
  :type '(alist :key-type (choice (const hsr) (const zzz))
                :value-type (alist :key-type string :value-type string))
  :group 'hoyogacha)

(defcustom hoyogacha-uid-aliases nil
  "UID 别名表，用于在分析界面显示便于识别的名字。
格式：((\"UID\" . \"别名\") ...)"
  :type '(alist :key-type string :value-type string)
  :group 'hoyogacha)

(defcustom hoyogacha-character-standard-schedule
  '((hsr . (("姬子" . "2023-01-01 00:00:00")
            ("布洛妮娅" . "2023-01-01 00:00:00")
            ("杰帕德" . "2023-01-01 00:00:00")
            ("克拉拉" . "2023-01-01 00:00:00")
            ("彦卿" . "2023-01-01 00:00:00")
            ("瓦尔特" . "2023-01-01 00:00:00")
            ("白露" . "2023-01-01 00:00:00")
            ("时节不居". "2023-01-01 00:00:00")
            ("但战斗还未结束". "2023-01-01 00:00:00")
            ("制胜的瞬间". "2023-01-01 00:00:00")
            ("如泥酣眠". "2023-01-01 00:00:00")
            ("银河铁道之夜". "2023-01-01 00:00:00")
            ("无可取代的东西". "2023-01-01 00:00:00")
            ("以世界之名". "2023-01-01 00:00:00")
            ("希儿" . "2025-04-09 06:00:00")
            ("符玄" . "2025-04-09 06:00:00")
            ("刃" . "2025-04-09 06:00:00")
            ("云璃" . "2026-04-22 06:00:00")
            ("银枝" . "2026-04-22 06:00:00")
            ("银狼" . "2026-04-22 06:00:00")))
    (zzz . (("「11号」" . "2024-07-01 00:00:00")
            ("猫又" . "2024-07-01 00:00:00")
            ("格莉丝" . "2024-07-01 00:00:00")
            ("珂蕾妲" . "2024-07-01 00:00:00")
            ("莱卡恩" . "2024-07-01 00:00:00")
            ("丽娜" . "2024-07-01 00:00:00")
            ("钢铁肉垫" . "2024-07-01 00:00:00")
            ("硫磺石" . "2024-07-01 00:00:00")
            ("拘缚者" . "2024-07-01 00:00:00")
            ("燃狱齿轮" . "2024-07-01 00:00:00")
            ("啜泣摇篮" . "2024-07-01 00:00:00")
            ("嵌合编译器" . "2024-07-01 00:00:00")
            ("柳" . "2026-07-29 06:00:00")
            ("朱鸢" . "2026-07-29 06:00:00")
            ("凯撒" . "2026-07-29 06:00:00")
            ("防暴者VI型" . "2026-07-29 06:00:00")
            ("时流贤者" . "2026-07-29 06:00:00")
            ("奔袭獠牙" . "2026-07-29 06:00:00"))))
  "常驻调整表。
格式：((hsr . ((\"角色名\" . \"加入常驻时间\") ...))
        (zzz . ((\"角色名\" . \"加入常驻时间\") ...)))
时间格式为 \"YYYY-MM-DD HH:MM:SS\"。

判定规则：
- 若抽到角色时，该时间已经到达或超过表内时间，则标记为“常”；
- 若早于表内时间，则标记为“限”；
- 若不在表内，则根据卡池是否常驻池判断。"
  :type '(alist :key-type (choice (const hsr) (const zzz))
                :value-type (alist :key-type string :value-type string))
  :group 'hoyogacha)

(defun hoyogacha--uid-alias (uid)
  "返回 UID 的别名；若未设置别名，则返回 UID 本身（字符串形式）。"
  (let* ((uid-str (format "%s" uid))  ; 强制转成字符串
         (alias (cdr (assoc uid-str hoyogacha-uid-aliases))))
    (if (and alias (not (string-empty-p alias)))
        alias
      uid-str)))

(defun hoyogacha--game-config (game)
  "返回 GAME 的配置 plist。"
  (map-elt hoyogacha-games game))

(defun hoyogacha--game-display-name (game)
  "返回 GAME 的显示名称。"
  (or (map-elt (hoyogacha--game-config game) :locallow)
      (symbol-name game)))

(defun hoyogacha--high-rank-p (record game)
  "判断 RECORD 是否为 GAME 的最高稀有度掉落。"
  (equal (format "%s" (map-elt record 'rank_type))
         (map-elt (hoyogacha--game-config game) :high-rank-code)))

(defun hoyogacha--high-rank-name (game)
  "返回 GAME 的最高稀有度显示名称，如 \"五星\" 或 \"S\"。"
  (let ((config (hoyogacha--game-config game)))
    (or (cdr (assoc (map-elt config :high-rank-code)
                    (map-elt config :rank-type-names)))
        (map-elt config :high-rank-code))))

(defun hoyogacha--sort-records-chronologically (records)
  "按时间、id 升序排序抽卡记录。返回新列表。"
  (sort (copy-sequence records)
        (lambda (a b)
          (let ((ta (map-elt a 'time))
                (tb (map-elt b 'time)))
            (if (and ta tb (not (string= ta tb)))
                (string< ta tb)
              (< (string-to-number (format "%s" (map-elt a 'id)))
                 (string-to-number (format "%s" (map-elt b 'id)))))))))

(defun hoyogacha--compute-constellation-map (records game)
  "为 RECORDS 中每个 UID 计算最高稀有度物品的重复次数（命座/精炼）。
返回哈希表，键为 UID 字符串，值为该 UID 的 const-map（(name . id) -> 次数）。"
  (let ((uid-to-map (make-hash-table :test #'equal)))
    (dolist (uid-group (hoyogacha--group-records-by-uid records))
      (let* ((uid (car uid-group))
             (uid-records (cdr uid-group))
             (counts (make-hash-table :test #'equal))
             (map (make-hash-table :test #'equal)))
        (dolist (rec (hoyogacha--sort-records-chronologically uid-records))
          (when (hoyogacha--high-rank-p rec game)
            (let* ((name (map-elt rec 'name))
                   (id (format "%s" (map-elt rec 'id)))
                   (const (gethash name counts 0)))
              (puthash (cons name id) const map)
              (puthash name (min 6 (1+ const)) counts))))
        (puthash uid map uid-to-map)))
    uid-to-map))

(defun hoyogacha--group-records-by-uid (records)
  "按 UID 分组 RECORDS，返回 ((uid . records) ...)。"
  (let (groups)
    (dolist (rec records)
      (let* ((uid (format "%s" (map-elt rec 'uid)))
             (cell (assoc uid groups)))
        (if cell
            (setcdr cell (cons rec (cdr cell)))
          (push (cons uid (list rec)) groups))))
    (mapcar (lambda (cell)
              (cons (car cell) (nreverse (cdr cell))))
            (nreverse groups))))

(defun hoyogacha--group-records-by-gacha-type (records)
  "按 gacha_type 分组 RECORDS，返回 ((gacha-type-str . records) ...)。"
  (let (groups)
    (dolist (rec records)
      (let* ((type-str (format "%s" (map-elt rec 'gacha_type)))
             (cell (assoc type-str groups)))
        (if cell
            (setcdr cell (cons rec (cdr cell)))
          (push (cons type-str (list rec)) groups))))
    (mapcar (lambda (cell)
              (cons (car cell) (nreverse (cdr cell))))
            (nreverse groups))))

(defun hoyogacha--abbreviate-name (name game)
  "按缩写表返回 GAME 角色 NAME 的缩写；未收录则返回原名。"
  (let ((table (cdr (assq game hoyogacha-name-abbreviations))))
    (or (cdr (assoc name table))
        name)))

(defun hoyogacha--pity-chart (pity)
  "生成 9 格抽数图，并根据 PITY 数值设置颜色。
0-37 绿色，38-75 黄色，76+ 红色。"
  (let* ((blocks (min 9 (floor (max 0 pity) 10)))
         (str (concat (make-string blocks ?#)
                      (make-string (- 9 blocks) ?-)))
         (face (cond ((<= pity 37) 'success)
                     ((<= pity 75) 'warning)
                     (t 'error))))
    (propertize str 'face face)))

(defun hoyogacha--permanent-pool-type-p (gacha-type game)
  "判断 GACHA-TYPE 是否为 GAME 的常驻卡池类型。"
  (member (format "%s" gacha-type)
          (map-elt (hoyogacha--game-config game) :permanent-gacha-types)))

(defun hoyogacha--limited-or-standard-p (record game)
  "根据常驻调整表判断 RECORD 对应角色此时是限/常。"
  (let* ((gacha-type (map-elt record 'gacha_type))
         (name (map-elt record 'name))
         (time (map-elt record 'time))
         (schedule (cdr (assq game hoyogacha-character-standard-schedule)))
         (entry (and name (assoc name schedule))))
    (cond
     ((hoyogacha--permanent-pool-type-p gacha-type game) "常")
     ((and entry time (not (string< time (cdr entry)))) "常")
     (t "限"))))

(defun hoyogacha--analyze-pool (records game &optional const-map-by-uid)
  "分析同一卡池的 RECORDS，自动按 UID 分组独立统计。
若未提供 CONST-MAP-BY-UID，则自动调用 `hoyogacha--compute-constellation-map' 生成。
返回 ((UID . (TOTAL HIGH-COUNT AVG WATER ROWS)) ...)。"
  (let ((const-map-by-uid (or const-map-by-uid
                              (hoyogacha--compute-constellation-map records game)))
        (result '()))
    (dolist (uid-group (hoyogacha--group-records-by-uid records))
      (let* ((uid (car uid-group))
             (uid-records (cdr uid-group))
             (const-map (gethash uid const-map-by-uid (make-hash-table :test #'equal)))
             (sorted (hoyogacha--sort-records-chronologically uid-records))
             (total (length sorted))
             (last-high-idx nil)
             (high-count 0)
             rows)
        (cl-loop for rec in sorted
                 for i from 0
                 do
                 (when (hoyogacha--high-rank-p rec game)
                   (setq high-count (1+ high-count))
                   (let* ((name (map-elt rec 'name))
                          (display-name (hoyogacha--abbreviate-name name game))
                          (id (format "%s" (map-elt rec 'id)))
                          (const (if (and id (not (equal id "nil")))
                                     (gethash (cons name id) const-map 0)
                                   0))
                          (limit-flag (hoyogacha--limited-or-standard-p rec game))
                          (pity (if last-high-idx
                                    (- i last-high-idx)
                                  (1+ i)))
                          (chart (hoyogacha--pity-chart pity))
                          (time (or (map-elt rec 'time) "")))
                     (push (list display-name const limit-flag chart pity time) rows)
                     (setq last-high-idx i))))
        (setq rows (nreverse rows))
        (let ((water (if last-high-idx
                         (- total last-high-idx 1)
                       total))
              (avg (if (> high-count 0)
                       (/ (float total) high-count)
                     0.0)))
          (push (cons uid (list total high-count avg water rows)) result))))
    (nreverse result)))

(defun hoyogacha--pad-right (str width)
  "右补空格到 WIDTH，保留 text property。"
  (let ((s (if (stringp str) str (format "%s" str))))
    (concat s (make-string (max 0 (- width (string-width s))) ?\s))))

(defun hoyogacha--format-analysis-table (rows)
  "将 ROWS 格式化为带颜色的对齐表格字符串。
ROWS 中每行是列表，元素可为字符串或数字。"
  (let* ((string-rows
          (mapcar (lambda (row)
                    (mapcar (lambda (cell) (format "%s" cell)) row))
                  rows))
         (name-width (max 8 (cl-loop for row in string-rows
                                     maximize (string-width (nth 0 row)))))
         (const-width (max 3 (cl-loop for row in string-rows
                                      maximize (string-width (nth 1 row)))))
         (flag-width 3)
         (chart-width 9)
         (pity-width (max 3 (cl-loop for row in string-rows
                                     maximize (string-width (nth 4 row)))))
         (time-width (max 19 (cl-loop for row in string-rows
                                      maximize (string-width (nth 5 row))))))
    (mapconcat
     (lambda (row)
       (let* ((name (nth 0 row))
              (const (nth 1 row))
              (flag (nth 2 row))
              (chart (nth 3 row))
              (pity (nth 4 row))
              (time (nth 5 row))
              (flag-str (cond
                         ((string= flag "限")
                          (propertize "限" 'face 'error))
                         ((string= flag "常")
                          (propertize "常"))
                         (t flag))))
         (concat
          "| " (hoyogacha--pad-right name name-width)
          " | " (hoyogacha--pad-right const const-width)
          " | " (hoyogacha--pad-right flag-str flag-width)
          " | " (hoyogacha--pad-right chart chart-width)
          " | " (hoyogacha--pad-right pity pity-width)
          " | " (hoyogacha--pad-right time time-width)
          " |")))
     string-rows "\n")))

(defun hoyogacha--insert-pool-analysis (game pool-name records &optional const-map-by-uid)
  "为同一个卡池的 RECORDS 插入分析内容（自动处理多 UID）。
若未提供 CONST-MAP-BY-UID，则自动计算。"
  (let ((analysis-list (hoyogacha--analyze-pool records game const-map-by-uid)))
    (if (null analysis-list)
        (insert (format "\n=== %s | %s ===\n"
                        (hoyogacha--game-display-name game) pool-name)
                "（无记录）\n")
      (dolist (item analysis-list)
        (let* ((uid (car item))
               (stats (cdr item))
               (total (nth 0 stats))
               (high-count (nth 1 stats))
               (avg (nth 2 stats))
               (water (nth 3 stats))
               (rows (nth 4 stats))
               (high-name (hoyogacha--high-rank-name game))
               (limited-count (cl-loop for row in rows
                                       when (string= (nth 2 row) "限")
                                       count row))
               (limited-avg (if (> limited-count 0)
                                (/ (float total) limited-count)
                              0)))
          (insert (format "\n=== %s | %s ===\n"
                          (hoyogacha--game-display-name game)
                          pool-name))
          (insert (format "抽卡总次数: %d，%s数量: %d，平均 %.1f 抽一个，限定%s平均 %s 抽一个。当前水位: %d 抽。\n"
                          total high-name high-count avg high-name
                          (if (> limited-count 0)
                              (format "%.1f" limited-avg)
                            "-")
                          water))
          (when rows
            ;; 保持原有倒序显示（最新在上）
            (setq rows (nreverse rows))
            (insert (hoyogacha--format-analysis-table rows) "\n")))))))

(defun hoyogacha--choose-game ()
  "交互式选择要分析的游戏，显示全名（如“崩坏：星穹铁道”/“绝区零”）。"
  (let* ((choices
          (mapcar (lambda (entry)
                    (cons (map-elt (cdr entry) :locallow) (car entry)))
                  hoyogacha-games))
         (choice
          (completing-read
           "选择游戏: "
           (mapcar #'car choices)
           nil t nil nil nil)))
    (cdr (assoc choice choices))))

(defun hoyogacha--records-for-game (data game)
  "从合并数据 DATA 中取出 GAME 的所有 UID 记录。

返回 ((uid . records-list) ...)。"
  (let* ((game-key (map-elt (hoyogacha--game-config game) :data-key))
         (game-vec (map-elt data game-key))
         entries)
    (when (vectorp game-vec)
      (dotimes (i (length game-vec))
        (let* ((entry (aref game-vec i))
               (uid (map-elt entry 'uid))
               (list-vec (map-elt entry 'list)))
          (when (vectorp list-vec)
            (push (cons uid (append list-vec nil)) entries)))))
    (nreverse entries)))

(defun hoyogacha--pool-name (game type)
  "返回 GAME 中卡池类型 TYPE 的显示名称。"
  (or (cdr (assoc (format "%s" type)
                  (map-elt (hoyogacha--game-config game) :gacha-type-names)))
      (format "%s" type)))

(defun hoyogacha--render-buffer (game data &optional loading)
  "将 GAME 与 DATA 渲染到 *抽卡统计* buffer。
LOADING 非 nil 时在标题下插入后台拉取提示。
只更新 buffer 内容，不切换窗口。"
  (let ((buf (get-buffer-create "*抽卡统计*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "📊 %s 抽卡数据分析报告\n\n"
                      (hoyogacha--game-display-name game)))
      (when loading
        (insert "正在后台拉取新记录，完成后将自动刷新……\n"))
      (let ((uid-entries (hoyogacha--records-for-game data game)))
        (if (null uid-entries)
            (insert "没有找到该游戏的数据。\n")
          (dolist (entry uid-entries)
            (let* ((uid (car entry))
                   (records (cdr entry))
                   (const-map-by-uid (hoyogacha--compute-constellation-map records game)))
              (insert (format "\nUID: %s\n" (hoyogacha--uid-alias uid)))
              (dolist (group (hoyogacha--group-records-by-gacha-type records))
                (let* ((type (car group))
                       (pool-name (hoyogacha--pool-name game type)))
                  (hoyogacha--insert-pool-analysis
                   game pool-name (cdr group) const-map-by-uid)))))))
      (goto-char (point-min))
      (read-only-mode 1))
    buf))

;;;###autoload
(defun hoyogacha-show ()
  "导入并合并抽卡数据，选择游戏后显示统计 buffer。

先立即展示本地已有数据，随后在后台异步拉取新记录（不阻塞 Emacs），
拉取完成后自动保存并刷新 buffer。可用 `hoyogacha-cancel' 中止拉取。"
  (interactive)
  (when hoyogacha--fetching-p
    (user-error "已有拉取任务正在进行，请等待其完成或执行 `hoyogacha-cancel'"))
  (let ((data (hoyogacha-merge-data)))
    (unless data (user-error "没有可用的抽卡数据"))
    (let* ((game (hoyogacha--choose-game))
           (local-data data))
      (cond
       ;; 绝区零未配置安装目录：跳过拉取，直接展示本地数据
       ((and (eq game 'zzz) (null hoyogacha-zzz-install-dir))
        (message "未设置 hoyogacha-zzz-install-dir，跳过绝区零新记录拉取")
        (hoyogacha--save-merged-data)
        (hoyogacha--render-buffer game local-data nil)
        (switch-to-buffer (get-buffer-create "*抽卡统计*")))
       (t
        ;; 先用本地数据立即渲染，避免用户等待
        (hoyogacha--render-buffer game local-data t)
        (switch-to-buffer (get-buffer-create "*抽卡统计*"))
        ;; 后台异步拉取新记录；失败不阻塞分析
        (setq hoyogacha--fetching-p t)
        (condition-case err
            (hoyogacha-get-warp-url-async
             game
             (lambda (url)
               (message "[%s] 获取到抽卡链接，开始拉取新记录..." (symbol-name game))
               (hoyogacha--fetch-and-merge-gacha-data-async
                local-data game url
                (lambda (new-data inserted)
                  (setq hoyogacha--fetching-p nil)
                  (condition-case err
                      (progn
                        (setq hoyogacha-merged-data new-data)
                        (hoyogacha--save-merged-data)
                        (hoyogacha--render-buffer game new-data nil)
                        (message "[%s] 拉取完成，新增 %d 条记录"
                                 (symbol-name game) inserted))
                    (error
                     (message "[%s] 保存或刷新数据时出错：%s"
                              (symbol-name game)
                              (error-message-string err)))))))
             (lambda (msg)
               (setq hoyogacha--fetching-p nil)
               (message "[%s] 获取抽卡链接失败：%s" (symbol-name game) msg)
               (hoyogacha--render-buffer game local-data nil)))
          (user-error
           (setq hoyogacha--fetching-p nil)
           (message "[%s] 获取抽卡链接失败：%s"
                    (symbol-name game) (error-message-string err)))
          (error
           (setq hoyogacha--fetching-p nil)
           (message "[%s] 获取抽卡记录过程中出错：%s"
                    (symbol-name game) (error-message-string err))
           (message "将使用本地已有数据进行分析。"))))))))

(defun hoyogacha-cancel ()
  "取消正在进行的后台拉取任务。
已拉取的记录仍会合并保存并刷新统计 buffer。"
  (interactive)
  (if (not hoyogacha--fetching-p)
      (message "[hoyogacha] 当前没有进行中的拉取任务")
    (setq hoyogacha--fetch-canceled-p t)
    (when (and hoyogacha--fetch-process
               (process-live-p hoyogacha--fetch-process))
      (kill-process hoyogacha--fetch-process))
    (message "[hoyogacha] 已请求取消后台拉取")))

(provide 'hoyogacha)
;;; hoyogacha.el ends here

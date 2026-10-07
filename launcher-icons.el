;;; launcher-icons.el --- Native macOS app icons for launcher -*- lexical-binding: t; -*-

;;; Commentary:

;; Icons of installed applications, as macOS draws them, for launcher's
;; completion and for any other view of an app: `launcher--icon' returns
;; an image of an app at a size, from a persistent cache, without waiting.
;;
;; The packaged worker assets/launcher-icons.js asks AppKit for each app's
;; icon (NSWorkspace iconForFile:) through macOS's built-in osascript, and
;; draws it into 64, 256 and 512 pixel PNGs.  It runs in the background,
;; one batch of apps at a time.  The cache keeps one generation of icons
;; per app, under a fingerprint of the app's metadata: its bundle, version,
;; icon file and asset catalog.  Icons are checked against the app when a
;; launcher starts, at most every `launcher-icon-check-interval' seconds,
;; and on every index refresh; a changed fingerprint replaces all sizes.
;; The fingerprint is a hint, not a guarantee: a custom Finder icon, or a
;; change that keeps the files' times and sizes, goes unnoticed.  Then
;; `launcher-clear-icon-cache' starts over.
;;
;; Loading this file starts nothing: icons are made when a graphical
;; launcher asks for them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defcustom launcher-show-icons t
  "Non-nil means show application icons in the launcher.
Icons need macOS and a graphical frame that displays PNG images;
elsewhere the launcher shows text only, as when this is nil."
  :type 'boolean
  :group 'launcher)

(defcustom launcher-icon-size 20
  "Size of application icons in the launcher's list, in logical pixels.
On a Retina display, an icon has twice as many device pixels."
  :type 'natnum
  :group 'launcher)

(defcustom launcher-icon-cache-directory
  (locate-user-emacs-file "cache/launcher/icons/")
  "Directory in which the launcher keeps application icons.
The launcher owns what it creates under this directory's \"v1\"
subdirectory, and deletes nothing else."
  :type 'directory
  :group 'launcher)

(defcustom launcher-icon-check-interval 300
  "Seconds before the launcher checks again whether an app's icon changed.
The launcher checks when it starts, not on a timer.  A refresh of the
app index, as with \\[universal-argument] \\[launcher], checks all apps at once."
  :type 'number
  :group 'launcher)

(defvar launcher-icon-updated-hook nil
  "Functions called with an app's path when its icons change.
Each is called with the normalized path after a new icon for the app
becomes available, or after its icons become obsolete, when
`launcher--icon' returns another image, or nil, for it.  A function
should check that its own view is still live and shows that app before
changing anything, and must neither select nor display a window.
Errors are logged and do not stop the other functions.")

(defconst launcher-icons--buckets '(64 256 512)
  "Sizes in pixels of the PNGs the cache keeps of each icon, ascending.")

(defconst launcher-icons--format "v1"
  "Version of the cache's layout and of how its icons are drawn.
Change it when either changes: the cache never reuses another version.")

(defconst launcher-icons--protocol 1
  "Version of the manifest the worker reads.")

(defconst launcher-icons--batch-limit 32
  "Most apps a worker handles at once.")

(defconst launcher-icons--timeout 30
  "Seconds before an unfinished worker is stopped.")

(defconst launcher-icons--max-record 65536
  "Longest line of worker output accepted, in characters.")

(defconst launcher-icons--stale-age (* 24 60 60)
  "Age in seconds after which another process's leftovers may be deleted.
A worker job, or a generation no metadata refers to, is that old only
if its process died or lost a race long ago.")

(defconst launcher-icons--osascript "/usr/bin/osascript"
  "Program that runs the worker.")

(defconst launcher-icons--script
  (expand-file-name "assets/launcher-icons.js"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "The packaged worker, next to this library.")

(defvar launcher--apps)

;;; State

(cl-defstruct (launcher-icons--record
               (:constructor launcher-icons--record-make)
               (:copier nil))
  "What this process knows of one app's icons.
FINGERPRINT, GENERATION and RASTERS describe the current generation,
whose PNGs of the sizes in RASTERS are published.  TOKEN changes
whenever the generation does, or the record is dropped, so that work
started for an older one is discarded.  CHECKED is when the app was
last checked successfully.  QUEUED and RUNNING list the work waiting
and in a worker, each `check' or a size.  WANTED lists larger sizes
asked for, made again for a new generation.  FAILURES is an alist of
work that failed, each with the time it may be tried again.  IMAGES
is an alist of image specifications by (SIZE DISPLAY-SIZE SCALE)."
  path key fingerprint generation rasters (token 0) checked
  queued running wanted failures images)

(defvar launcher-icons--records (make-hash-table :test #'equal)
  "Records of apps, by normalized path.")

(defvar launcher-icons--epoch 0
  "Count of cache resets.  Work started before the last one is discarded.")

(defvar launcher-icons--urgent nil
  "Records with queued work asked for by a view, oldest first.")

(defvar launcher-icons--queue nil
  "Records with queued work to warm the cache, oldest first.")

(defvar launcher-icons--job nil
  "The running worker job, or nil.")

(defvar launcher-icons--kick-timer nil
  "Timer that starts the next worker job, or nil.")

(defvar launcher-icons--swept nil
  "Non-nil once this process deleted stale worker jobs.")

(defvar launcher-icons--available 'unknown
  "Whether this Emacs can make icons, or `unknown' until first asked.")

(cl-defstruct (launcher-icons--job
               (:constructor launcher-icons--job-make)
               (:copier nil))
  "A worker run.  REQUESTS is an alist of (ID RECORD TOKEN SIZES OUTPUTS),
OUTPUTS an alist of (SIZE . FILE) in DIRECTORY.  EPOCH is
`launcher-icons--epoch' when it started."
  id directory process stderr timer epoch requests answered (partial "") done)

;;; Small helpers

(defun launcher-icons--now ()
  "Return the current time in seconds."
  (float-time))

(defun launcher-icons--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS in the buffer \" *launcher-icons*\".
Keep only the most recent lines, without displaying anything."
  (with-current-buffer (get-buffer-create " *launcher-icons*")
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format-time-string "%F %T ") (apply #'format format-string args) "\n")
      (when (> (count-lines (point-min) (point-max)) 500)
        (goto-char (point-min))
        (forward-line 100)
        (delete-region (point-min) (point))))))

(defun launcher-icons--normalize (path)
  "Return PATH as an absolute directory name without a trailing slash."
  (directory-file-name (expand-file-name path)))

(defun launcher-icons--hash (string)
  "Return the SHA-256 of STRING, encoded as UTF-8, in hexadecimal."
  (secure-hash 'sha256 (encode-coding-string string 'utf-8-unix)))

(defun launcher-icons--root ()
  "Return the directory of this cache format's files."
  (file-name-as-directory
   (expand-file-name launcher-icons--format
                     (expand-file-name launcher-icon-cache-directory))))

(defun launcher-icons--app-directory (record)
  "Return the directory of RECORD's files."
  (expand-file-name (launcher-icons--record-key record) (launcher-icons--root)))

(defun launcher-icons--png (record size)
  "Return the PNG of RECORD's current generation at SIZE."
  (expand-file-name (format "%s/%d.png" (launcher-icons--record-generation record) size)
                    (launcher-icons--app-directory record)))

(defun launcher-icons--unique-id ()
  "Return a new name, unique among processes, for a job or generation."
  (format "%s-%d-%06x" (format-time-string "%Y%m%dT%H%M%S") (emacs-pid)
          (random #x1000000)))

(defconst launcher-icons--id-regexp
  "\\`[0-9]\\{8\\}T[0-9]\\{6\\}-[0-9]+-[0-9a-f]\\{6\\}\\'"
  "Matches names `launcher-icons--unique-id' returns.")

(defun launcher-icons--available-p ()
  "Return non-nil if this Emacs can make app icons.
That needs macOS, the worker, native JSON and PNG support."
  (when (eq launcher-icons--available 'unknown)
    (setq launcher-icons--available
          (and (eq system-type 'darwin)
               (file-executable-p launcher-icons--osascript)
               (file-readable-p launcher-icons--script)
               (fboundp 'json-parse-string)
               (or (not (fboundp 'json-available-p)) (json-available-p))
               (image-type-available-p 'png))))
  launcher-icons--available)

(defun launcher-icons--display-p (frame)
  "Return non-nil if FRAME displays images."
  (display-images-p frame))

(defun launcher-icons-enabled-p (&optional frame)
  "Return non-nil if the launcher shows icons in FRAME.
FRAME defaults to the selected frame."
  (and launcher-show-icons
       (launcher-icons--available-p)
       (launcher-icons--display-p frame)))

(defun launcher-icons--scale (frame)
  "Return FRAME's backing scale factor, or 2 if unknown.
Oversampling a 1x display costs little; undersampling a Retina one blurs."
  (let ((scale (and (fboundp 'frame-scale-factor)
                    (ignore-errors (frame-scale-factor frame)))))
    (if (and (numberp scale) (> scale 0)) scale 2)))

(defun launcher-icons--bucket (display-size scale)
  "Return the smallest raster size for DISPLAY-SIZE pixels at SCALE.
That is the smallest of `launcher-icons--buckets' with at least
DISPLAY-SIZE times SCALE pixels, or the largest, which views larger
than that enlarge."
  (let ((pixels (* display-size scale)))
    (or (seq-find (lambda (bucket) (>= bucket pixels)) launcher-icons--buckets)
        (car (last launcher-icons--buckets)))))

(defun launcher-icons--notify (path)
  "Run `launcher-icon-updated-hook' with PATH, logging errors."
  (run-hook-wrapped 'launcher-icon-updated-hook
                    (lambda (function)
                      (condition-case err
                          (funcall function path)
                        (error (launcher-icons--log "Hook %S failed for %s: %s"
                                                    function path
                                                    (error-message-string err))))
                      nil)))

;;; Records and the disk cache

(defun launcher-icons--read-json (file)
  "Return FILE's JSON object as an alist, or nil if it is not one.
Never evaluate anything: the file is only data."
  (condition-case nil
      (when (< (or (file-attribute-size (file-attributes file)) 0) 65536)
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix))
            (insert-file-contents file))
          (let ((object (json-parse-buffer :object-type 'alist :array-type 'list
                                           :null-object nil :false-object nil)))
            (and (consp object) (consp (car object)) object))))
    (error nil)))

(defun launcher-icons--valid-metadata (metadata path)
  "Return METADATA, as read from a metadata.json, if it describes PATH.
Return nil for metadata of another path, version or form."
  (let-alist metadata
    (and (equal .format launcher-icons--format)
         (equal .path path)
         (stringp .fingerprint) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" .fingerprint)
         (stringp .generation) (string-match-p launcher-icons--id-regexp .generation)
         (proper-list-p .rasters)
         (seq-every-p (lambda (size) (memq size launcher-icons--buckets)) .rasters)
         metadata)))

(defun launcher-icons--load (path)
  "Return a new record of PATH, with its published icons, if any."
  (let* ((record (launcher-icons--record-make
                  :path path :key (launcher-icons--hash path)))
         (metadata (launcher-icons--valid-metadata
                    (launcher-icons--read-json
                     (expand-file-name "metadata.json" (launcher-icons--app-directory record)))
                    path)))
    (when metadata
      (let-alist metadata
        (setf (launcher-icons--record-fingerprint record) .fingerprint
              (launcher-icons--record-generation record) .generation
              (launcher-icons--record-rasters record) (sort (copy-sequence .rasters) #'<))))
    record))

(defun launcher-icons--record (path)
  "Return the record of the app at PATH, loading it if needed."
  (let ((path (launcher-icons--normalize path)))
    (or (gethash path launcher-icons--records)
        (puthash path (launcher-icons--load path) launcher-icons--records))))

(defun launcher-icons--flush (record)
  "Forget RECORD's images, also in Emacs's image cache."
  (dolist (entry (launcher-icons--record-images record))
    (image-flush (cdr entry) t))
  (setf (launcher-icons--record-images record) nil))

(defun launcher-icons--write-metadata (record)
  "Publish RECORD's current generation, replacing its metadata atomically."
  (let* ((directory (launcher-icons--app-directory record))
         (file (expand-file-name "metadata.json" directory))
         (temporary (expand-file-name
                     (format "metadata.json.%s.tmp" (launcher-icons--unique-id))
                     directory)))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file temporary
        (json-insert
         `((format . ,launcher-icons--format)
           (path . ,(launcher-icons--record-path record))
           (fingerprint . ,(launcher-icons--record-fingerprint record))
           (generation . ,(launcher-icons--record-generation record))
           (rasters . ,(vconcat (launcher-icons--record-rasters record)))))))
    (condition-case err
        (rename-file temporary file t)
      (error (ignore-errors (delete-file temporary))
             (signal (car err) (cdr err))))))

(defun launcher-icons--delete-generation (record generation)
  "Delete GENERATION of RECORD's icons, a directory of PNGs only."
  (let ((directory (expand-file-name generation (launcher-icons--app-directory record))))
    (when (and (string-match-p launcher-icons--id-regexp generation)
               (file-directory-p directory)
               (not (file-symlink-p directory)))
      (dolist (file (directory-files directory t "\\`[0-9]+\\.png\\'"))
        (ignore-errors (delete-file file)))
      (ignore-errors (delete-directory directory)))))

(defun launcher-icons--sweep-generations (record)
  "Delete RECORD's stale generations, other than its current one."
  (let ((directory (launcher-icons--app-directory record))
        (current (launcher-icons--record-generation record)))
    (dolist (generation (ignore-errors (directory-files directory nil launcher-icons--id-regexp)))
      (unless (or (equal generation current)
                  (< (launcher-icons--age (expand-file-name generation directory))
                     launcher-icons--stale-age))
        (launcher-icons--delete-generation record generation)))))

(defun launcher-icons--age (file)
  "Return the seconds since FILE was modified, or 0 if unknown."
  (if-let* ((time (file-attribute-modification-time (file-attributes file))))
      (- (launcher-icons--now) (float-time time))
    0))

(defun launcher-icons--invalidate (record &optional fingerprint)
  "Start a new, empty generation of RECORD, with FINGERPRINT if non-nil.
Forget its images and failures together, discard work started for its
previous generation, and run `launcher-icon-updated-hook' if it had
icons.  Delete the previous generation's files."
  (let ((old (launcher-icons--record-generation record))
        (had-icons (launcher-icons--record-rasters record)))
    (launcher-icons--flush record)
    (setf (launcher-icons--record-generation record) (launcher-icons--unique-id)
          (launcher-icons--record-rasters record) nil
          (launcher-icons--record-failures record) nil
          (launcher-icons--record-token record) (1+ (launcher-icons--record-token record)))
    (when fingerprint
      (setf (launcher-icons--record-fingerprint record) fingerprint))
    (when old
      (launcher-icons--delete-generation record old))
    (when had-icons
      (launcher-icons--notify (launcher-icons--record-path record)))))

(defun launcher-icons--retire (record)
  "Forget RECORD, an app that is no longer indexed, and its files."
  (let ((had-icons (launcher-icons--record-rasters record)))
    (launcher-icons--flush record)
    (cl-incf (launcher-icons--record-token record))
    (setf (launcher-icons--record-queued record) nil
          (launcher-icons--record-rasters record) nil)
    (remhash (launcher-icons--record-path record) launcher-icons--records)
    (when had-icons
      (launcher-icons--notify (launcher-icons--record-path record)))))

(defun launcher-icons--publish (record outputs)
  "Move OUTPUTS, an alist of (SIZE . PNG), into RECORD's current generation.
Then publish the generation's metadata.  Return the sizes published."
  (let* ((directory (expand-file-name (launcher-icons--record-generation record)
                                      (launcher-icons--app-directory record)))
         (new (not (file-directory-p directory)))
         published)
    (make-directory directory t)
    (pcase-dolist (`(,size . ,file) outputs)
      (rename-file file (expand-file-name (format "%d.png" size) directory) t)
      (push size published))
    (setf (launcher-icons--record-rasters record)
          (sort (seq-union published (launcher-icons--record-rasters record)) #'<))
    (launcher-icons--write-metadata record)
    (when new
      (launcher-icons--sweep-generations record))
    published))

;;; Images

(defun launcher-icons--image (record size display-size scale)
  "Return an image of RECORD at DISPLAY-SIZE from its SIZE PNG, or nil.
If the PNG is gone, start a new generation instead."
  (let ((key (list size display-size scale)))
    (or (cdr (assoc key (launcher-icons--record-images record)))
        (let ((file (launcher-icons--png record size)))
          (if (not (file-readable-p file))
              (progn (launcher-icons--log "Missing %s; making the icons of %s again"
                                          file (launcher-icons--record-path record))
                     (launcher-icons--invalidate record)
                     (launcher-icons--rewarm record)
                     nil)
            (let ((image (create-image file 'png nil
                                       :width display-size :height display-size
                                       :scale 1 :ascent 'center)))
              (push (cons key image) (launcher-icons--record-images record))
              image))))))

(defun launcher-icons--best (record bucket)
  "Return the published size of RECORD to show for BUCKET, or nil.
That is BUCKET itself, else the smallest larger size, else the largest
smaller one."
  (let ((rasters (launcher-icons--record-rasters record)))
    (cond ((memq bucket rasters) bucket)
          ((seq-find (lambda (size) (> size bucket)) rasters))
          (t (car (last rasters))))))

(defun launcher--icon (app-path display-size &optional frame)
  "Return an image of the app at APP-PATH, DISPLAY-SIZE logical pixels wide.
FRAME, defaulting to the selected frame, determines the pixels needed.
Return nil while no icon is cached yet, or if FRAME shows no icons.
Never wait: queue work to make a missing or better icon in the
background, and run `launcher-icon-updated-hook' when it is done."
  (when (launcher-icons-enabled-p frame)
    (let* ((record (launcher-icons--record app-path))
           (scale (launcher-icons--scale (or frame (selected-frame))))
           (bucket (launcher-icons--bucket display-size scale))
           (urgent (> bucket (car launcher-icons--buckets))))
      (when urgent
        (cl-pushnew bucket (launcher-icons--record-wanted record)))
      (when (launcher-icons--stale-p record)
        (launcher-icons--enqueue record 'check urgent))
      (unless (memq bucket (launcher-icons--record-rasters record))
        (launcher-icons--enqueue record bucket urgent))
      (when-let* ((size (launcher-icons--best record bucket)))
        (launcher-icons--image record size display-size scale)))))

(defun launcher-icons-prefix (app-path &optional frame)
  "Return the completion prefix showing the icon of the app at APP-PATH.
Without APP-PATH, or while the icon is missing, return blank space of
the same size, so that names stay aligned and rows equally tall."
  (let ((image (and app-path (launcher--icon app-path launcher-icon-size frame))))
    (concat (propertize " " 'display
                        (or image `(space :width (,launcher-icon-size)
                                          :height (,launcher-icon-size))))
            " ")))

;;; Scheduling

(defun launcher-icons--stale-p (record)
  "Return non-nil if RECORD is due a check."
  (let ((checked (launcher-icons--record-checked record)))
    (or (null checked)
        (>= (- (launcher-icons--now) checked) launcher-icon-check-interval))))

(defun launcher-icons--failed-p (record what)
  "Return non-nil if WHAT failed for RECORD and may not be retried yet."
  (when-let* ((deadline (alist-get what (launcher-icons--record-failures record))))
    (< (launcher-icons--now) deadline)))

(defun launcher-icons--fail (record what)
  "Note that WHAT failed for RECORD, until the next check interval."
  (setf (alist-get what (launcher-icons--record-failures record))
        (+ (launcher-icons--now) launcher-icon-check-interval)))

(defun launcher-icons--enqueue (record what &optional urgent)
  "Queue WHAT, `check' or a size, for RECORD, unless it is already.
URGENT work, for a view, runs before warming.  Start a worker soon."
  (unless (or (memq what (launcher-icons--record-queued record))
              (memq what (launcher-icons--record-running record))
              (launcher-icons--failed-p record what))
    (push what (launcher-icons--record-queued record))
    (cond (urgent
           (setq launcher-icons--queue (delq record launcher-icons--queue))
           (unless (memq record launcher-icons--urgent)
             (setq launcher-icons--urgent (nconc launcher-icons--urgent (list record)))))
          ((not (or (memq record launcher-icons--urgent) (memq record launcher-icons--queue)))
           (setq launcher-icons--queue (nconc launcher-icons--queue (list record)))))
    (launcher-icons--kick)))

(defun launcher-icons--kick ()
  "Start the next worker job from the event loop, if work waits and none runs."
  (unless (or launcher-icons--job launcher-icons--kick-timer
              (not (or launcher-icons--urgent launcher-icons--queue)))
    (setq launcher-icons--kick-timer (run-at-time 0 nil #'launcher-icons--start))))

(defun launcher-icons--take ()
  "Remove and return the records of the next job, urgent ones first."
  (let (batch)
    (while (and (< (length batch) launcher-icons--batch-limit)
                (or launcher-icons--urgent launcher-icons--queue))
      (let ((record (if launcher-icons--urgent
                        (pop launcher-icons--urgent)
                      (pop launcher-icons--queue))))
        (when (and (launcher-icons--record-queued record)
                   (eq record (gethash (launcher-icons--record-path record)
                                       launcher-icons--records)))
          (push record batch))))
    (nreverse batch)))

(defun launcher-icons-prepare (paths)
  "Queue the checks and list icons due for the apps at PATHS.
Call when a launcher with icons starts.  Load each app's published
icons first; this reads small files and starts no process."
  (unless launcher-icons--swept
    (setq launcher-icons--swept t)
    (launcher-icons--sweep-jobs))
  (let ((bucket (car launcher-icons--buckets)))
    (dolist (path paths)
      (let ((record (launcher-icons--record path)))
        (when (launcher-icons--stale-p record)
          (launcher-icons--enqueue record 'check))
        (unless (memq bucket (launcher-icons--record-rasters record))
          (launcher-icons--enqueue record bucket))))))

;;; Worker jobs

(defun launcher-icons--jobs-directory ()
  "Return the directory of worker jobs."
  (expand-file-name "jobs" (launcher-icons--root)))

(defun launcher-icons--sweep-jobs ()
  "Delete worker jobs that are too old to be live in any process."
  (dolist (directory (ignore-errors
                       (directory-files (launcher-icons--jobs-directory) t
                                        launcher-icons--id-regexp)))
    (when (> (launcher-icons--age directory) launcher-icons--stale-age)
      (launcher-icons--delete-job-directory directory))))

(defun launcher-icons--delete-job-directory (directory)
  "Delete DIRECTORY, a worker job's, with the files in it.
A job's directory holds only files: its manifest, PNGs, and what an
interrupted atomic write leaves."
  (when (and (string-match-p launcher-icons--id-regexp (file-name-nondirectory directory))
             (file-equal-p (file-name-directory (directory-file-name directory))
                           (launcher-icons--jobs-directory))
             (file-directory-p directory)
             (not (file-symlink-p directory)))
    (dolist (file (directory-files directory t directory-files-no-dot-files-regexp))
      (unless (and (file-directory-p file) (not (file-symlink-p file)))
        (ignore-errors (delete-file file))))
    (ignore-errors (delete-directory directory))))

(defun launcher-icons--start ()
  "Start a worker job with the next records, if there are any."
  (setq launcher-icons--kick-timer nil)
  (unless launcher-icons--job
    (when-let* ((batch (launcher-icons--take)))
      (condition-case err
          (launcher-icons--spawn batch)
        (error
         (launcher-icons--log "Cannot start the icon worker: %s"
                              (error-message-string err)))))))

(defun launcher-icons--insert-manifest (requests)
  "Insert the worker's manifest of REQUESTS, as JSON."
  (json-insert
   `((version . ,launcher-icons--protocol)
     (requests
      . ,(vconcat
          (mapcar (pcase-lambda (`(,request ,record ,_ ,sizes ,outputs))
                    `((id . ,request)
                      (app . ,(launcher-icons--record-path record))
                      (sizes . ,(vconcat sizes))
                      (outputs . ,(mapcar (pcase-lambda (`(,size . ,file))
                                            (cons (intern (number-to-string size)) file))
                                          outputs))))
                  requests))))))

(defun launcher-icons--spawn (batch)
  "Start the worker for the records of BATCH, moving their queued work.
If it cannot start, their work fails until the next check interval."
  (let* ((id (launcher-icons--unique-id))
         (directory (file-name-as-directory
                     (expand-file-name id (launcher-icons--jobs-directory))))
         (manifest (expand-file-name "manifest.json" directory))
         (number 0)
         (requests
          (mapcar (lambda (record)
                    (let* ((queued (launcher-icons--record-queued record))
                           (sizes (sort (seq-filter #'integerp queued) #'<))
                           (request (number-to-string (cl-incf number))))
                      (setf (launcher-icons--record-running record) queued
                            (launcher-icons--record-queued record) nil)
                      (list request record (launcher-icons--record-token record) sizes
                            (mapcar (lambda (size)
                                      (cons size (expand-file-name
                                                  (format "%s-%d.png" request size)
                                                  directory)))
                                    sizes))))
                  batch))
         (job (launcher-icons--job-make :id id :directory directory
                                        :epoch launcher-icons--epoch
                                        :requests requests)))
    (setq launcher-icons--job job)
    (condition-case err
        (let ((default-directory (progn (make-directory directory t) directory))
              (stderr (generate-new-buffer " *launcher-icons-stderr*")))
          (setf (launcher-icons--job-stderr job) stderr)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file manifest
              (launcher-icons--insert-manifest requests)))
          (setf (launcher-icons--job-process job)
                (make-process
                 :name "launcher-icons"
                 :command (list launcher-icons--osascript "-l" "JavaScript"
                                launcher-icons--script manifest)
                 :connection-type 'pipe :coding 'utf-8-unix :noquery t
                 :stderr stderr
                 :filter (lambda (_process output) (launcher-icons--receive job output))
                 :sentinel (lambda (process _event)
                             (unless (process-live-p process)
                               (launcher-icons--finish job)))))
          (when-let* ((stderr-process (get-buffer-process stderr)))
            (set-process-query-on-exit-flag stderr-process nil))
          (setf (launcher-icons--job-timer job)
                (run-at-time launcher-icons--timeout nil
                             (lambda ()
                               (launcher-icons--log "Icon worker timed out after %ds"
                                                    launcher-icons--timeout)
                               (launcher-icons--finish job)))))
      (error (launcher-icons--finish job)
             (signal (car err) (cdr err))))))

(defun launcher-icons--receive (job output)
  "Handle OUTPUT of JOB's worker, a part of its JSON lines."
  (unless (launcher-icons--job-done job)
    (let ((text (concat (launcher-icons--job-partial job) output))
          (start 0))
      (while-let ((end (string-search "\n" text start)))
        (launcher-icons--handle-line job (substring text start end))
        (setq start (1+ end)))
      (setq text (substring text start))
      (if (> (length text) launcher-icons--max-record)
          (progn (launcher-icons--log "Icon worker output is too long; stopping it")
                 (launcher-icons--finish job))
        (setf (launcher-icons--job-partial job) text)))))

(defun launcher-icons--parse (line)
  "Return LINE of worker output as an alist, or nil if it is not one."
  (when (<= (length line) launcher-icons--max-record)
    (condition-case nil
        (let ((object (json-parse-string line :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil)))
          (and (consp object) (consp (car object)) object))
      (error nil))))

(defun launcher-icons--valid-fingerprint (fingerprint)
  "Return FINGERPRINT, the worker's, if it is a list of lists of strings."
  (and (consp fingerprint) (proper-list-p fingerprint) (< (length fingerprint) 32)
       (seq-every-p (lambda (part)
                      (and (consp part) (proper-list-p part) (< (length part) 16)
                           (seq-every-p #'stringp part)))
                    fingerprint)
       fingerprint))

(defun launcher-icons--fingerprint-hash (fingerprint)
  "Return the hash identifying FINGERPRINT, the worker's."
  (let ((json (json-serialize (vconcat (mapcar #'vconcat fingerprint)))))
    (secure-hash 'sha256 (if (multibyte-string-p json)
                             (encode-coding-string json 'utf-8-unix)
                           json))))

(defun launcher-icons--png-p (file)
  "Return non-nil if FILE is a regular file that starts like a PNG."
  (and (file-regular-p file)
       (not (file-symlink-p file))
       (condition-case nil
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally file nil 0 8)
             (equal (buffer-string) "\211PNG\r\n\032\n"))
         (error nil))))

(defun launcher-icons--handle-line (job line)
  "Handle LINE, one record of JOB's worker output.
Ignore records that are malformed, or not for a request still due."
  (let* ((data (launcher-icons--parse line))
         (id (alist-get 'id data))
         (request (and (stringp id) (assoc id (launcher-icons--job-requests job)))))
    (if (or (not request) (member id (launcher-icons--job-answered job)))
        (launcher-icons--log "Ignoring unexpected icon worker output: %s"
                             (truncate-string-to-width line 200))
      (push id (launcher-icons--job-answered job))
      (pcase-let ((`(,_ ,record ,_ ,sizes ,_) request))
        (setf (launcher-icons--record-running record) nil)
        (condition-case err
            (launcher-icons--accept job request data)
          (error (launcher-icons--log "Failed to store icons of %s: %s"
                                      (launcher-icons--record-path record)
                                      (error-message-string err))
                 (when (eq record (gethash (launcher-icons--record-path record)
                                           launcher-icons--records))
                   (dolist (size sizes)
                     (launcher-icons--fail record size)))))))))

(defun launcher-icons--current-p (job record token)
  "Return non-nil if work of JOB for RECORD at TOKEN is still wanted."
  (and (= (launcher-icons--job-epoch job) launcher-icons--epoch)
       (= token (launcher-icons--record-token record))
       (eq record (gethash (launcher-icons--record-path record) launcher-icons--records))))

(defun launcher-icons--accept (job request data)
  "Store DATA, the worker's result for REQUEST of JOB, if still current."
  (pcase-let ((`(,_ ,record ,token ,sizes ,outputs) request))
    (when (launcher-icons--current-p job record token)
      (let ((error (alist-get 'error data))
            (fingerprint (launcher-icons--valid-fingerprint (alist-get 'fingerprint data)))
            (written (alist-get 'written data)))
        (cond
         (error
          (let ((code (and (consp error) (alist-get 'code error))))
            (launcher-icons--log "Icon worker: %s: %s" (launcher-icons--record-path record)
                                 (or (and (consp error) (alist-get 'message error)) code))
            (if (equal code "changed")
                ;; Check again, which makes a new generation if needed.
                (launcher-icons--enqueue record 'check)
              (dolist (what (cons 'check sizes))
                (launcher-icons--fail record what)))))
         ((not (and fingerprint (proper-list-p written)
                    (seq-every-p (lambda (size) (memq size sizes)) written)
                    (seq-every-p (lambda (size)
                                   (launcher-icons--png-p (alist-get size outputs)))
                                 written)))
          (launcher-icons--log "Ignoring an invalid icon worker result for %s"
                               (launcher-icons--record-path record))
          (dolist (what (cons 'check sizes))
            (launcher-icons--fail record what)))
         (t
          (launcher-icons--store record (launcher-icons--fingerprint-hash fingerprint)
                                 (mapcar (lambda (size) (assq size outputs)) written)
                                 sizes)))))))

(defun launcher-icons--store (record fingerprint outputs sizes)
  "Store a successful check of RECORD with FINGERPRINT and OUTPUTS.
OUTPUTS is an alist of (SIZE . PNG) of the SIZES requested."
  (setf (launcher-icons--record-checked record) (launcher-icons--now)
        (alist-get 'check (launcher-icons--record-failures record) nil t) nil)
  (let ((changed (not (equal fingerprint (launcher-icons--record-fingerprint record)))))
    (when changed
      (launcher-icons--invalidate record fingerprint))
    (dolist (size sizes)
      (unless (assq size outputs)
        (launcher-icons--fail record size)))
    (when (and outputs (launcher-icons--publish record outputs))
      (launcher-icons--notify (launcher-icons--record-path record)))
    (when changed
      (launcher-icons--rewarm record))))

(defun launcher-icons--rewarm (record)
  "Queue the sizes RECORD's new generation needs: list icons, and wanted ones."
  (let ((list-size (car launcher-icons--buckets)))
    (dolist (size (cons list-size (launcher-icons--record-wanted record)))
      (unless (memq size (launcher-icons--record-rasters record))
        (launcher-icons--enqueue record size (/= size list-size))))))

(defun launcher-icons--finish (job)
  "End JOB: stop its worker and clean up after it, then start the next.
Requests it did not answer fail until the next check interval."
  (unless (launcher-icons--job-done job)
    (setf (launcher-icons--job-done job) t)
    (when-let* ((timer (launcher-icons--job-timer job)))
      (cancel-timer timer))
    (let ((process (launcher-icons--job-process job))
          (stderr (launcher-icons--job-stderr job)))
      (when (process-live-p process)
        (delete-process process))
      (when (and process (not (memq (process-status process) '(run stop)))
                 (not (eql (process-exit-status process) 0)))
        (launcher-icons--log "Icon worker exited with %s: %s"
                             (process-exit-status process)
                             (if (buffer-live-p stderr)
                                 (with-current-buffer stderr
                                   (truncate-string-to-width (buffer-string) 1000))
                               "")))
      (when (buffer-live-p stderr)
        (when-let* ((stderr-process (get-buffer-process stderr)))
          (delete-process stderr-process))
        (kill-buffer stderr)))
    (pcase-dolist (`(,id ,record ,token ,sizes ,_) (launcher-icons--job-requests job))
      (setf (launcher-icons--record-running record) nil)
      (when (and (not (member id (launcher-icons--job-answered job)))
                 (launcher-icons--current-p job record token))
        (dolist (what (cons 'check sizes))
          (launcher-icons--fail record what))))
    (launcher-icons--delete-job-directory
     (directory-file-name (launcher-icons--job-directory job)))
    (when (eq launcher-icons--job job)
      (setq launcher-icons--job nil)
      (launcher-icons--kick))))

;;; Index refreshes and resets

(defun launcher-icons--app-directories ()
  "Return the app directories of the cache, named by their hashes."
  (ignore-errors
    (directory-files (launcher-icons--root) t "\\`[0-9a-f]\\{64\\}\\'")))

(defun launcher-icons--delete-app-directory (directory)
  "Delete DIRECTORY, one app's, with its metadata and generations."
  (when (and (file-directory-p directory) (not (file-symlink-p directory)))
    (let ((record (launcher-icons--record-make :key (file-name-nondirectory directory))))
      (dolist (generation (directory-files directory nil launcher-icons--id-regexp))
        (launcher-icons--delete-generation record generation)))
    (dolist (file (directory-files directory t "\\`metadata\\.json\\(\\..*\\.tmp\\)?\\'"))
      (ignore-errors (delete-file file)))
    (ignore-errors (delete-directory directory))))

(defun launcher-icons-index-refreshed (paths)
  "Note that the app index was rebuilt, and now holds PATHS.
Check all of them again, at once if icons show in the selected frame,
otherwise when a launcher with icons next starts.  If PATHS is
nonempty, forget apps that are no longer indexed, with their files."
  (let ((indexed (make-hash-table :test #'equal)))
    (dolist (path paths)
      (puthash (launcher-icons--normalize path) t indexed))
    (when paths
      (dolist (record (hash-table-values launcher-icons--records))
        (unless (gethash (launcher-icons--record-path record) indexed)
          (launcher-icons--retire record)))
      (dolist (directory (launcher-icons--app-directories))
        (let ((path (alist-get 'path (launcher-icons--read-json
                                      (expand-file-name "metadata.json" directory)))))
          (unless (if (stringp path)
                      (or (gethash path indexed)
                          (not (equal (launcher-icons--hash path)
                                      (file-name-nondirectory directory))))
                    (< (launcher-icons--age directory) launcher-icons--stale-age))
            (launcher-icons--delete-app-directory directory)))))
    (maphash (lambda (_path record) (setf (launcher-icons--record-checked record) nil))
             launcher-icons--records)
    (when (and paths (launcher-icons-enabled-p))
      (launcher-icons-prepare paths))))

;;;###autoload
(defun launcher-clear-icon-cache ()
  "Delete the launcher's app icons, and make them again.
Discard work in progress, even for apps that seem unchanged.  Delete
only files the launcher made.  Unlike a refresh of the app index, which
checks icons and keeps those of unchanged apps, this starts over."
  (interactive)
  (cl-incf launcher-icons--epoch)
  (when launcher-icons--job
    (launcher-icons--finish launcher-icons--job))
  (maphash (lambda (_path record)
             (launcher-icons--flush record)
             (cl-incf (launcher-icons--record-token record)))
           launcher-icons--records)
  (let ((paths (hash-table-keys launcher-icons--records)))
    (clrhash launcher-icons--records)
    (setq launcher-icons--urgent nil
          launcher-icons--queue nil)
    (mapc #'launcher-icons--delete-app-directory (launcher-icons--app-directories))
    (mapc #'launcher-icons--notify paths))
  (let ((apps (and (boundp 'launcher--apps) launcher--apps)))
    (when (and apps (launcher-icons-enabled-p))
      (launcher-icons-prepare (mapcar #'cdr apps))))
  (message "Cleared the launcher's icon cache"))

(provide 'launcher-icons)
;;; launcher-icons.el ends here

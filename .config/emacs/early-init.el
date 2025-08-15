;; Inspired by minimal-emacs.d
;; https://github.com/jamescherti/minimal-emacs.d/blob/main/early-init.el
;; Not used for now

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Native compilation and Byte compilation
(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq native-comp-deferred-compilation t
          native-comp-jit-compilation t
          package-native-compile t)
    ;; Log but don't ;; send to message
    (setq native-comp-async-report-warnings-errors 'silent)
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb

;; Startup buffer
;; Reduce *Message* noise at startup. An empty scratch buffer (or the
;; dashboard) is more than enough, and faster to display.
(setq inhibit-startup-screen t)
(setq initial-buffer-choice t
      inhibit-startup-buffer-menu t)

;; ;; Shave seconds off startup time by starting the scratch buffer in
;; ;; `fundamental-mode'
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

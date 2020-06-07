;;;-------------------------------------------------------------------
;;; @doc pollution_lfe public API
;;; @end
;;;-------------------------------------------------------------------

(defmodule pollution_lfe_app
  (behaviour application)
  (export (start 2) (stop 2)))

(defun start (_state _startArgs)
       (pollution_lfe_sup:start_link))

(defun stop (_state) 'ok)

;; internal functions

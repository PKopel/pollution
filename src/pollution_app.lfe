;;;-------------------------------------------------------------------
;;; @doc pollution_lfe public API
;;; @end
;;;-------------------------------------------------------------------

(defmodule pollution_app
  (behaviour application)
  (export (start 2) (stop 1)))

(defun start (_state _startArgs)
       (pollution_sup:start_link))

(defun stop (_state) 'ok)

;; internal functions
(defun stop () (pollution_gen_server:stop))

(defun add_station (name coords) (pollution_gen_server:add_station name coords))

(defun add_value (station date type value) (pollution_gen_server:add_value station date type value))

(defun remove_value (station date type) (pollution_gen_server:remove_value station date type))

(defun get_one_value (station date type) (pollution_gen_server:get_one_value station date type))

(defun get_daily_mean (date type) (pollution_gen_server:get_daily_mean date type))

(defun get_station_mean (station type) (pollution_gen_server:get_station_mean station type))

(defun get_min_type_mean (type) (pollution_gen_server:get_min_type_mean type))

(defun get_two_closest_stations () (pollution_gen_server:get_two_closest_stations))
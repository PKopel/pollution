
(defmodule pollution_gen_server
  (behaviour gen_server)
  (export (start_link 0)
          (stop 0)
          (init 1)
          (terminate 2)
          (handle_call 3)
          (handle_info 2)
          (add_station 2)
          (add_value 4)
          (remove_value 3)
          (get_one_value 3)
          (get_station_mean 2)
          (get_daily_mean 2)
          (get_min_type_mean 1)
          (get_two_closest_stations 0)))

(defun server_module () (MODULE))

(defun start_link () (gen_server:start_link `#(local pollution_gen_server) (server_module) [] []))

;; internal functions
(defun init (([]) (tuple 'ok (pollution:create_monitor))))

(defun serve (monitor function)
  (case (funcall function monitor)
    (result (when (is_list result)) `#(reply ok result))
    (result `#(reply result monitor))))

(defun terminate (normal _monitor) (io:format "Stopped monitor~n"))

;; interface
(defun stop () (gen_server:call (MODULE) #(stop)))

(defun add_station (name coords) (gen_server:call (MODULE) #(add_station name coords)))

(defun add_value (station date type value) (gen_server:call (MODULE) #(add_value station date type value)))

(defun remove_value (station date type) (gen_server:call (MODULE) #(remove_value station date type)))

(defun get_one_value (station date type) (gen_server:call (MODULE) #(get_value station date type)))

(defun get_daily_mean (type date) (gen_server:call (MODULE) #(daily_mean date type)))

(defun get_station_mean (station type) (gen_server:call (MODULE) #(station_mean station type)))

(defun get_min_type_mean (type) (gen_server:call (MODULE) #(min_type_mean type)))

(defun get_two_closest_stations () (gen_server:call (MODULE) #(closest_stations)))

;; message handling
(defun handle_call
  (((tuple 'add_station name coords) _from monitor)
   (serve monitor (lambda (m) (pollution:add_station m name coords))))
  (((tuple 'add_value station date type value) _from monitor)
   (serve monitor (lambda (m) (pollution:add_value m station date type value))))
  (((tuple 'remove_value station date type) _from monitor)
   (serve monitor (lambda (m) (pollution:remove_value m station date type))))
  (((tuple 'get_value station date type) _from monitor)
   (serve monitor (lambda (m) (pollution:get_one_value m station date type))))
  (((tuple 'daily_mean date type) _from monitor)
   (serve monitor (lambda (m) (pollution:get_daily_mean m date type))))
  (((tuple 'station_mean station type) _from monitor)
   (serve monitor (lambda (m) (pollution:get_station_mean m station type))))
  (((tuple 'min_type_mean type) _from monitor)
   (serve monitor (lambda (m) (pollution:get_min_type_mean m type))))
  (((tuple 'closest_stations) _from monitor)
   (serve monitor (lambda (m) (pollution:get_two_closest_stations m))))
  (((tuple 'stop) _from monitor)
   `#(stop normal ok ,monitor)))

(defun handle_info (_info monitor) #('noreply monitor))
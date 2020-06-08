

(defmodule pollution
  (export (create_monitor 0)
          (add_station 3)
          (add_value 5)
          (remove_value 4)
          (get_one_value 4)
          (get_station_mean 3)
          (get_daily_mean 3)
          (get_min_type_mean 2)
          (get_two_closest_stations 1)))

(defrecord station name coords (measurements '[]))

;; Comparing stations
(defun check_station
;; by coordinates
  (((match-station coords station_coords) (tuple x y))
   (=:= station_coords #(x y)))
;; by name
  (((match-station name station_name) name)
   (=:= station_name name)))


;; Searching station in monitor:
(defun search_monitor
  (((cons current tail) searched func)
    (if (check_station current searched)
      (funcall func (cons current tail))
      (handle_result (search_monitor tail searched func) current)))
  ((monitor _searched func) (funcall func monitor)))

;; handling search result
(defun handle_result
;; if list, assume that it's a new monitor
  (((cons head tail) current) (cons current (cons head tail)))
;; otherwise a return value
  ((result _current) result))


;; Create new empty monitor
(defun create_monitor () '[])

;; Add new station: check if station is already in monitor,
;; if not put new one at the beginning.
(defun add_station (monitor name coords)
  (flet ((compare_station (station)
            (or (=:= (station-name station) name) (=:= (station-coords station) coords))))
    (if (lists:any #'compare_station/1 monitor)
      `#(error station_already_exists)
      (cons (make-station name name coords coords) monitor))))


;; helper function for searching measurement list
(defun filter (date type)
  (match-lambda
    (((tuple date type _v)) 'true)
    ((_other) 'false)))


;; Add measurement: get station from monitor,
;; check if measurement is already recorded in that station,
;; if not put it at the beginning.
(defun add_value (monitor station date type value)
  (flet ((internal_add_value
      (((cons current tail))
        (let ((measurements (station-measurements current)))
          (if (lists:any (filter date type) measurements)
            `#(error measurement_already_recorded)
            (cons (set-station-measurements current (cons #(date type value) measurements)) tail))))
      ((_other) `#(error no_such_station))))
   (search_monitor monitor station #'internal_add_value/1)))


;; Remove measurement: get station from monitor,
;; check if measurement is recorded in that station
;; and remove it.
(defun remove_value (monitor station date type)
  (flet ((internal_remove_value
      (((cons current tail))
        (let ((old_list (station-measurements current)))
          (case (lists:any (filter date type) old_list)
            ('true (cons (set-station-measurements current (lists:filter (lambda (x) (not (funcall (filter date type) x))) old_list)) tail))
            ('false #('error 'no_such_measurement)))))
      ((_) `#(error no_such_station))))
    (search_monitor monitor station #'internal_remove_value/1)))


;; Get measurement: get station from monitor,
;; check if measurement is recorded in that station
;; and return it.
(defun get_one_value (monitor station date type)
  (flet ((internal_get_value
    (((cons current tail))
      (case (lists:filter (filter date type) (station-measurements current))
        ((list (tuple _date _type value)) value)
        ((list) #('error 'no_such_measurement))))
        (([]) #('error 'no_such_station))))
    (search_monitor monitor station #'internal_get_value/1)))


;; Daily mean: get mean value of measurements of Type
;; from {Year, Month, Day}.
(defun get_daily_mean ((monitor type (tuple year month day))
  (flet
    ((internal_daily_mean
      ((station (tuple sum_acc num_acc))
        (let (((tuple value number)
          (lists:foldl
            (match-lambda (((tuple (tuple (tuple y m d) _hour) t v) (tuple sum num))
              (if (and (and (== t type) (== y year)) (and (== m month) (== d day)))
                #((+ sum v) (+ num 1))
                #(sum num))))
          #(0 0)
          (station-measurements station))))
             (tuple (+ sum_acc value) (+ num_acc number))))))
    (case (lists:foldl #'internal_daily_mean/2 (tuple 0 0) monitor)
      ((tuple 0 0) 0)
      ((tuple sum number) (when (and (is_number sum) (is_number number))) (/ sum number))
      (_ (tuple 'error 'wrong_arguments))))))


;; helper function for computing mean value of measurements of Type in one station
(defun type_mean (type)
  (flet ((sum_measurements
    (((tuple _ t v) (tuple sum num))
      (if (== t type) (tuple (+ sum v) (+ 1 num) (tuple sum num))))))
        (match-lambda
          (((cons station _))
            (case (lists:foldl #'sum_measurements/2 (tuple 0 0) (station-measurements station))
              ((tuple 0 0) 0)
              ((tuple value number) (/ value number))))
          (('[])
            `#(error no_such_station)))))


;; Mean value of Type in Station: get Station from monitor
;; and compute mean value of measurements of Type in it.
(defun get_station_mean (monitor station type)
  (search_monitor monitor station (type_mean type)))


;; Station with minimal mean value of Type: find station in
;; Monitor with loves mean value of measurements of Type.
(defun get_min_type_mean (monitor type)
  (let
    ((type_mean_fun (type_mean type)))
    (flet ((min_mean_fun
      ((station (tuple min_station min_mean))
        (case (funcall type_mean_fun '[station])
          ((tuple 'error reason) #('error reason))
          (mean (if (or (== 0 min_mean) (> min_mean mean))
            #(station mean)
            #(min_station min_mean)))))
          (('empty acc) acc)))
      (case (lists:foldl #'min_mean_fun/2 #('empty 0) monitor)
        ((tuple _ 0) `#(error no_such_measurement))
        ((tuple s m) (when (is_number m)) #((station-name s) m))
        (_ `#(error wrong_arguments))))))


;; helper function for computing distance between stations
(defun distance_from (((match-station name _ coords (tuple x1 y1) measurements _))
  (match-lambda
    (((match-station coords (tuple x2 y2))) (math:sqrt (+ (math:pow (- x1 x2) 2) (math:pow (- y1 y2) 2))))
    ((_) `#(error wrong_arguments)))))

;; helper function for finding two closest stations:
(defun closest_two
  (([] (tuple a b min_dist)) #(a b min_dist))
  (((cons _ []) (tuple a b min_dist)) #(a b min_dist))
  (((cons station tail) (tuple a b min_dist))
    (let ((distance_from_s (distance_from station)))
      (let* (((cons closest _) (lists:sort (lambda (x y) (< (funcall distance_from_s x) (funcall distance_from_s y))) tail))
        (distance (funcall distance_from_s closest)))
        (if (> min_dist distance)
          (closest_two tail #(station closest distance))
          (closest_two tail #(a b min_dist))))))
  ((_ _) (tuple 'error 'wrong_arguments)))

;; Two closest stations: get two stations in Monitor
;; that are closest to each other.
(defun get_two_closest_stations (monitor)
  (case (closest_two monitor `#(empty empty infinity))
    ((tuple (match-station name first_name) (match-station name second_name) distance) #(first_name second_name distance))
    (other other)))
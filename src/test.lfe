(defmodule test
	(export (get_two_closest_stations 1)))


(defrecord station name coords (measurements '[]))

;; Comparing stations
(defun check_station
;; by coordinates
  (((match-station coords station_coords) (tuple x y))
   ( =:= station_coords #(x y)))
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

;; helper function for computing distance between stations
(defun distance_from (((match-station name _ coords (tuple x1 y1) measurements _))
  (match-lambda
    (((match-station coords (tuple x2 y2))) (math:sqrt (+ (math:pow (- x1 x2) 2) (math:pow (- y1 y2) 2))))
    ((_) (tuple 'error 'wrong_arguments)))))

;; helper function for finding two closest stations:
(defun closest_two
  (([] (tuple a b min_dist)) (tuple a b min_dist))
  (((cons _ []) (tuple a b min_dist)) (tuple a b min_dist))
  (((cons station tail) (tuple a b min_dist))
    (let ((distance_from_s (distance_from station)))
      (let* (((cons closest _) (lists:sort (lambda (x y) (< (funcall distance_from_s x) (funcall distance_from_s y))) tail))
        (distance (funcall distance_from_s closest)))
        (if (> min_dist distance)
          (closest_two tail (tuple station closest distance))
          (closest_two tail (tuple a b min_dist))))))
  ((_ _) (tuple 'error 'wrong_arguments)))

;; Two closest stations: get two stations in Monitor
;; that are closest to each other.
(defun get_two_closest_stations (monitor)
  (case (closest_two monitor (tuple 'empty 'empty 'infinity))
    ((tuple (match-station name first_name) (match-station name second_name) distance) (tuple first_name second_name distance))
    (other other)))

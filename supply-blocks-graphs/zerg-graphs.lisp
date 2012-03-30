;;;; zerg-graphs.lisp

;;; Globals

;; do "env gnuplot ..." instead?
(defparameter *gnuplot* "/usr/bin/gnuplot")

;; http://wiki.teamliquid.net/starcraft2/Resources#Mining_Rates
(defparameter *1-worker-per-geyser*  (/  42 60))
(defparameter *2-workers-per-geyser* (/  84 60))
(defparameter *3-workers-per-geyser* (/ 114 60))

(defparameter *1-worker-per-patch*   (/  45 60))
(defparameter *2-workers-per-patch*  (/  90 60))
(defparameter *3-workers-per-patch*  (/ 102 60))

(defparameter *stats*
  '(;; buildings
    (:type :hatchery      :minerals 300 :gas 0 :time 100 :supply-cap 2)
    (:type :spawning-pool :minerals 200 :gas 0 :time 65)
    ;; units
    (:type :drone    :minerals  50 :gas 0 :time 17 :supply 1)
    (:type :larva                         :time 15)  ; when hatch < 3 larva
    (:type :queen    :minerals 150 :gas 0 :time 50 :supply 2)
    (:type :overlord :minerals 100 :gas 0 :time 40 :supply-cap 8)
    ;; spells
    (:type :spawn-larva :energy 25 :time 40)))


;;; Classes

(defclass drone ()
  ((indicator   :reader   indicator   :initform #\d)))


(defclass hatchery ()
  ((indicator   :reader   indicator   :initform #\h)
   (larva-timer :accessor larva-timer :initform nil)
   (n-larva     :accessor n-larva     :initform 3)
   (queen       :accessor queen       :initform nil)
   (spawn-larva :accessor spawn-larva :initform nil)))


(defclass larva ()
  ((indicator :reader indicator :initform #\l)))


(defclass overlord ()
  ((indicator :reader indicator :initform #\o)))


(defclass queen ()
  ((hatchery    :accessor hatchery    :initform nil)
   (indicator   :reader   indicator   :initform #\q)
   (max-energy  :accessor max-energy  :initform 200)
   (energy      :accessor energy      :initform  25)))


(defclass spawning-pool ()
  ((indicator :reader indicator :initform #\s)))


(defclass state ()
  ((max-supply :accessor max-supply :initform 200)
   (supply     :accessor supply     :initform 6)
   (buildings  :accessor buildings  :initform (list (make-instance 'hatchery)))
   (units      :accessor units      :initform (list (make-instance 'overlord)
                                                    (make-instance 'drone)
                                                    (make-instance 'drone)
                                                    (make-instance 'drone)
                                                    (make-instance 'drone)
                                                    (make-instance 'drone)
                                                    (make-instance 'drone)))
   (minerals   :accessor minerals   :initform 50)
   (gas        :accessor gas        :initform  0)
   (queue      :accessor queue      :initform nil)))


;;; Functions

(defun count-buildings (state type)
  (loop for building in (buildings state)
        when (typep building type) sum 1))


(defun count-units (state type)
  (loop for unit in (units state)
        when (typep unit type) sum 1))


(defun delay-of (order)
  (if (> (length order) 2)
      (elt order 2)
      nil))


;; "drone minus 1": remove one drone from the game state
(defun drone-1 (state)
  (loop for unit in (units state)
        do (when (typep unit 'drone)
             (setf (units state) (remove unit (units state)))
             (loop-finish))))


(defun drones (state)
  (count-units state 'drone))


(defun gather-minerals (state patches)
  (multiple-value-bind (workers-per-patch remainder)
      (floor (drones state) patches)
    (incf (minerals state)
          (cond ((= workers-per-patch 0)
                 (* remainder *1-worker-per-patch*))
                ((= workers-per-patch 1)
                 (+ (* (- patches remainder) *1-worker-per-patch*)
                    (* remainder *2-workers-per-patch*)))
                ((= workers-per-patch 2)
                 (+ (* (- patches remainder) *2-workers-per-patch*)
                    (* remainder *3-workers-per-patch*)))
                ((>= workers-per-patch 3)
                 (* patches *3-workers-per-patch*))
                (t 0)))))


(defun hatcheries (state)
  (count-buildings state 'hatchery))


;; Increase energy of units (only queens currently).
(defun increase-energy (state)
  (loop for unit in (units state)
        do (when (and (typep unit 'queen)
                      (< (energy unit) (max-energy unit)))
             (incf (energy unit)))))


(defun inject-larvae (state)
  (loop for building in (buildings state)
        do (when (and (typep building 'hatchery)
                      (null (spawn-larva building))
                      (queen building)
                      (>= (energy (queen building))
                          (stats :spawn-larva :energy)))
             (push (list :spawn-larva (stats :spawn-larva :time) building)
                   (queue state))
             (setf (spawn-larva building) (stats :spawn-larva :time))
             (decf (energy (queen building)) (stats :spawn-larva :energy)))))


(defun kind-of (order)
  (elt order 1))


(defun larvae (state)
  (loop for building in (buildings state)
        when (typep building 'hatchery) sum (n-larva building)))


;; "larvae minus 1": remove one larva from the game state
;;
;; I tried "(defun (setf larvae) ..." but it had some issues.
(defun larvae-1 (state)
  (loop for building in (buildings state)
        do (when (and (typep building 'hatchery)
                      (> (n-larva building) 0))
             (decf (n-larva building))
             (loop-finish))))


(defun larva-birth (state)
  (loop for building in (buildings state)
        do (when (typep building 'hatchery)
             ;; pulled out of COND to get timing right
             (when (larva-timer building)
               (decf (larva-timer building)))
             (cond ;; hatchery has 3 or more larvae: do nothing
                   ((>= (n-larva building) 3)
                    (setf (larva-timer building) nil))
                   ;; hatchery has less than 3 larvae and no timer is running
                   ((and (< (n-larva building) 3)
                         (null (larva-timer building)))
                    (setf (larva-timer building) (stats :larva :time)))
                   ;; hatchery has less than 3 larvae and timer <= 0
                   ((and (< (n-larva building) 3)
                         (<= (larva-timer building) 0))
                    (incf (n-larva building))
                    (setf (larva-timer building) (stats :larva :time))))
             ;; lets handle this counter here as well
             (cond ((and (spawn-larva building)
                         (> (spawn-larva building) 0))
                    (decf (spawn-larva building)))
                   ((and (spawn-larva building)
                         (<= (spawn-larva building) 0))  ; for clarity
                    (setf (spawn-larva building) nil))))))


(let ((delay nil))
  (defun maybe-execute-order (state order)
    (cond ((and (null delay) (delay-of order))
           (setf delay (delay-of order))
           nil)
          ((and delay (> delay 0))
           (decf delay)
           nil)
          (t (let ((queued
                    (case (kind-of order)
                      (:hatchery      (maybe-queue-hatchery state))
                      (:overlord      (maybe-queue-overlord state))
                      (:queen         (maybe-queue-queen state))
                      (:spawning-pool (maybe-queue-spawning-pool state))
                      (otherwise nil))))
               (when queued
                 (setf delay nil))
               queued)))))


(defun maybe-queue-drone (state)
  (let ((supply (stats :drone :supply)))
    (when (and (<= (+ (supply state) supply) (supply-cap state))
               (> (larvae state) 0)
               (>= (minerals state) (stats :drone :minerals)))
      (push (list :drone (stats :drone :time)) (queue state))
      (larvae-1 state)
      (incf (supply state) supply)
      (decf (minerals state) (stats :drone :minerals))
      t)))


(defun maybe-queue-hatchery (state)
  (when (and (> (drones state) 0)
             (>= (minerals state) (stats :hatchery :minerals)))
    (push (list :hatchery (stats :hatchery :time)) (queue state))
    (drone-1 state)
    (decf (minerals state) (stats :hatchery :minerals))
    t))


(defun maybe-queue-overlord (state)
  (when (and (> (larvae state) 0)
             (>= (minerals state) (stats :overlord :minerals)))
    (push (list :overlord (stats :overlord :time)) (queue state))
    (larvae-1 state)
    (decf (minerals state) (stats :overlord :minerals))
    t))


(defun maybe-queue-queen (state)
  (let ((supply (stats :queen :supply)))
    (when (and (< (queens-in-production state) (hatcheries state))
               (<= (+ (supply state) supply) (supply-cap state))
               (> (spawning-pools state) 0)
               (>= (minerals state) (stats :queen :minerals)))
    (push (list :queen (stats :queen :time)) (queue state))
    (incf (supply state) supply)
    (decf (minerals state) (stats :queen :minerals))
    t)))


(defun maybe-queue-spawning-pool (state)
  (when (and (> (drones state) 0)
             (>= (minerals state) (stats :spawning-pool :minerals)))
    (push (list :spawning-pool (stats :spawning-pool :time)) (queue state))
    (drone-1 state)
    (decf (minerals state) (stats :spawning-pool :minerals))
    t))


(defun overlords (state)
  (count-units state 'overlord))


(defun print-debug-header ()
  (format *debug-io* "  sec |    supply | h | q |  l |  o |   d | minerals |  gas | queue~%")
  (format *debug-io* "------+-----------+---+---+----+----+-----+----------+------+-------~%"))


(defun print-debug-output (state time)
  (format *debug-io* " ~4D | ~3D / ~3D | ~D | ~D | ~2D | ~2D | ~3D | ~8F | ~4F | ~A~%"
          time (supply state) (supply-cap state) (hatcheries state)
          (queens state) (larvae state) (overlords state) (drones state)
          (minerals state) (gas state) (queue2str (queue state))))


(defun process-queue (state)
  (loop with new-queue = nil
        for item in (sort (copy-seq (queue state))
                          (lambda (a b)
                            (> (elt a 1) (elt b 1))))
        do (decf (elt item 1))
           (if (<= (elt item 1) 0)
               (case (elt item 0)
                 (:drone         (spawn-drone state))
                 (:hatchery      (spawn-hatchery state))
                 (:overlord      (spawn-overlord state))
                 (:queen         (spawn-queen state))
                 (:spawn-larva   (spawn-spawn-larva (elt item 2)))
                 (:spawning-pool (spawn-spawning-pool state)))
               (push item new-queue))
        finally (setf (queue state) new-queue)))



(defun queens (state)
  (count-units state 'queen))


(defun queens-in-production (state)
  (loop for item in (queue state)
        when (equal :queen (elt item 0)) sum 1))


(defun queue2str (queue)
  (loop for item in (sort (copy-seq queue)
                          (lambda (a b)
                            (> (elt a 1) (elt b 1))))
        when (equal :drone         (elt item 0)) collect #\d into result
        when (equal :hatchery      (elt item 0)) collect #\h into result
        when (equal :overlord      (elt item 0)) collect #\o into result
        when (equal :queen         (elt item 0)) collect #\q into result
        when (equal :spawn-larva   (elt item 0)) collect #\i into result
        when (equal :spawning-pool (elt item 0)) collect #\s into result
        finally (return (coerce result 'string))))


(defun spawn-drone (state)
  (push (make-instance 'drone) (units state)))


(defun spawn-hatchery (state)
  (let ((hatchery (make-instance 'hatchery)))
    (push hatchery (buildings state))
    ;; assign a spare queen to the hatchery
    (loop for unit in (units state)
          do (when (and (typep unit 'queen)
                        (not (hatchery unit)))
               (setf (hatchery unit) hatchery)
               (setf (queen hatchery) unit)
               (loop-finish)))))


(defun spawn-overlord (state)
  (push (make-instance 'overlord) (units state)))


(defun spawn-queen (state)
  (let ((queen (make-instance 'queen)))
    (push queen (units state))
    ;; assign to hatchery that has no queen
    (loop for building in (buildings state)
          do (when (and (typep building 'hatchery)
                        (not (queen building)))
               (setf (hatchery queen) building)
               (setf (queen building) queen)
               (loop-finish)))))


(defun spawn-spawn-larva (hatchery)
  (incf (n-larva hatchery) 4)
  (when (> (n-larva hatchery) 19)
    (setf (n-larva hatchery) 19)))


(defun spawn-spawning-pool (state)
  (push (make-instance 'spawning-pool) (buildings state)))


(defun spawning-pools (state)
  (count-buildings state 'spawning-pool))


(defun stats (type &optional property)
  (loop for item in *stats*
        do (when (equal type (getf item :type))
             (if property
                 (return-from stats (getf item property))
                 (return-from stats item)))))


(defun supply-cap (state)
  (loop for entity in (append (buildings state) (units state))
        for supply-cap = (stats (symbol2keyword (type-of entity)) :supply-cap)
        when supply-cap sum supply-cap))


(defun supply-of (order)
  (elt order 0))


(defun symbol2keyword (symbol)
  (case symbol
    (drone         :drone)
    (hatchery      :hatchery)
    (larva         :larva)
    (overlord      :overlord)
    (queen         :queen)
    (spawning-pool :spawning-pool)))


(defun total-patches (state patches-per-location)
  (loop for building in (buildings state)
        when (typep building 'hatchery) sum patches-per-location))


;;; Main Program

;; 'Delay' is a little awkward and unintuitive in its usage but I didn't
;; want to needlessly complicate the code for now.  What delay means is that
;; the order will be delayed from the moment there's enough SUPPLY.  Not
;; necessarily from the moment it is actually possible to execute the order
;; (f.e. due to minerals), so a delay will not line up with the expected
;; moment most of the times.  Sorry...
(defun main (&key (state (make-instance 'state)) (run-time (* 8 60))
                  (file "tmp.dat") (debug nil) (debug-timer 10)
                  (geysers 2) (mineral-patches 8)
                  ;;             (supply type delay)
                  (build-order '(( 9 :overlord)
                                 (15 :hatchery)
                                 (15 :spawning-pool)
                                 (16 :overlord)
                                 (16 :queen)
                                 (16 :queen)
                                 (24 :overlord)
                                 (25 :hatchery)
                                 (26 :overlord)
                                 (32 :overlord)
                                 (34 :queen)
                                 (40 :overlord)
                                 (48 :overlord)
                                 (54 :overlord)
                                 (62 :overlord)
                                 (70 :overlord)
                                 (78 :overlord)
                                 (86 :overlord))))
  (declare (ignore geysers))
  (when debug (print-debug-header))
  (with-open-file (f file :direction :output :if-exists :supersede)
    (loop with order = (let ((first-order (first build-order)))
                         (setf build-order (rest build-order))
                         first-order)
          for time from 0 below run-time
          do (when (and debug (= (mod time debug-timer) 0))
               (print-debug-output state time))
             (larva-birth state)
             (increase-energy state)
             (process-queue state)
             (gather-minerals state (total-patches state mineral-patches))
             (inject-larvae state)
             (if (and order (>= (supply state) (supply-of order)))
                 (when (maybe-execute-order state order)
                   (setf order       (first build-order)
                         build-order (rest build-order)))
                 (maybe-queue-drone state))
             (format f "~D ~D ~D~%"
                     time (floor (minerals state)) (drones state))))
  (list :supply (list (supply state) (supply-cap state)):drones (drones state)
        :minerals (floor (minerals state)) :gas (gas state)
        :hatcheries (hatcheries state) :queens (queens state)))


;; Try out this build order:
(defun 8-mins-15h15p ()
  (main :debug t :debug-timer 1
        :file "8-mins-15h15p.dat"
        :build-order '(( 9 :overlord)
                       (15 :hatchery)
                       (15 :spawning-pool)
                       (16 :overlord)
                       (16 :queen)
                       (16 :queen)
                       (24 :overlord)
                       (25 :hatchery)
                       (26 :overlord)
                       (32 :overlord)
                       (34 :queen)
                       (40 :overlord)
                       (48 :overlord)
                       (54 :overlord)
                       (62 :overlord)
                       (70 :overlord)
                       (78 :overlord)
                       (86 :overlord)))
  (format *debug-io* "---~%")
  ;(main :debug t :debug-timer 1
  ;      :file "8-mins-15h15p-28sb.dat"
  ;      :build-order '(( 9 :overlord)
  ;                     (15 :hatchery)
  ;                     (15 :spawning-pool)
  ;                     (16 :overlord)
  ;                     (16 :queen)
  ;                     (16 :queen)
  ;                     (25 :hatchery)
  ;                     (28 :overlord 38)
  ;                     (28 :overlord)
  ;                     (32 :overlord)
  ;                     (34 :queen)
  ;                     (40 :overlord)
  ;                     (48 :overlord)
  ;                     (54 :overlord)
  ;                     (62 :overlord)
  ;                     (70 :overlord)
  ;                     (78 :overlord)
  ;                     (86 :overlord)))
  (main :debug t :debug-timer 1
        :file "8-mins-15h15p-28sb.dat"
        :build-order '(( 9 :overlord)
                       (15 :hatchery)
                       (15 :spawning-pool)
                       (16 :overlord)
                       (16 :queen)
                       (16 :queen)
                       (25 :hatchery)
                       (26 :queen)
                       (28 :overlord 38)
                       (28 :overlord)
                       (28 :overlord)
                       (32 :overlord)
                       (46 :overlord)
                       (54 :overlord)
                       (62 :overlord)
                       (70 :overlord)
                       (78 :overlord)
                       (86 :overlord)))
  )

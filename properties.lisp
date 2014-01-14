(defun only-one-door-at-a-time (vars-in)
  (let* ((vars-out (hotel vars-in))
         (unlockDoor1 (cadddr (cdr  vars-out)))
         (unlockDoor2 (cadddr (cddr vars-out))))
    (not (and unlockDoor1 unlockDoor2))))
              

(defthm prove-only-one-door-at-a-time (only-one-door-at-a-time vars-in))

(good-bye)

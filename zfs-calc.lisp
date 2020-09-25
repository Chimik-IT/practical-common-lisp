;; Name: ZFS-Calculator
;; Version: 1.0.0
;; Author: Chimik-IT
;; Date: 09/22/2020

;; In this early stage I have created one function, that resembles the notion of how raid5 and raid6 were
;; calculated. with 1-1/n and 1-2/n respactively while n is the amount of disks used in the arrays. I added
;; 1-3/n for the raidz3 serving triple parity. Since the ZFS does a little slop allocation I needed to add
;; the correction factor "slop" it's bound to some conditionals marking it's size borders.


(defvar number-of-discs)
(defvar size-of-discs)
(defvar zfs-raidz-number)
(defvar disc-size-unit)
(defvar number-of-groups)
(defvar slop-space-allocation)
(defvar factor)
(defun storage-capacity ()
 (* size-of-discs number-of-discs 0.9951))
(defun parity-padding-cost (&optional (block-size 64))
  (if (< zfs-raidz-number 2)
      (setf factor 1)
      (setf factor 0.875))
  (- (/ (ceiling (+ block-size (* zfs-raidz-number (floor (/ (+ block-size number-of-discs (- zfs-raidz-number) -1) (- number-of-discs zfs-raidz-number))))) factor) block-size) 1))
(defun raidz-calc ()
  (princ "Enter disc count per group: ")
  (setq number-of-discs (read))
  (princ "Enter size of discs: ")
  (setq size-of-discs (read))
  (princ "Enter raidz number: ")
  (setq zfs-raidz-number (read))
  (princ "Enter number of groups: ")
  (setq number-of-groups (read))
  (princ "Enter unit of disc size: ")
  (setq disc-size-unit (read))
  (cond ((> (/ (* size-of-discs number-of-discs) 32) (/ (* size-of-discs number-of-discs) 2)) (setq slop-space-allocation (/ (* size-of-discs number-of-discs) 2)))
	(t (setq slop-space-allocation (/ (* size-of-discs number-of-discs) 32))))
  (format t "The usable size of the ZFS array: ~F~A" (* number-of-groups (- (storage-capacity) (* (storage-capacity) (/ (parity-padding-cost) (+ (parity-padding-cost) 1))) slop-space-allocation)) disc-size-unit)
  (setq number-of-discs nil
	size-of-discs nil
	zfs-raidz-number nil
	disc-size-unit nil))


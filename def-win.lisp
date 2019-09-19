;;(in-package moge)

(defmacro my-enum (&rest names)
  `(progn
     ,@(loop for i from 0
             for name in names
	  collect `(defconstant ,name ,i))))

(my-enum +big-crouch+ +fire-crouch+ +star-crouch+)
(my-enum +migi+ +normal+ +hidari+)
(my-enum +coin+ +hard-block1+ +hard-block2+ +hatena-block+ +kara-block+ +kinoko+ +soft-block+
	 +star+ +koura+ +1up+ +toumei-block+ +top-left-dokan+ +mid-left-dokan+ +bot-left-dokan+ +top-right-dokan+
	 +mid-right-dokan+ +bot-right-dokan+ +fireball+ +flower+)


(defconstant +small+       0)
(defconstant +big+         3)
(defconstant +fire+        6)
(defconstant +big-star+    9)
(defconstant +small-star+ 12)

;;------------------------------------------------------------------------------------------
;;ウィンドウ透過用
(defcfun (%set-layered-window-attributes "SetLayeredWindowAttributes" :convention :stdcall)
         :boolean
  (hwnd :pointer)
  (crkey :int32)
  (balpha :uint8)
  (dwflags :uint32))

(defun set-layered-window-attributes (hwnd crkey balpha dwflags)
  (%set-layered-window-attributes hwnd crkey balpha dwflags))

;;もげお by もげぞうβ
;;TODO アイテムとobjの当たり判定
;;
(ql:quickload :trivial-gamekit)

(load "stage.lisp")
(defparameter *obj-w* 32)
(defparameter *obj-h* 32)

(defparameter *tate* 20)
(defparameter *yoko* 30)

(defparameter +screen-w+ (* *obj-w* *yoko*))
(defparameter +screen-h+ (* *obj-h* *tate*))

(defparameter +screen-center-x+ (floor +screen-w+ 2))

(defparameter *light-blue* (gamekit:vec4 0 1 1 1))
(defparameter *yellow* (gamekit:vec4 1 1 0 1))
(defparameter *brown*  (gamekit:vec4 0.651 0.152 0.015 1))
(defparameter *brown2*  (gamekit:vec4 0.751 0.252 0.015 1))
(defparameter *red* (gamekit:vec4 1 0 0 1))
(defparameter *green* (gamekit:vec4 0 0.5 0 1))
(defparameter *kuribo* (gamekit:vec4 0.91 0.376 0.063 1))
(defparameter *nokonoko* (gamekit:vec4 0 0.678 0 1))
(defparameter *flag* (gamekit:vec4 0.722 0.973 0.094 1))
(defparameter *kinoko* (gamekit:vec4 0.973 0.22 0 1))

(defparameter *p* nil) ;;player
(defparameter *field* nil)
(defparameter *keystate* nil)



(defclass keystate ()
  ((left
     :accessor left
     :initform nil
     :initarg :left)
   (right
     :accessor right
     :initform nil
     :initarg :right)
   (x
     :accessor x
     :initform nil
     :initarg :x)
   (z
     :accessor z
     :initform nil
     :initarg :z)))

(defclass obj ()
  ((color
    :accessor color
    :initform nil
    :initarg :color)
   (obj-type
    :accessor obj-type
    :initform 0
    :initarg :obj-type)
   (pos
    :accessor pos
    :initform (gamekit:vec2 0 0) ;; x y
    :initarg :pos)))

(defclass chara (obj)
  ((ax
    :accessor ax
    :initform 0
    :initarg :ax)
   (ay
    :accessor ay
    :initform 0
    :initarg :ay)
   (vx
    :accessor vx
    :initform 0
    :initarg :vx)
   (vy
    :accessor vy
    :initform 0
    :initarg :vy)
   (lastpos
    :accessor lastpos
    :initform (gamekit:vec2 0 0)
    :initarg :lastpos)
   (jump
    :accessor jump
    :initform nil
    :initarg :jump)
   (scroll
    :accessor scroll
    :initform 0
    :initarg :scroll)
   (field-w-max
    :accessor field-w-max
    :initform 0
    :initarg :field-w-max)
   (fall ;;落下
    :accessor fall
    :initform nil
    :initarg :fall)
    (state ;;キャラの状態 or アイテムの種類
      :accessor state
      :initform :small
      :initarg :state)
    (items ;;ステージ上に出現したアイテム
      :accessor items
      :initform nil
      :initarg :items)))

(gamekit:defgame mogeo () ()
  (:viewport-width +screen-w+)
  (:viewport-height +screen-h+)
  (:viewport-title "スーパーもげおワールド"))

(defun rect-center-x (pos)
  (+ (gamekit:x pos) (floor *obj-w* 2))) 

;;ステージデータ生成
(defun create-field (field-obj p)
  (with-slots (field-w-max) p
    (let* ((tate (length field-obj))
           (yoko (length (car field-obj)))
           (arr (make-array (list tate yoko) :initial-contents field-obj))
           (obj-list nil))
      (setf field-w-max yoko)
      (loop for y from (1- tate) downto 0
            do (loop for x below yoko
                     do (let ((obj-type (aref arr y x)))
                          (when (not (eq obj-type 0))
                            (let ((obj-pos (gamekit:vec2 (* x 32) (* (- (1- tate) y) 32)))
                                  (color
                                    (case obj-type
                                      (1 *brown*)
                                      (2 *brown2*) 
                                      ((3 z) *green*)
                                      (8 *light-blue*)
                                      ((4 5 6 7 9)
                                       *yellow*)
                                      (a *kuribo*)
                                      (b *nokonoko*)
                                      (y *flag*))))
                              (push
                                (make-instance 'obj :pos obj-pos
                                                    :color color
                                                    :obj-type obj-type)
                                obj-list))))))
      (make-array (length obj-list) :initial-contents obj-list))))

(defun init-data ()
  (setf *p* (make-instance 'chara :pos (gamekit:vec2 0 64) :lastpos (gamekit:vec2 0 64)
                                  :vx 2 :vy 0)
        *field* (create-field *stage1-1* *p*)
        *keystate* (make-instance 'keystate)))

(defun input-key ()
  (with-slots (left right z x) *keystate*
    (gamekit:bind-button :left :pressed
      (lambda () (setf left t)))
    (gamekit:bind-button :left :released
      (lambda () (setf left nil)))
    (gamekit:bind-button :right :pressed
      (lambda () (setf right t)))
    (gamekit:bind-button :right :released
      (lambda () (setf right nil)))
    (gamekit:bind-button :z :pressed
      (lambda () (setf z t)))
    (gamekit:bind-button :z :released
      (lambda () (setf z nil)))
    (gamekit:bind-button :x :pressed
      (lambda () (setf x t)))
    (gamekit:bind-button :x :released
      (lambda () (setf x nil)))
    (gamekit:bind-button :escape :pressed
        (lambda ()
          (gamekit:stop)))))

(defun draw-field ()
  (with-slots (scroll) *p*
    (loop :for obj across *field*
          :do (with-slots (pos color obj-type) obj
                (when (and (>= (+ (gamekit:x pos) *obj-w*) scroll) (>= (+ scroll +screen-w+) (gamekit:x pos)))
                  (let ((new-pos (gamekit:vec2 (- (gamekit:x pos) scroll) (gamekit:y pos))))
                    (gamekit:draw-rect new-pos *obj-w* *obj-h* :fill-paint color
                                                              :stroke-paint (gamekit:vec4 0 0 0 1))))))))
;;背景表示 
(defun draw-background ()
  (gamekit:draw-rect (gamekit:vec2 0 0) +screen-w+ +screen-h+ :fill-paint *light-blue*))

(defun draw-player ()
  (with-slots (pos scroll) *p*
    (let ((new-pos (gamekit:vec2 (- (gamekit:x pos) scroll) (gamekit:y pos))))
      (gamekit:draw-rect new-pos *obj-w* *obj-h* :fill-paint *red*)
      (gamekit:draw-line (gamekit:vec2 (gamekit:x new-pos) (+ (gamekit:y new-pos) 6))
                          (gamekit:vec2 (+ *obj-w* (gamekit:x new-pos)) (+ (gamekit:y new-pos) 6))
                          (gamekit:vec4 0 0 1 1)))))

(defun draw-items ()
  (with-slots (items scroll) *p*
    (dolist (item items)
      (with-slots (pos color) item
        (let ((new-pos (gamekit:vec2 (- (gamekit:x pos) scroll) (gamekit:y pos))))
          (gamekit:draw-rect new-pos *obj-w* *obj-h* :fill-paint color
                                                     :stroke-paint (gamekit:vec4 0 0 0 1)))))))

;;座標を表示するよ
(defun draw-debug ()
  (with-slots (left right z) *keystate*
    (with-slots (pos vx vy) *p*
      (gamekit:draw-text (format nil "player-x:~d player-y:~d vx:~d vy:~d" (gamekit:x pos) (gamekit:y pos) vx vy)
                          (gamekit:vec2 100 300)))))
    ;;(gamekit:draw-text (format nil " left    :~:[ no ~; yes ~] " left) (gamekit:vec2 100 100))
    ;;(gamekit:draw-text (format nil " right    :~:[ no ~; yes ~] " right) (gamekit:vec2 100 200))
    ;;(gamekit:draw-text (format nil " z    :~:[ no ~; yes ~] " z) (gamekit:vec2 100 300))))

(defparameter *text-buffer* nil)
(defun debug-format (&rest args)
  (let ((line (apply #'format (append '(nil) args))))
    (setf *text-buffer* (cons line *text-buffer*))
    (setf *text-buffer* (subseq *text-buffer* 0 (min (length *text-buffer*) 5)))))

(defun draw-console ()
  (loop for i below 5
        for line in *text-buffer*
        do
        (gamekit:draw-text line (gamekit:vec2 200 (- 580 (* i 20))))))
    
;;アイテムが動くよ
(defun update-items ()
  (with-slots (items) *p*
    (dolist (item items)
      (with-slots (pos vx) item
        (incf (gamekit:x pos) vx)))))


;;プレーヤーが動くよ🧢👨
(defun update-player ()
  (with-slots (left right z x) *keystate*
    (with-slots (pos vx vy lastpos jump fall scroll) *p*
      (when left
        (decf (gamekit:x pos) vx))
      (when right
        (incf (gamekit:x pos) vx))
      (if (and x (null jump) (null fall))
          (setf jump t
                vy 18)
          (when (or jump fall)
            (setf vy -1)))
      (when z
        (incf (gamekit:x pos) vx))
      ;;プレイヤーのy座標更新
      (let ((temp (gamekit:y pos)))
        (incf (gamekit:y pos) (+ (- (gamekit:y pos) (gamekit:y lastpos)) vy))
        (setf (gamekit:y lastpos) temp)))))

;;     y 0 1 3 6 10 15 21 ...
;; lasty 0 0 1 3  6 10 15 ...
;;    vy 1 1 1 1  1  1  1 ...

;;画面スクロール
(defun update-scroll ()
  (with-slots (pos scroll) *p*
    (when (or (> (rect-center-x pos) (+ scroll +screen-center-x+))
              (< (rect-center-x pos) (+ scroll +screen-center-x+)))
      (setf scroll (max 0 (- (rect-center-x pos) +screen-center-x+ ))))))

;;プレイヤー当たり判定の判定
(defun hit-player-obj (obj p-x p-x2 p-y p-y2 p-center)
  (let* ((obj-pos (pos obj))
         (obj-x (gamekit:x obj-pos))
         (obj-x2 (+ obj-x *obj-w*))
         (obj-y (gamekit:y obj-pos))
         (obj-y2 (+ obj-y *obj-h*)))
    (cond
      ;;プレイヤーの下側とobjの上側 （プレイヤーの下側がオブジェクトの中にめり込んでいたらプレイヤーをオブジェクトの上に持ち上げる）
      ((and (>= obj-x2 p-center obj-x)
            (>= obj-y2 p-y) (< obj-y2 p-y2)
            (< obj-y p-y) (< obj-y p-y2)) ;(< obj-y p-y)ならばかならず(< obj-y p-y2)
        ;;(debug-format "~A (~A,~A) SITA HOSEI!" (get-universal-time) obj-x obj-y)
        1) ;; ここまでよんだ

      ;;プレイヤーの上側とobjの下側当たり判定
      ((and (>= obj-x2 p-center obj-x)
            (> obj-y2 p-y) (> obj-y2 p-y2)
            (> obj-y p-y) (< obj-y p-y2)) ;; 今ここを読んでいます ha2ne2
        (debug-format "~A (~A,~A) (~A,~A) UE HOSEI!" (get-universal-time) obj-x obj-y p-x p-y)
        2)
;; 今からここ読みます。 6とは何でしょうか！

      ;;プレイヤーの左側とobjの右側 
      ((and (or (> obj-y2 (+ p-y 6) obj-y)) ;;(> obj-y2 (- p-y2 6) obj-y))
            (> obj-x2 p-x) (< obj-x2 p-x2)
            (< obj-x p-x) (< obj-x p-x2))
        (debug-format "~A (~A,~A) (~A,~A) HIDARI HOSEI!" (get-universal-time) obj-x obj-y p-x p-y)
        3)
      ;;プレイヤーの右側とobjの左側
      ((and (or (> obj-y2 (+ p-y 6) obj-y)) ;;(> obj-y2 (- p-y2 6) obj-y))
            (> obj-x2 p-x) (> obj-x2 p-x2)
            (> obj-x p-x) (< obj-x p-x2))
        (debug-format "~A (~A,~A) (~A,~A) MIGI HOSEI!" (get-universal-time) obj-x obj-y p-x p-y)
        4))))

;; プレイヤーとブロックがあたったら
(defun hit-player-block (obj i)
  (with-slots (obj-type color pos) obj
    (with-slots (state items) *p*
      (case obj-type
        (2 (setf *field* (remove obj *field* :test #'equal)))
        (5 (setf obj-type 1
                color *brown*)
            (case state
              (:small (push (make-instance 'chara :vx 1 :state :kinoko :color *kinoko*
                                                  :pos (gamekit:vec2 (gamekit:x pos) (+ (gamekit:y pos) *obj-h*)))
                            items)))))
      )))

;;プレイヤー当たり判定
(defun hit-player-objects ()
  (with-slots (scroll vy jump fall) *p*
    (let* ((p-pos (pos *p*))
	         (hoge nil) ; 接地したか判定
           (p-d nil) (p-u nil) (p-l nil) (p-r nil) ;;プレイヤーの上下左右の当たり判定
           (p-center (rect-center-x p-pos))
           (p-x (gamekit:x p-pos)) (p-x2 (+ p-x *obj-w*))
           (p-y (gamekit:y p-pos)) (p-y2 (+ p-y *obj-h*)))
      ;;(debug-format "hit-player: (px,py) = (~A,~A)" p-x p-y)
      (loop :for obj across *field*
            :for i from 0
            :do (with-slots (obj-type) obj
                  (when (or (integerp obj-type)
                            (eq obj-type 'z))
                    (case (hit-player-obj obj p-x p-x2 p-y p-y2 p-center)
                      (1 (setf p-d obj))
                      (2 
                        (setf p-u obj)
                        (hit-player-block obj i))
                      (3 (setf p-l obj))
                      (4 (setf p-r obj))))))
      (when p-d
        (let* ((obj-pos (pos p-d))
                (obj-y2 (+ (gamekit:y obj-pos) *obj-h*)))
          (setf hoge t vy 0
                (gamekit:y p-pos) obj-y2
                (gamekit:y (lastpos *p*)) obj-y2)))
      (when p-u
        (let* ((obj-pos (pos p-u))
              (obj-y (gamekit:y obj-pos)))
          (setf (gamekit:y p-pos) (- obj-y *obj-h*)
                (gamekit:y (lastpos *p*)) (- obj-y *obj-h*))))
      (when (and p-l
                (or (and p-d (null fall))
                    (and fall (null p-d))))
        (let* ((obj-pos (pos p-l))
                (obj-x2 (+ (gamekit:x obj-pos) *obj-w*)))
          (setf (gamekit:x p-pos) obj-x2)))
      (when (and p-r
                (or (and p-d (null fall))
                    (and fall (null p-d))))
        (let* ((obj-pos (pos p-r))
                (obj-x (gamekit:x obj-pos)))
          (setf (gamekit:x p-pos) (- obj-x *obj-w*))))
      (if hoge
          (setf jump nil fall nil)
          (setf fall t)))))  ; 🐈🐈🐈🐈

;;ゲームループ？
(defmethod gamekit:act ((app mogeo))
  (update-player)
  (update-items)
  (update-scroll)
  (hit-player-objects)
  (sleep 0.011))

;;描画
(defmethod gamekit:draw ((app mogeo))
  (draw-background)
  (draw-field)
  (draw-player)
  (draw-items)
  (draw-debug)
  (draw-console))


(defun start ()
  (init-data)
  (gamekit:start 'mogeo)
  ;; (gamekit:bind-cursor (lambda (x y)
  ;; 			   (when *head-grabbed-p*
  ;; 			     (let ((head-position (aref *curve* 3)))
  ;; 			       (setf (gamekit:x head-position) x
  ;; 				     (gamekit:y head-position) y)))))
  (input-key))

        
;;もげお by もげぞうβ
;;TODO アイテムとobjの当たり判定
;;
(ql:quickload :trivial-gamekit)

(load "stage.lisp")
(defparameter *obj-w* 32)
(defparameter *obj-h* 32)

(defparameter *obj-w/2* (floor *obj-w* 2))
(defparameter *obj-h/2* (floor *obj-h* 2))

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
(defparameter *flower* (gamekit:vec4 0.973 0.72 0.01 1))
;; 1:地面ブロック 2:壊せるブロック 3:土管
;; 4:1コインはてな 5:きのこetcはてな 6:スターはてな 7:1UPはてな
;; 8:隠し1UPはてな 9:10コインはてな
;; a:クリボー b:ノコノコ
;; z:入れる土管 y:ゴール
(defparameter *obj-type-list*
  `(1 (,*brown* :hard-block) 2 (,*brown2* :soft-block) 3 (,*green* :dokan)
    4 (,*yellow* :1coin) 5 (,*yellow* :item) 6 (,*yellow* :star)
    7 (,*yellow* :1up) 8 (,*light-blue* :hide-1up) 9 (,*yellow* :10coin)
    a (,*kuribo* :kuribo) b (,*nokonoko* :nokonoko)
    y (,*flag* :goal) z (,*green* :enter-dokan)))

(defparameter *stroke-paint* (gamekit:vec4 0 0 0 1))
(defparameter *background-pos* (gamekit:vec2 0 0))

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
    :initarg :pos)
   (x
    :accessor x
    :initform 0
    :initarg :x)
   (x2
    :accessor x2
    :initform 0
    :initarg :x2)
   (y
    :accessor y
    :initform 0
    :initarg :y)
   (y2
    :accessor y2
    :initform 0
    :initarg :y2)
   (width
    :accessor width
    :initform *obj-w*
    :initarg :width)
   (height
    :accessor height
    :initform *obj-h*
    :initarg :height)))

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
   (fall ;;落下
    :accessor fall
    :initform nil
    :initarg :fall)
   (state ;;キャラの状態 or アイテムの種類
    :accessor state
    :initform :small
    :initarg :state)
   (u-hit ;;上部当たり判定フラグかつ当たったobjを入れる
    :accessor u-hit
    :initform nil
    :initarg :u-hit)
   (d-hit ;;下部当たり判定フラグ
    :accessor d-hit
    :initform nil
    :initarg :d-hit)
   (r-hit ;;右部当たり判定フラグ
    :accessor r-hit
    :initform nil
    :initarg :r-hit)
   (l-hit ;;左部当たり判定フラグ
    :accessor l-hit
    :initform nil
    :initarg :l-hit)))

(defclass player (chara)
  ((field-w-max
   :accessor field-w-max
   :initform 0
   :initarg :field-w-max)
   (scroll
    :accessor scroll
    :initform 0
    :initarg :scroll)
   (muteki-time
    :accessor muteki-time
    :initform 0
    :initarg :muteki-time)
   (move-obj ;;ステージ上に出現したアイテムとか敵
    :accessor move-obj
    :initform nil
    :initarg :move-obj)))

(gamekit:defgame mogeo () ()
  (:viewport-width +screen-w+)
  (:viewport-height +screen-h+)
  (:viewport-title "スーパーもげおワールド"))

(defun rect-center-x (pos)
  (+ (gamekit:x pos) *obj-w/2*)) 

;;ステージデータ生成
(defun create-field (field-obj p)
  (with-slots (field-w-max move-obj) p
    (let* ((tate (length field-obj))
           (yoko (length (car field-obj)))
           (arr (make-array (list tate yoko) :initial-contents field-obj))
           (obj-list nil))
      (setf field-w-max yoko)
      (loop for y from (1- tate) downto 0
            do (loop for x below yoko
                     do (let ((obj-type (aref arr y x)))
                          (when (not (eq obj-type 0))
                            (let* ((obj-x (* x 32)) (obj-x2 (+ obj-x *obj-w*))
				   (obj-y (* (- (1- tate) y) 32)) (obj-y2 (+ obj-y *obj-h*))
				   (obj-pos (gamekit:vec2 obj-x obj-y))
				   (hoge (getf *obj-type-list* obj-type))
				   (color (car hoge)) (name (cadr hoge)))
			      (cond
				((or (integerp obj-type)
				     (eq 'y obj-type))
				 (push
				  (make-instance 'obj :pos obj-pos :x obj-x :x2 obj-x2
						      :y obj-y :y2 obj-y2
						      :width *obj-w* :height *obj-h*
						      :color color
						      :obj-type name)
				  obj-list))
				(t
				 (push
				  (make-instance 'chara :pos (gamekit:vec2 obj-x obj-y)
							:lastpos (gamekit:vec2 obj-x obj-y)
							:x obj-x :x2 obj-x2
							:y obj-y :y2 obj-y2
							:width *obj-w* :height *obj-h*
							:vx -1 :vy 0 :fall nil :jump nil
							:color color
							:obj-type name)
				  move-obj))))))))
      (make-array (length obj-list) :initial-contents obj-list))))

(defun init-data ()
  (setf *p* (make-instance 'player :pos (gamekit:vec2 0 64) :lastpos (gamekit:vec2 0 64)
				   :x 0 :x2 *obj-w* :y 64 :y2 (+ 64 *obj-h*)
				   :width *obj-w* :height *obj-h*
				   :vx 2 :vy 0)
        *field* (create-field *stage1-1* *p*)))

;;キー入力
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

;;描画
(defun draw-field ()
  (with-slots (scroll) *p*
    (loop :for obj across *field*
          :do (with-slots (pos x x2 y y2 color obj-type width height) obj
                (when (and (>= x2 scroll) ;;画面範囲内かどうか
			   (>= (+ scroll +screen-w+) x))
                  (let ((new-pos (gamekit:vec2 (- x scroll) y)))
                    (gamekit:draw-rect new-pos width height :fill-paint color
							    :stroke-paint *stroke-paint*)))))))

;;背景表示 
(defun draw-background ()
  (gamekit:draw-rect *background-pos* +screen-w+ +screen-h+ :fill-paint *light-blue*))

;;プレイヤー描画
(defun draw-player ()
  (with-slots (pos x y scroll width height muteki-time) *p*
    (let ((new-pos (gamekit:vec2 (- x scroll) y)))
      (if (> muteki-time 0)
	  (when (not (zerop (mod muteki-time 5)))
	    (gamekit:draw-rect new-pos width height :fill-paint *red*))
	  (gamekit:draw-rect new-pos width height :fill-paint *red*)))))

;;アイテムとか敵描画
(defun draw-move-obj ()
  (with-slots (move-obj scroll) *p*
    (dolist (obj move-obj)
      (with-slots (pos x y color width height) obj
        (let ((new-pos (gamekit:vec2 (- x scroll) y)))
          (gamekit:draw-rect new-pos width height :fill-paint color
						  :stroke-paint *stroke-paint*))))))

;;座標を表示するよ
(defun draw-debug ()
  (with-slots (left right z) *keystate*
    (with-slots (pos vx vy x y lastpos muteki-time) *p*
      (gamekit:draw-text (format nil "player-x:~d player-y:~d vx:~d vy:~d muteki:~d" x y vx vy muteki-time)
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


;;横にしか動かない
(defun kuribo-move (obj)
  (with-slots (pos x x2 y y2 vx lastpos fall jump vy width height) obj
    (setf (gamekit:x lastpos) x)
    (incf x vx) ;;x方向
    (when (or fall jump) ;;落下orジャンプ
      (setf vy -0.5))
    (let ((temp y)) ;;y方向
      (incf y (+ (- y (gamekit:y lastpos)) vy))
      (setf (gamekit:y lastpos) temp
	    x2 (+ x width)
	    y2 (+ y height)))))

;;敵とアイテムが動くよ
(defun update-move-obj ()
  (with-slots (move-obj) *p*
    (dolist (obj move-obj)
      (with-slots (obj-type) obj
	(case obj-type
	  ((:kuribo :nokonoko :kinoko)
	   (kuribo-move obj)))))))
	     


;;プレーヤーが動くよ🧢👨
(defun update-player ()
  (with-slots (pos x x2 y y2 vx vy lastpos jump fall scroll width height muteki-time state) *p*
    (setf (gamekit:x lastpos) x)
    (when (left *keystate*)
      (decf x vx))
    (when (right *keystate*)
      (incf x vx))
    (if (and (x *keystate*) (null jump) (null fall))
	(setf jump t
	      vy 18)
	(when (or jump fall)
	  (setf vy -1)))
    (when (z *keystate*))
    ;;プレイヤーのy座標更新
    (let ((temp y))
      (incf y (+ (- y (gamekit:y lastpos)) vy))
      (setf (gamekit:y lastpos) temp
	    x2 (+ x width)
	    y2 (+ y height)))
    (when (> muteki-time 0)
      (decf muteki-time))
    (when (> 0 y)
      (setf state :dead))
    (when (eq state :dead)
      (init-data))))

;;     y 0 1 3 6 10 15 21 ...
;; lasty 0 0 1 3  6 10 15 ...
;;    vy 1 1 1 1  1  1  1 ...

;;画面スクロール
(defun update-scroll ()
  (with-slots (pos x scroll) *p*
    (let ((p-center (+ x *obj-w/2*)))
      (when (or (> p-center (+ scroll +screen-center-x+))
		(< p-center (+ scroll +screen-center-x+)))
	(setf scroll (max 0 (- p-center +screen-center-x+)))))))

;;obj1とobj2の当たり判定の判定
(defun obj-hit-p (obj1 obj2)
  (with-slots (u-hit d-hit r-hit l-hit) obj1
    (let* ((obj1-x (x obj1)) (obj1-x2 (x2 obj1))
	   (obj1-y (y obj1)) (obj1-y2 (y2 obj1))
	   (obj1-center (+ obj1-x *obj-w/2*))
	   (obj2-x (x obj2)) (obj2-x2 (x2 obj2))
	   (obj2-y (y obj2)) (obj2-y2 (y2 obj2)))
      (cond
	;;プレイヤーの下側とobjの上側 （プレイヤーの下側がオブジェクトの中にめり込んでいたらプレイヤーをオブジェクトの上に持ち上げる）
	((and (or (>= obj2-x2 (+ obj1-x 10) obj2-x)
		  (>= obj2-x2 (- obj1-x2 10) obj2-x))
	      (>= obj2-y2 obj1-y) (< obj2-y2 obj1-y2)
	      (< obj2-y obj1-y)) ;;(< obj2-y obj1-y2)) ;(< obj-y p-y)ならばかならず(< obj-y p-y2)
	 ;;(debug-format "~A (~A,~A) SITA HOSEI!" (get-universal-time) obj-x obj-y)
	 :bot-hit) ;; ここまでよんだ

	;;プレイヤーの上側とobjの下側当たり判定
	((and (or (>= obj2-x2 (+ obj1-x 10) obj2-x)
		  (>= obj2-x2 (- obj1-x2 10) obj2-x))
	      (> obj2-y2 obj1-y) (> obj2-y2 obj1-y2)
	      (> obj2-y obj1-y) (< obj2-y obj1-y2)) ;; 今ここを読んでいます ha2ne2
	 ;;(debug-format "~A (~A,~A) (~A,~A) UE HOSEI!" (get-universal-time) obj-x obj-y p-x p-y)
	 :top-hit)
	;; 今からここ読みます。 6とは何でしょうか！

	;;プレイヤーの左側とobjの右側 
	((and (or (> obj2-y2 (+ obj1-y 6) obj2-y)) ;;(> obj-y2 (- p-y2 6) obj-y))
	      (> obj2-x2 obj1-x) (< obj2-x2 obj1-x2)
	      (< obj2-x obj1-x) (< obj2-x obj1-x2))
	 ;;(debug-format "~A (~A,~A) (~A,~A) HIDARI HOSEI!" (get-universal-time) obj-x obj-y p-x p-y)
	 :left-hit)
	;;プレイヤーの右側とobjの左側
	((and (or (> obj2-y2 (+ obj1-y 6) obj2-y)) ;;(> obj-y2 (- p-y2 6) obj-y))
	      (> obj2-x2 obj1-x) (> obj2-x2 obj1-x2)
	      (> obj2-x obj1-x) (< obj2-x obj1-x2))
	 ;;(debug-format "~A (~A,~A) (~A,~A) MIGI HOSEI!" (get-universal-time) obj-x obj-y p-x p-y)
	 :right-hit)))))

;; プレイヤーとブロックがあたったら
(defun hit-player-block (obj)
  (with-slots (obj-type color x x2 y y2 pos height) obj
    (with-slots (state move-obj) *p*
      (case obj-type
	(:soft-block ;;壊れるブロック
	 (setf *field* (remove obj *field* :test #'equal)))
	(:item ;;アイテムが出るハテナブロック
	 (setf obj-type :hard-block
	       color *brown*)
	 (case state ;;プレイヤーの状態で出るアイテム変わる
	   (:small (push (make-instance 'chara :vx 1 :vy 0 :state :kinoko :color *kinoko* :fall nil
					       :width *obj-w* :height *obj-h*
					       :x x :x2 x2 :y y2 :y2 (+ y2 *obj-h*) 
					       :pos (gamekit:vec2 x y2)
					       :obj-type :kinoko
					       :lastpos (gamekit:vec2 x y2))
			 move-obj))
	   (:big (push (make-instance 'chara :vx 0 :vy 0 :state :flower :color *flower* :fall nil
					     :width *obj-w* :height *obj-h*
					     :x x :x2 x2 :y y2 :y2 (+ y2 *obj-h*) 
					     :pos (gamekit:vec2 x y2)
					     :obj-type :flower
					     :lastpos (gamekit:vec2 x y2))
		       move-obj))))))))

;;出現してるアイテムと障害物の当たり判定


;;出現してるアイテムとプレイヤーの当たり判定
(defun hit-player-move-obj ()
  (with-slots (move-obj state height muteki-time d-hit vy jump fall) *p*
    (dolist (obj move-obj)
      (let ((hit-dir (obj-hit-p *p* obj)))
	(when hit-dir
	  (case (obj-type obj)
	    ((:kuribo :nokonoko)
	     (if (eq hit-dir :bot-hit) ;;踏んづけてたら
		 (setf move-obj (remove obj move-obj :test #'equal)
		       d-hit obj)
		 (cond
		   ((and (= muteki-time 0)
			 (eq state :small))
		    (setf state :dead))
		   ((not (eq state :small))
		    (setf state :small
			  muteki-time 100
			  height *obj-h*)))))
	    (:kinoko (setf state :big
			   height (* *obj-h* 2)
			   move-obj (remove obj move-obj :test #'equalp)))
	    (:flower (setf state :fire
			   move-obj (remove obj move-obj :test #'equalp)))
	    (:star   (setf state :muteki
			   move-obj (remove obj move-obj :test #'equalp)))))))))

;;obj1とobj2がぶつかってたらobj2を保存
(defun set-hit-obj (obj1 obj2)
  (with-slots (u-hit d-hit r-hit l-hit) obj1
    (case (obj-hit-p obj1 obj2)
      (:bot-hit (setf d-hit obj2))
      (:top-hit (setf u-hit obj2))
      (:left-hit (setf l-hit obj2))
      (:right-hit (setf r-hit obj2)))))

;;動くobjの位置補正
(defun position-hosei (obj)
  (with-slots (x x2 y y2 height width vy vx lastpos u-hit d-hit l-hit r-hit fall jump) obj
    (when u-hit
      (setf y (- (y u-hit) height)
	    (gamekit:y lastpos) (- (y u-hit) height))
      (when (eq (type-of obj) 'player) ;;プレイヤーの上部とブロックの下部があたったら
	 (hit-player-block u-hit)))
    (when (and l-hit
	       (or (and d-hit (null fall))
		   (and fall (null d-hit))))
      (setf (gamekit:x lastpos) (x2 l-hit)
	    x (x2 l-hit))
      (when (eq (type-of obj) 'chara) ;;プレイヤー以外
	(when (> 0 vx)
	  (setf vx (- vx)))))
    (when (and r-hit
	       (or (and d-hit (null fall))
		   (and fall (null d-hit))))
      (setf (gamekit:x lastpos) (- (x r-hit) width)
	    x (- (x r-hit) width))
      (when (eq (type-of obj) 'chara)
	(when (> vx 0)
	  (setf vx (- vx)))))
    (if d-hit
	(case (obj-type d-hit) 
	  ((:kuribo :nokonoko) ;;敵を踏んづけてたら跳ねる
	   (setf vy 14 jump nil fall nil
		 y (y2 d-hit)
		 (gamekit:y (lastpos obj)) (y2 d-hit)))
	  (otherwise
	   (setf vy 0 jump nil fall nil
		 y (y2 d-hit)
		 (gamekit:y (lastpos obj)) (y2 d-hit))))
	(setf fall t))
    (setf u-hit nil d-hit nil l-hit nil r-hit nil)))

  
;;プレイヤーとかの当たり判定 (アイテム 敵 障害物)
(defun hit-player-objects ()
  (with-slots (scroll vy jump fall move-obj state width height) *p*
    (hit-player-move-obj) ;;プレイヤーとアイテムや敵の当たり判定
    ;;動くobjと動かないobjとの当たり判定
    (loop :for obj across *field*
	  :for i from 0
	  :do (set-hit-obj *p* obj)
	      (dolist (mobj move-obj)
		(set-hit-obj mobj obj)))
    (position-hosei *p*)
    (dolist (mobj move-obj)
      (position-hosei mobj))))
;; 🐈🐈🐈🐈

;;ゲームループ？
(defmethod gamekit:act ((app mogeo))
  (update-player)
  (update-move-obj)
  (update-scroll)
  (hit-player-objects)
  (sleep 0.01))

;;描画
(defmethod gamekit:draw ((app mogeo))
  (draw-background)
  (draw-field)
  (draw-player)
  (draw-move-obj)
  (draw-debug)
  (draw-console))


(defun start ()
  (init-data)
  (setf *keystate* (make-instance 'keystate))
  (gamekit:start 'mogeo)
  ;; (gamekit:bind-cursor (lambda (x y)
  ;; 			   (when *head-grabbed-p*
  ;; 			     (let ((head-position (aref *curve* 3)))
  ;; 			       (setf (gamekit:x head-position) x
  ;; 				     (gamekit:y head-position) y)))))
  (input-key))

        

;;もげお by もげぞうβ
;;TODO
;;

(load "stage.lisp")

;;基本サイズ
(defparameter *obj-w* 32)
(defparameter *obj-h* 32)

(defparameter *obj-w/2* (floor *obj-w* 2))
(defparameter *obj-h/2* (floor *obj-h* 2))

;;
(defparameter *tate* 20)
(defparameter *yoko* 30)

(defparameter *fire-r* 10)
(defparameter *fire-2r* (* *fire-r* 2))
(defparameter *fire-vx* 4)
(defparameter *koura-r* 16)
(defparameter *koura-2r* (* *koura-r* 2))

(defparameter *jump-power* 16)
(defparameter *mogeo-ax+* 0.08) ;;キー押してるときの加速
(defparameter *mogeo-ax-* 0.04) ;;キー放したときの減速

;;画面サイズ
(defparameter +screen-w+ (* *obj-w* *yoko*))
(defparameter +screen-h+ (* *obj-h* *tate*))

(defparameter +screen-center-x+ (floor +screen-w+ 2))

(defparameter *light-blue* (gamekit:vec4 0 1 1 1))
(defparameter *yellow*     (gamekit:vec4 1 1 0 1))
(defparameter *brown*      (gamekit:vec4 0.651 0.152 0.015 1))
(defparameter *brown2*     (gamekit:vec4 0.751 0.252 0.015 1))
(defparameter *red*        (gamekit:vec4 1 0 0 1))
(defparameter *green*      (gamekit:vec4 0 0.5 0 1))
(defparameter *kuribo*     (gamekit:vec4 0.91 0.376 0.063 1))
(defparameter *nokonoko*   (gamekit:vec4 0 0.678 0 1))
(defparameter *flag*       (gamekit:vec4 0.722 0.973 0.094 1))
(defparameter *kinoko*     (gamekit:vec4 0.973 0.22 0 1))
(defparameter *flower*     (gamekit:vec4 0.973 0.72 0.01 1))

;; 1:地面ブロック 2:壊せるブロック 3:土管
;; 4:1コインはてな 5:きのこetcはてな 6:スターはてな 7:1UPはてな
;; 8:隠し1UPはてな 9:10コインはてな
;; a:クリボー b:ノコノコ
;; z:入れる土管 y:ゴール
(defparameter *obj-type-list*
  `(1 (,*brown*  :hard-block) 2 (,*brown2*     :soft-block)   3 (,*green*  :dokan)
    4 (,*yellow* :1coin)      5 (,*yellow*     :item)         6 (,*yellow* :star)
    7 (,*yellow* :1up)        8 (,*light-blue* :hide-1up)     9 (,*yellow* :10coin)
    a (,*kuribo* :kuribo)     b (,*nokonoko*   :nokonoko)
    y (,*flag*   :goal)       z (,*green*      :enter-dokan)))

(defparameter *stroke-paint* (gamekit:vec4 0 0 0 1))
(defparameter *background-pos* (gamekit:vec2 0 0))

(defparameter *p* nil) ;;player
(defparameter *field* nil)
(defparameter *keystate* nil)



(defclass keystate ()
  ((left  :accessor left  :initform nil :initarg :left)
   (right :accessor right :initform nil :initarg :right)
   (down  :accessor down  :initform nil :initarg :down)
   (x     :accessor x     :initform nil :initarg :x)
   (z     :accessor z     :initform nil :initarg :z)))

(defclass nanka () ;;共通で使いそうなの
  ((color    :accessor color    :initform nil :initarg :color)
   (obj-type :accessor obj-type :initform 0   :initarg :obj-type)
   (u-hit    :accessor u-hit    :initform nil :initarg :u-hit)   ;;上部当たり判定フラグかつ当たったobjを入れる
   (d-hit    :accessor d-hit    :initform nil :initarg :d-hit)   ;;下部
   (r-hit    :accessor r-hit    :initform nil :initarg :r-hit)   ;;右部
   (l-hit    :accessor l-hit    :initform nil :initarg :l-hit))) ;;左部

(defclass ball (nanka)
  ((px     :accessor px     :initform 0 :initarg :px)     ;;中心x座標
   (py     :accessor py     :initform 0 :initarg :py)     ;;中心y座標
   (lastpy :accessor lastpy :initform 0 :initarg :lastpy) ;;一個前のy座標
   (vx     :accessor vx     :initform 0 :initarg :vx)     ;;x方向スピード
   (vy     :accessor vy     :initform 0 :initarg :vy)     ;;y方向スピード
   (r      :accessor r      :initform 0 :initarg :r)))    ;;半径

(defclass obj (nanka)
  ((pos    :accessor pos    :initform (gamekit:vec2 0 0) :initarg :pos)      ;;いらないかも
   (x      :accessor x      :initform 0                  :initarg :x)        ;;左端
   (x2     :accessor x2     :initform 0                  :initarg :x2)       ;;右端
   (y      :accessor y      :initform 0                  :initarg :y)        ;;下端
   (y2     :accessor y2     :initform 0                  :initarg :y2)       ;;上端
   (width  :accessor width  :initform *obj-w*            :initarg :width)    ;;幅
   (height :accessor height :initform *obj-h*            :initarg :height))) ;;高さ

(defclass chara (obj)
  ((vx      :accessor vx      :initform 0      :initarg :vx)      ;;x方向スピード
   (vy      :accessor vy      :initform 0      :initarg :vy)      ;;y方向スピード
   (dir     :accessor dor     :initform :right :initarg :dir)     ;;向き
   (lasty   :accessor lastt   :initform 0      :initarg :lasty)   ;;一個前のy
   (jump    :accessor jump    :initform nil    :initarg :jump)    ;;ジャンプ中フラグ
   (fall    :accessor fall    :initform nil    :initarg :fall)    ;;落下中フラグ
   (r       :accessor r       :initform 0      :initarg :r)       ;;半径
   (state   :accessor state   :initform :small :initarg :state))) ;;プレイヤー:今の状態 (:small :big :fire) 他obj:動いているかどうか


(defclass player (chara)
  ((field-w-max :accessor field-w-max :initform 0   :initarg :field-w-max) ;;ステージの幅
   (scroll      :accessor scroll      :initform 0   :initarg :scroll)      ;;スクロール
   (fire        :accessor fire        :initform nil :initarg :fire)        ;;ファイアーボールリスト
   (muteki-time :accessor muteki-time :initform 0   :initarg :muteki-time) ;;敵と接触したときの無敵時間
   (fire-time   :accessor fire-time   :initform 0   :initarg :fire-time)   ;;ファイアーボール打てる間隔
   (items       :accessor items       :initform nil :initarg :items)       ;;アイテムリスト
   (koura       :accessor koura       :initform nil :initarg :koura)       ;;ノコノコの甲羅リスト
   (enemies     :accessor enemies     :initform nil :initarg :enemies)))   ;;敵リスト

(gamekit:defgame mogeo () ()
  (:viewport-width +screen-w+)
  (:viewport-height +screen-h+)
  (:viewport-title "スーパーもげおワールド"))

(defun rect-center-x (x)
  (+ x *obj-w/2*))

;;ステージデータ生成
(defun create-field (field-obj p)
  (with-slots (field-w-max enemies) p
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
                                   (eq 'y obj-type) (eq 'z obj-type))
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
						      :lasty obj-y
						      :x obj-x :x2 obj-x2
						      :y obj-y :y2 obj-y2
						      :width *obj-w* :height *obj-h*
						      :vx -1 :vy 0 :fall nil :jump nil
						      :color color :state nil
						      :obj-type name)
				enemies))))))))
      (make-array (length obj-list) :initial-contents obj-list))))

(defun init-data ()
  (setf *p* (make-instance 'player :pos (gamekit:vec2 0 64) :lasty 64
             :x 0 :x2 *obj-w* :y 64 :y2 (+ 64 *obj-h*)
             :width *obj-w* :height *obj-h* :color *red*
             :vx 2 :vy 0)
        *field* (create-field *stage1-1* *p*)))

;;キー入力
(defun input-key ()
  (with-slots (left right down z x) *keystate*
    (gamekit:bind-button :left :pressed
       (lambda () (setf left t)))
    (gamekit:bind-button :left :released
       (lambda () (setf left nil)))
    (gamekit:bind-button :right :pressed
       (lambda () (setf right t)))
    (gamekit:bind-button :right :released
       (lambda () (setf right nil)))
    (gamekit:bind-button :down :pressed
       (lambda () (setf down t)))
    (gamekit:bind-button :down :released
       (lambda () (setf down nil)))
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

;;矩形オブジェクト描画
(defun draw-rect-obj (obj scroll)
  (with-slots (pos x x2 y color width height obj-type r) obj
    (when (and (>= x2 scroll) ;;画面範囲内かどうか
               (>= (+ scroll +screen-w+) x))
      (case obj-type
        ((:fire :koura)
         (let ((new-pos (gamekit:vec2 (- (+ x r) scroll) (+ y r))))
           (gamekit:draw-circle new-pos r :fill-paint color
                                :stroke-paint *stroke-paint*)))
        (t
          (let ((new-pos (gamekit:vec2 (- x scroll) y)))
            (gamekit:draw-rect new-pos width height :fill-paint color
                                                    :stroke-paint *stroke-paint*)))))))

;;円形obj描画
(defun draw-circle-obj (obj scroll)
  (with-slots (px py r color) obj
    (when (and (>= (+ px r) scroll) ;;画面範囲内かどうか
               (>= (+ scroll +screen-w+) (- px r)))
      (let ((new-pos (gamekit:vec2 (- px scroll) py)))
	(gamekit:draw-circle new-pos r :fill-paint color
			     :stroke-paint *stroke-paint*)))))

;;描画　ステージの動かない矩形オブジェクト描画
(defun draw-field ()
  (with-slots (scroll) *p*
    (loop :for obj across *field*
          :do (draw-rect-obj obj scroll))))

;;背景表示
(defun draw-background ()
  (gamekit:draw-rect *background-pos* +screen-w+ +screen-h+ :fill-paint *light-blue*))

;;プレイヤー描画
(defun draw-player ()
  (with-slots (x y scroll width height muteki-time color) *p*
    (let ((new-pos (gamekit:vec2 (- x scroll) y)))
      (if (> muteki-time 0) ;;敵と接触したら点滅させる
       (when (not (zerop (mod muteki-time 5)))
          (gamekit:draw-rect new-pos width height :fill-paint color))
       (gamekit:draw-rect new-pos width height :fill-paint color)))))



;;アイテムとか敵描画
(defun draw-move-objs ()
  (with-slots (fire enemies koura items scroll) *p*
    ;;(dolist (f fire)
    ;;  (draw-rect-obj f scroll))
    (mapc (lambda (f) (draw-rect-obj f scroll)) fire)
    (dolist (enemy enemies)
      (draw-rect-obj enemy scroll))
    (dolist (item items)
      (draw-rect-obj item scroll))
    (dolist (k koura)
      (draw-rect-obj k scroll))))


;;座標を表示するよ
(defun draw-debug ()
  (with-slots (left right z) *keystate*
    (with-slots (pos vx vy x y muteki-time koura scroll) *p*
      (gamekit:draw-text (format nil "player-x:~d player-y:~d vx:~d vy:~d muteki:~d scroll:~d" x y vx vy muteki-time scroll)
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
  (with-slots (x x2 y y2 vx lasty vy width height) obj
    ;;(setf (gamekit:x lastpos) x)
    (incf x vx) ;;x方向
    (let ((temp y)) ;;y方向
      (incf y (+ (- y lasty) vy))
      (setf lasty temp
            x2 (+ x width)
            y2 (+ y height)))))

;;敵とアイテムが動くよ
(defun update-move-obj (scroll slot-name)
  (dolist (obj (slot-value *p* slot-name))
    (with-slots (x x2 y obj-type state) obj
      (when (and (null state)
                 (>= x2 scroll) (>= (+ scroll +screen-w+) x) (>= +screen-h+ y 0)) ;;画面範囲内かどうか
        ;;一度画面内に入ったら動かす
        (setf state :move))
      (when state ;;画面内に入るまで動かない
        (kuribo-move obj)
        (when (> 0 y) ;;画面下に落ちたら消す
          (setf (slot-value *p* slot-name) (remove obj (slot-value *p* slot-name) :test #'equal)))))))

(defun update-all-move-obj ()
  (with-slots (enemies items koura scroll) *p*
    (update-move-obj scroll 'enemies)
    (update-move-obj scroll 'items)
    (update-move-obj scroll 'koura)))

;;ファイア動くよ
(defun update-fire ()
  (with-slots (fire scroll) *p*
    (dolist (f fire)
      (with-slots (x x2 y y2 lasty width height vx vy) f
       (cond
          ((and (>= x2 scroll) ;;画面範囲内かどうか
                (>= (+ scroll +screen-w+) x)
                (>= +screen-h+ y 0))
           (incf x vx) ;;x方向
           (let ((temp y)) ;;y方向
             (incf y (+ (- y lasty) vy))
             (setf lasty temp
                   x2 (+ x width)
                   y2 (+ y height))))
          (t ;;画面外に出たら消す
            (setf fire (remove f fire :test #'equal))))))))

;;ファイアーボール生成
(defun make-fire (x x2 y y2 lasty vx)
  (make-instance 'chara :vx vx :vy 0 :color *red*
			:x x :x2 x2 :y y :y2 y2
			:lasty lasty :width *fire-2r* :height *fire-2r*
			:r *fire-r* :state nil
			:obj-type :fire))

;;プレーヤーが動くよ🧢👨
(defun update-player ()
  (with-slots (dir x x2 y y2 vx vy lasty width height muteki-time fire-time state fire) *p*
    (let ((maxvx 2))
      (when (z *keystate*) ;;zキー
	(setf maxvx 4) ;;ダッシュ仮
	(when (and (eq state :fire) ;;ファイアマリオ状態
		   (= fire-time 0))
	  (let ((fireball
		  (if (eq dir :right)
		      (make-fire x2 (+ x2 *fire-2r*) (- y2 *fire-2r*) y2 (- y2 *fire-2r*) *fire-vx*)
		      (make-fire (- x *fire-2r*) x (- y2 *fire-2r*) y2 (- y2 *fire-2r*) (- *fire-vx*)))))
	    (push fireball fire))
	  (setf fire-time 50)))
      (when (left *keystate*) ;;左キー
	(setf dir :left) ;;左向きにセット
	(cond ;; maxvxまで加速or減速
	  ((> vx (- maxvx)) (decf vx *mogeo-ax+*))
	  ((> (- maxvx) vx) (incf vx *mogeo-ax+*))))
      (when (right *keystate*) ;;右キー
	(setf dir :right) ;;右向きにセット
	(cond ;; maxvxまで加速or減速
	  ((> maxvx vx) (incf vx *mogeo-ax+*))
	  ((> vx maxvx) (decf vx *mogeo-ax+*))))
      (when (and (null (left *keystate*)) ;;左右キーどちらも押してなかったら
		 (null (right *keystate*)))
	(cond ;;徐々に減速
	  ((> vx 0) (setf vx (max 0 (- vx *mogeo-ax-*))))
	  ((< vx 0) (setf vx (min 0 (+ vx *mogeo-ax-*))))))
      ;;x座標更新
      (incf x vx)
      ;;下キー
      (if (down *keystate*) 
	  (when (not (eq state :small))
	    (setf height *obj-h*
		  y2 (+ y height)))
	  (when (and (not (eq state :small))
		     (> (* *obj-h* 2) height))
	    (setf height (* *obj-h* 2)
		  y2 (+ y height))))
      ;;xキー
      (if (and (x *keystate*) (= vy 0))
	  (setf vy (+ *jump-power* (abs vx))))
    
      ;;プレイヤーのy座標更新
      (let ((temp y))
	(incf y (+ (- y lasty) vy))
	(setf lasty temp
	      x2 (+ x width)
	      y2 (+ y height)))
      (when (> muteki-time 0) ;;無敵中だったら無敵時間減らす
	(decf muteki-time))
      (when (> fire-time 0) ;;ファイアーのクールタイム
	(decf fire-time))
      (when (> 0 y) ;;落下して死亡
	(setf state :dead))
      (when (eq state :dead) ;;死亡してたらリセット
	(init-data)))))

;;画面スクロール
(defun update-scroll ()
  (with-slots (pos x scroll) *p*
    (let ((p-center (+ x *obj-w/2*)))
      (when (or (> p-center (+ scroll +screen-center-x+))
             (< p-center (+ scroll +screen-center-x+)))
       (setf scroll (max 0 (- p-center +screen-center-x+)))))))

;;ファイアーボールと四角の当たり判定 動かないオブジェクトとの当たり判定ではマルとして扱う
(defun fire-rect-hit-p (maru rectan)
  (with-slots (vx vy lasty) maru
    (with-slots (x x2 y y2) rectan
      (let* ((r *fire-r*) (px (+ (x maru) r)) (py (+ (y maru) r))
             (b1x (- x r)) (b2x (+ x2 r))
             (b1y (- y r)) (b2y (+ y2 r)))
       (cond
    ;;ボールの下部とブロックの上部にあたった
          ((or (>= r (sqrt (+ (expt (- px x) 2) (expt (- py y2) 2))))
               (>= r (sqrt (+ (expt (- px x2) 2) (expt (- py y2) 2))))
               (and (>= x2 px x) (> lasty y2)
                    (>= b2y py y)))
           (debug-format "moge")
           :bot-hit)
    ;;ボールの上部とブロックの下部にあたった
          ((and (>= x2 px x) (< lasty y)
            (>= y py b1y))
           :top-hit)
    ;;ボールの左とブロックの右にあたった
          ((and (>= b2x px x2) (> 0 vx)
                               (>= y2 py y))
           (debug-format "left")
           :left-hit)
    ;;ボールの右とブロックの左にあたった
          ((and (>= x2 px b1x) (> vx 0)
                               (>= y2 py y))
           (debug-format "right")
           :right-hit)
    ;;当たらなかった
          (t nil))))))

;;矩形同士のobj1とobj2の当たり判定の判定
(defun obj-hit-p (obj1 obj2)
  (with-slots (u-hit d-hit r-hit l-hit) obj1
    (let* ((obj1-x (x obj1)) (obj1-x2 (x2 obj1))
           (obj1-y (y obj1)) (obj1-y2 (y2 obj1))
     ;;(obj1-center (+ obj1-x *obj-w/2*))
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
    (with-slots (state items) *p*
      (case obj-type
       (:soft-block ;;壊れるブロック
         (when (not (eq state :small))
            (setf *field* (remove obj *field* :test #'equal))))
       (:item ;;アイテムが出るハテナブロック
         (setf obj-type :hard-block
                color *brown*)
         (case state ;;プレイヤーの状態で出るアイテム変わる
            (:small (push (make-instance 'chara :vx 1 :vy 0 :color *kinoko* :fall nil
                           :width *obj-w* :height *obj-h*
                           :x x :x2 x2 :y y2 :y2 (+ y2 *obj-h*)
                           :pos (gamekit:vec2 x y2)
                           :obj-type :kinoko :state nil
                           :lasty y2)
                     items))
            (:big (push (make-instance 'chara :vx 0 :vy 0 :color *flower* :fall nil
                         :width *obj-w* :height *obj-h*
                         :x x :x2 x2 :y y2 :y2 (+ y2 *obj-h*)
                         :pos (gamekit:vec2 x y2)
                         :obj-type :flower :state nil
                         :lasty y2)
                   items))))))))

;;ファイアーと敵の当たり判定
(defun hit-fire-enemies ()
  (with-slots (fire enemies) *p*
    (dolist (f fire)
      (dolist (enemy enemies)
        (with-slots (obj-type) enemy
          (case obj-type
            ((:kuribo :nokonoko)
             (when (fire-rect-hit-p f enemy)
               (setf fire (remove f fire :test #'equal)
                     enemies (remove enemy enemies :test #'equal))
               (return)))))))))

;;甲羅と敵の当たり判定
(defun hit-koura-enemies ()
  (with-slots (koura enemies) *p*
    (dolist (k koura)
      (when (/= (vx k) 0)
        (dolist (enemy enemies)
          (with-slots (state vy) enemy
            (when (and (obj-hit-p k enemy) (not (eq state :dead)))
              (setf state :dead ;;死んだ
                    vy 12) ;;ちょっとジャンプさせる
              (if (> (vx k) 0)
                  (setf (vx enemy) (abs (vx enemy)))
                  (setf (vx enemy) (- (abs (vx enemy))))))))))))
            ;;(setf enemies (remove enemy enemies :test #'equal))))))))



;;プレイヤーと敵の当たり判定
(defun hit-player-enemies ()
  (with-slots (enemies items koura state height muteki-time d-hit color) *p*
    (dolist (obj enemies)
      (let ((hit-dir (obj-hit-p *p* obj)))
        (when hit-dir
          (case (obj-type obj)
            ((:kuribo :nokonoko)
             (if (eq hit-dir :bot-hit) ;;踏んづけてたら
                 (with-slots (x y y2) obj
                   (setf enemies (remove obj enemies :test #'equal)
                         d-hit obj
                         (y *p*) (1+ y2))
                   (when (eq (obj-type obj) :nokonoko)
                     (push (make-instance 'chara :vx 0 :vy 0 :x x :x2 (+ x *koura-2r*) :y y :y2 (+ y *koura-2r*)
                                            :width *koura-2r* :height *koura-2r* :lasty y
                                            :r *koura-r* :state nil
                                            :color *nokonoko* :obj-type :koura)
                           koura)
                     (return)))
                 (cond
                   ((and (= muteki-time 0) ;;無敵ではなくチビ状態だったら死亡
                         (eq state :small))
                    (setf state :dead))
                   ((not (eq state :small)) ;;チビ以外のときはチビ状態にして無敵状態にする
                    (setf state :small
                          color *red*
                          muteki-time 100
                          height *obj-h*)))))))))))

;;プレイヤーと甲羅の当たり判定
(defun hit-player-koura ()
  (with-slots (koura d-hit state muteki-time height color) *p*
    (dolist (k koura)
      (with-slots (vx) k
        (let ((dir (obj-hit-p *p* k)))
          (cond
            ((and (= vx 0) dir) ;;甲羅が止まっていてかつプレイヤーと当たってる
             (if (>= (rect-center-x (x *p*)) (rect-center-x (x k)))
                 (setf (vx k) -3
                       (x k) (- (x *p*) (width k)))
                 (setf vx 3
                       (x k) (x2 *p*)))
             (debug-format "vx:~d vy: ~d" (vx k) (vy k)))
            ((and (/= vx 0) (eq dir :bot-hit)) ;;甲羅が動いているときにプレイヤーが踏んづける
             (setf vx 0 ;;甲羅のvx
                   d-hit k))
            ((and (/= vx 0) dir (= muteki-time 0))
             (cond
               ((eq state :small)
                (setf state :dead))
               ((not (eq state :small)) ;;チビ以外のときはチビ状態にして無敵状態にする
                (setf state :small
                      color *red*
                      muteki-time 100
                      height *obj-h*))))))))))

;;プレイヤーとアイテムの当たり判定
(defun hit-player-items ()
  (with-slots (items state height color y y2) *p*
    (dolist (item items)
      (when (obj-hit-p *p* item)
        (with-slots (obj-type) item
          (case obj-type
            (:kinoko (setf state :big
                           height (* *obj-h* 2)
                           y2 (+ y height)
                           items (remove item items :test #'equalp)))
            (:flower (setf state :fire
                           height (* *obj-h* 2)
                           y2 (+ y height)
                           color *flower*
                           items (remove item items :test #'equalp)))
            (:star   (setf state :muteki
                           items (remove item items :test #'equalp)))))))))

;;obj1とobj2がぶつかってたらobj2を保存
(defun set-hit-obj (obj1 obj2)
  (with-slots (u-hit d-hit r-hit l-hit obj-type state) obj1
    (when (not (eq state :dead))
      (let ((hantei (if (eq obj-type :fire) ;;obj1がファイアーボールか四角か
                        (fire-rect-hit-p obj1 obj2)
                        (obj-hit-p obj1 obj2))))
        (case hantei
          (:bot-hit (setf d-hit obj2))
          (:top-hit (setf u-hit obj2))
          (:left-hit (setf l-hit obj2))
          (:right-hit (setf r-hit obj2)))))))


;;objsとobj2の当たり判定
(defun all-set-hit-obj (obj)
  (with-slots (fire enemies items koura) *p*
    (set-hit-obj *p* obj)
    (dolist (f fire)
      (set-hit-obj f obj))
    (dolist (item items)
      (set-hit-obj item obj))
    (dolist (enemy enemies)
      (set-hit-obj enemy obj))
    (dolist (k koura)
      (set-hit-obj k obj))))



;;ファイアーボールの位置補正と速度変更
(defun fire-position-hosei ()
  (with-slots (fire) *p*
    (dolist (f fire)
      (with-slots (x x2 y y2 width height lasty vx vy u-hit d-hit l-hit r-hit) f
        (when u-hit
          (setf vy -0.5 y (y u-hit) y2 (+ y height) lasty (y u-hit)))
        (when (and l-hit (null d-hit))
          (setf fire (remove f fire :test #'equalp)))
        (when (and r-hit (null d-hit))
          (setf fire (remove f fire :test #'equalp)))
        (when d-hit
          (setf vy 7 y (y2 d-hit) y2 (+ y height) lasty (y2 d-hit)))
        (when (null d-hit)
          (setf vy -0.5))
        (setf u-hit nil d-hit nil l-hit nil r-hit nil)))))

;;動くobjの位置補正
(defun position-hosei (obj)
  (with-slots (x x2 y y2 height width vy vx lasty u-hit d-hit l-hit r-hit state) obj
    (when u-hit ;; u-hit=ぶつかったオブジェクト
      (setf y (- (y u-hit) height)
            vy -1
            lasty (- (y u-hit) height))
      (when (eq (type-of obj) 'player) ;;プレイヤーの上部とブロックの下部があたったら
        (hit-player-block u-hit)))
    (when (and l-hit
               (or (and d-hit (= vy 0))
                   (and (/= vy 0) (null d-hit))))
      (setf x (x2 l-hit))
      (when (eq (type-of obj) 'chara) ;;プレイヤー以外だったらvxの向きかえる
        (when (> 0 vx)
          (setf vx (- vx)))))
    (when (and r-hit
               (or (and d-hit (= vy 0))
                   (and (/= vy 0) (null d-hit))))
      (setf x (- (x r-hit) width))
      (when (eq (type-of obj) 'chara)
        (when (> vx 0)
          (setf vx (- vx)))))
    (if d-hit ;;地面に設置してたり敵を踏んづけてたりしたら
        (case (obj-type d-hit)
          ((:kuribo :nokonoko :koura) ;;敵を踏んづけてたら跳ねる
           (setf vy 14
                 y (y2 d-hit)
                 lasty (y2 d-hit)))
          (otherwise
            (setf vy 0
                  y (y2 d-hit)
                  lasty (y2 d-hit))))
        (setf vy -1))
    (setf u-hit nil d-hit nil l-hit nil r-hit nil)))

;;動くものの位置修正
(defun all-position-hosei ()
  (with-slots (items enemies fire koura) *p*
    (position-hosei *p*)
    (fire-position-hosei)
    (dolist (item items)
      (position-hosei item))
    (dolist (enemy enemies)
      (position-hosei enemy))
    (dolist (k koura)
      (position-hosei k))))

;;プレイヤーとかの当たり判定 (アイテム 敵 障害物)
(defun hit-player-objects ()
  ;;(with-slots (scroll fire enemies items) *p*
  (hit-player-enemies) ;;プレイヤーと敵の当たり判定
  (hit-player-items)   ;;プレイヤーとアイテム
  (hit-player-koura)
  ;;動くobjと動かないobjとの当たり判定
  (loop :for obj across *field*
        :for i from 0
        :do (all-set-hit-obj obj))
      ;; (dolist (mobj move-obj)
      ;; 	(set-hit-obj mobj obj)))
  (all-position-hosei)) ;;ポジション補正
;; 🐈🐈🐈🐈

;;ゲームループ？
(defmethod gamekit:act ((app mogeo))
  (update-player)
  (update-all-move-obj)
  (update-fire)
  (update-scroll)
  (hit-player-objects)
  (hit-fire-enemies)
  (hit-koura-enemies)
  (sleep 0.01))

;;描画
(defmethod gamekit:draw ((app mogeo))
  (draw-background)
  (draw-field)
  (draw-player)
  (draw-move-objs)
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

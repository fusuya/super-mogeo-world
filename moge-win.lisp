;;TODO しゃがみの続き

;;(in-package moge)

;;ブロックとか
(defclass obj ()
  ((x        :accessor x        :initform 0      :initarg :x)
   (y        :accessor y        :initform 0      :initarg :y)
   (x2       :accessor x2       :initform 0      :initarg :x2)
   (y2       :accessor y2       :initform 0      :initarg :y2)
   (w        :accessor w        :initform 0      :initarg :w)
   (h        :accessor h        :initform 0      :initarg :h)
   (w/2        :accessor w/2        :initform 0      :initarg :w/2)
   (h/2        :accessor h/2        :initform 0      :initarg :h/2)
   (vx    :accessor vx    :initform 0      :initarg :vx)
   (vy    :accessor vy    :initform 0      :initarg :vy)
   (dy    :accessor dy    :initform 0      :initarg :dy)
   (obj-type :accessor obj-type :initform 0      :initarg :obj-type)
   (state    :accessor state    :initform :small :initarg :state)
   (u-hit    :accessor u-hit    :initform nil    :initarg :u-hit)   ;;上部当たり判定フラグかつ当たったobjを入れる
   (d-hit    :accessor d-hit    :initform nil    :initarg :d-hit)   ;;下部
   (r-hit    :accessor r-hit    :initform nil    :initarg :r-hit)   ;;右部
   (l-hit    :accessor l-hit    :initform nil    :initarg :l-hit) ;;左部
   (img      :accessor img      :initform nil    :initarg :img)
   (hit      :accessor hit    :initform 0      :initarg :hit)))

;;アイテムとか敵
(defclass move-obj (obj)
  ((dir   :accessor dir   :initform :right :initarg :dir)
   (lasty :accessor lasty :initform 0      :initarg :lasty)
   (nextx :accessor nextx :initform 0      :initarg :nextx)
   (nexty :accessor nexty :initform 0      :initarg :nexty)
   (walk-p     :accessor walk-p     :initform nil   :initarg :walk-p)
   (walk-c     :accessor walk-c     :initform 0   :initarg :walk-c) ;;歩行カウンター 一定値になると歩行画像更新
   (walk-state :accessor walk-state :initform 0   :initarg :walk-state) ;;
   (walk-img-type :accessor walk-img-type :initform 0   :initarg :walk-img-type) ;;画像タイプ
   (walk-func  :accessor walk-func  :initform #'+ :initarg :walk-func)))

;;プレイヤー
(defclass player (move-obj)
  ((dash-c :accessor dash-c     :initform 0   :initarg :dash-c)
   (ax     :accessor ax         :initform 1   :initarg :ax)
   (ay     :accessor ay         :initform 1   :initarg :ay)
   (maxvx  :accessor maxvx      :initform 0   :initarg :maxvx)
   (scroll :accessor scroll     :initform 0   :initarg :scroll)
   (stage  :accessor stage      :initform 0   :initarg :stage)
   (goal   :accessor goal       :initform nil :initarg :goal)
   (score   :accessor score       :initform 0 :initarg :score)
   (crouch? :accessor crouch?     :initform nil   :initarg :couch?)
   (jump?   :accessor jump?       :initform nil   :initarg :jump?)
   (timer   :accessor timer       :initform 200   :initarg :timer)
   (timer-c   :accessor timer-c       :initform 0   :initarg :timer-c)
   (coin        :accessor coin        :initform 0   :initarg :coin)        ;;コイン枚数
   (zanki       :accessor zanki       :initform 2   :initarg :zanki)       ;;残機
   (muteki-time :accessor muteki-time :initform 0   :initarg :muteki-time) ;;敵と接触したときの無敵時間
   (star-time   :accessor star-time   :initform 0   :initarg :star-time)   ;;スター無敵時間
   (fire-time   :accessor fire-time   :initform 0   :initarg :fire-time)))   ;;ファイアーボール打てる間隔))


(defclass keystate ()
  ((right :accessor right :initform nil :initarg :right)
   (left  :accessor left  :initform nil :initarg :left)
   (down  :accessor down  :initform nil :initarg :down)
   (enter :accessor enter :initform nil :initarg :enter)
   (z     :accessor z     :initform nil :initarg :z)
   (x     :accessor x     :initform nil :initarg :x)
   (key1     :accessor key1     :initform nil :initarg :key1)
   (key2     :accessor key2     :initform nil :initarg :key2)
   (key4     :accessor key4     :initform nil :initarg :key4)
   (key5     :accessor key5     :initform nil :initarg :key5)
   (key6     :accessor key6     :initform nil :initarg :key6)
   (key7     :accessor key7     :initform nil :initarg :key7)
   (key8     :accessor key8     :initform nil :initarg :key8)
   (key9     :accessor key9     :initform nil :initarg :key9)
   (key0     :accessor key0     :initform nil :initarg :key0)
   (keya     :accessor keya     :initform nil :initarg :keya)
   (keyb     :accessor keyb     :initform nil :initarg :keyb)
   ))

;;基本サイズ
(defparameter *obj-w* 32)
(defparameter *obj-h* 32)

(defparameter *w/2* (floor *obj-w* 2))
(defparameter *h/2* (floor *obj-h* 2))
(defparameter *fb-w* 16) ;;ファイアーボールの幅
(defparameter *fb-h* 16) ;;ファイアーボールの高さ
(defparameter *fb-w/2* (floor *fb-w* 2))
(defparameter *fb-h/2* (floor *fb-h* 2))

;;
(defparameter *tate* 20)
(defparameter *yoko* 30)

(defparameter *screen-w* 960)
(defparameter *screen-h* 720)
(defparameter *waku-size* 10) ;;ゲームフィールドの周りの枠太さ
(defparameter *c-rect* nil) ;;クライアント領域
(defparameter *p* nil)
(defparameter *e* nil)
(defparameter *keystate* (make-instance 'keystate))

(defparameter *client-w* (* *obj-w* *yoko*))
(defparameter *client-h* (* *obj-h* *tate*))

(defparameter *screen-center-x* 0)

(defparameter *brush* nil)
(defparameter *start* nil)
(defparameter *hmemDC* nil)
(defparameter *hbitmap* nil)


(defparameter *hogememDC* nil)
(defparameter *hogebitmap* nil)

(defparameter *mogeo-walk-right-imgs* nil)
(defparameter *mogeo-walk-left-imgs* nil)
(defparameter *mogeo-walk-imgs* nil)
(defparameter *big-mogeo-walk-imgs* nil)
(defparameter *kuribo-walk-imgs* nil)
(defparameter *nokonoko-walk-imgs* nil)
(defparameter *objects-imgs* nil)
(defparameter *mogeo-crouch-right-imgs* nil)
(defparameter *mogeo-crouch-left-imgs* nil)


(defparameter *enemies* nil)
(defparameter *items* nil)
(defparameter *fire* nil)
(defparameter *objects* nil)




(defparameter *jump-power* -16)
(defparameter *walk-speed* 2)
(defparameter *dash-speed* 5)
(defparameter *muteki-time* 80)
(defparameter *star-time* 400)
(defparameter *koura-speed* 5)
(defparameter *fire-time* 50)

(defparameter *coin-wav* "./wav/coin.wav")
(defparameter *block-hit-wav* "./wav/kabe-hit.wav")
(defparameter *fire-wav* "./wav/fire2.wav")
(defparameter *get-item-wav* "./wav/getitem.wav")
(defparameter *hit-enemy-wav* "./wav/hitenemy.wav")
(defparameter *humu-wav* "./wav/humu3.wav")
(defparameter *jump-wav* "./wav/Jump6")
(defparameter *item-wav* "./wav/item.wav")
(defparameter *dead-wav* "./wav/dead.wav")

(defparameter *font140* (create-font "MSゴシック" :height 140))
(defparameter *font40* (create-font "MSゴシック" :height 40))
(defparameter *font20* (create-font "MSゴシック" :height 20))

(defun set-font ()
  (setf *font140* (create-font "MSゴシック" :height 140)
	*font40* (create-font "MSゴシック" :height 40)
	*font20* (create-font "MSゴシック" :height 20)))

(defun delete-font ()
  (delete-object *font140*)
  (delete-object *font40*)
  (delete-object *font20*))

(defparameter *obj-type-list* ;;マップ番号に対応する色とobj-type
  '(1 :hard-block 2 :soft-block 3 :dokan
    4 :1coin 5 :item 6 :star
    7 :1up        8 :hide-1up     9 :10coin
    a :kuribo     b :nokonoko
    y :goal       z :enter-dokan))


(defun get-obj-img (obj)
  (case obj
    (1 (aref *objects-imgs* +hard-block1+))
    (2 (aref *objects-imgs* +soft-block+))
    (4 (aref *objects-imgs* +hatena-block+))
    (5 (aref *objects-imgs* +hatena-block+))
    (6 (aref *objects-imgs* +hatena-block+))
    (7 (aref *objects-imgs* +hatena-block+))
    (8 (aref *objects-imgs* +toumei-block+))
    (9 (aref *objects-imgs* +hatena-block+))
    (a (aref *kuribo-walk-imgs* +normal+))
    (b (aref *nokonoko-walk-imgs* +normal+))
    (x (aref *objects-imgs* +top-left-dokan+))
    (w (aref *objects-imgs* +mid-left-dokan+))
    (v (aref *objects-imgs* +bot-left-dokan+))
    (u (aref *objects-imgs* +top-right-dokan+))
    (s (aref *objects-imgs* +mid-right-dokan+))
    (r (aref *objects-imgs* +bot-right-dokan+))
    (y (aref *objects-imgs* +bot-right-dokan+))))


;ステージデータ生成
(defun create-field (field-obj)
  (let* ((tate (length field-obj))
	 (yoko (length (car field-obj)))
	 (arr (make-array (list tate yoko) :initial-contents field-obj)))
    (loop for y from 0 below tate
       do (loop for x from 0 below yoko
	     do (let ((obj-type (aref arr y x)))
		  (when (not (eq obj-type 0))
		    (let* ((obj-x (* x *obj-w*)) (obj-x2 (+ obj-x *obj-w*))
			   (obj-y (* y *obj-h*)) (obj-y2 (+ obj-y *obj-h*))
			   (hoge (getf *obj-type-list* obj-type))
			   (img (get-obj-img obj-type)))
		      (cond
			((or (integerp obj-type)
			     (find obj-type '(r s u v w x y)))
			 (push
			  (make-instance 'obj :x obj-x :x2 obj-x2
					 :y obj-y :y2 obj-y2
					 :w *obj-w* :h *obj-h*
					 :w/2 *w/2* :h/2 *h/2*
					 :img img
					 :obj-type hoge)
			  *objects*))
			((eq obj-type 'p) ;;プレイヤー初期位置
			 (with-slots (x x2 y y2 lasty w h) *p*
			   (setf x obj-x x2 (+ x w) y obj-y y2 (+ y h)
				 lasty y)))
			(t
			 (push
			  (make-instance 'move-obj
					 :lasty obj-y
					 :x obj-x :x2 obj-x2
					 :y obj-y :y2 obj-y2
					 :lasty obj-y
					 :w *obj-w* :h *obj-h*
					 :w/2 *w/2* :h/2 *h/2*
					 :vx -1 :vy 0 :dir :left
					 :img img :state :alive
					 :obj-type hoge)
			  *enemies*))))))))))

(defun init-mogeo ()
  (setf *p* (make-instance 'player
			   :vx 0 :vy 0 :ax 0.1 :w *obj-w* :h *obj-h* :w/2 *w/2* :h/2 *h/2*
			   :img (aref *mogeo-walk-right-imgs* +normal+) :state :small
			   :obj-type :player :timer-c (get-internal-real-time))
	*objects* nil
	;;*c-rect* nil
	*items* nil
	*fire* nil
	*enemies* nil)
   (create-field (nth (stage *p*) *stage-list*)))

(defun init-game ()
  (with-slots (x x2 y y2 lasty vx vy ax dy nextx nexty walk-c walk-state walk-func timer-c timer stage goal crouch? jump? state w h w/2 h/2 star-time) *p*
    (setf vx 0 vy 0 ax 0.1 timer 200
	  dy 0 nextx 0 nexty 0 walk-c 0 walk-state 0 walk-func #'+
	  timer-c (get-internal-real-time) state :small w *obj-w* h *obj-h* w/2 *w/2* h/2 *h/2*
	  goal nil star-time 0 crouch? nil jump? nil
	  *objects* nil
	  *items* nil
	  *fire* nil
	  *enemies* nil)
    (create-field (nth (stage *p*) *stage-list*))))

;;ステージクリア後
(defun next-game ()
  (with-slots (x x2 y y2 lasty vx vy ax dy nextx nexty walk-c walk-state walk-func timer timer-c stage goal crouch? jump? star-time) *p*
    (setf vx 0 vy 0 ax 0.1 timer 200
	  dy 0 nextx 0 nexty 0 walk-c 0 walk-state 0 walk-func #'+
	  timer-c (get-internal-real-time)
	  goal nil star-time 0 crouch? nil jump? nil
	  *objects* nil 
	  *items* nil
	  *fire* nil
	  *enemies* nil)
    (incf stage)
    (create-field (nth (stage *p*) *stage-list*))))

;;".\\images\\*.*"
(defun make-imgs-array (img-path)
  (let* ((img-list (mapcar #'namestring (directory img-path)))
         (imgs (make-array (length img-list))))
    (loop for str in img-list
          for i from 0
          do (setf (aref imgs i)
                   (load-image str :type :bitmap
                               :flags '(:load-from-file :create-dib-section))))
    imgs))


(defun load-images ()
  (setf *mogeo-walk-right-imgs* (make-imgs-array ".\\images\\mogeowalk1-*.*")
	*mogeo-walk-left-imgs* (make-imgs-array ".\\images\\mogeowalk2-*.*")
	*objects-imgs* (make-imgs-array ".\\images\\obj*.*")
	*kuribo-walk-imgs* (make-imgs-array ".\\images\\kuribo*.*")
	*nokonoko-walk-imgs* (make-imgs-array ".\\images\\nokonoko*.*")
	*mogeo-crouch-right-imgs* (make-imgs-array ".\\images\\crouch-right*.*")
	*mogeo-crouch-left-imgs* (make-imgs-array ".\\images\\crouch-left*.*")))

(defun delete-object-array (arr)
  (loop for i across arr
        do (delete-object i)))

(defun delete-images ()
  (delete-object-array *mogeo-walk-right-imgs*)
  (delete-object-array *mogeo-walk-left-imgs*)
  (delete-object-array *objects-imgs*)
  (delete-object-array *kuribo-walk-imgs*)
  (delete-object-array *nokonoko-walk-imgs*)
  (delete-object-array *mogeo-crouch-left-imgs*)
  (delete-object-array *mogeo-crouch-right-imgs*))


(defun delete-hoge (hoge)
  (loop for obj in hoge
     do (with-slots (img) obj
	  (delete-object img))))

(defparameter *mogeo* nil)
(defparameter *coin* nil)
(defparameter *moge* nil)

;;四捨五入
(defun gonyu (n)
  (floor (+ 0.5 n)))

;;ブラシ生成
(defun set-brush ()
  (setf *brush* (make-array 7 :initial-contents
                              (list
                                (create-solid-brush (encode-rgb 128 0 255))
                                (create-solid-brush (encode-rgb 255 0 0))
                                (create-solid-brush (encode-rgb 0 255 0))
                                (create-solid-brush (encode-rgb 0 0 255))
                                (create-solid-brush (encode-rgb 255 255 0))
                                (create-solid-brush (encode-rgb 0 255 255))
                                (create-solid-brush (encode-rgb 255 0 255))))))

;;ブラシ削除
(defun delete-brush ()
  (loop for i across *brush*
     do (delete-object i)))


;

;;キー押したとき
(defun moge-keydown (hwnd wparam)
  (with-slots (left right down z x enter) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
	(:left (setf left t))
	(:right (setf right t))
	(:down (setf down t))
	(:return (setf enter t))
	(:keyz (setf z t))
	(:keyx (setf x t))
	;; (:key1 (create-stage 1 hwnd))
	;; (:key2 (create-stage 2 hwnd))
        ;; (:key4 (create-stage 4 hwnd))
	;; (:key5 (create-stage 5 hwnd))
	;; (:key6 (create-stage 6 hwnd))
	;; (:key7 (create-stage 7 hwnd))
	;; (:key8 (create-stage 8 hwnd))
	;; (:key9 (create-stage 9 hwnd))
	;; (:keya (create-stage 'a hwnd))
	;; (:keyb (create-stage 'b hwnd))
	;; (:key0 (create-stage 0 hwnd))
	;; (:keyy (create-stage 'y hwnd))
	(:keyq ;; quit
	 (send-message hwnd (const +wm-close+) nil nil))))))

;;キー話したとき
(defun moge-keyup (wparam)
  (with-slots (left right down z x enter) *keystate*
    (let ((key (virtual-code-key wparam)))
      (case key
	(:left (setf left nil))
	(:right (setf right nil))
	(:down (setf down nil))
	(:return (setf enter nil))
	(:keyx (setf x nil))
	(:keyz (setf z nil))))))


;;効果音ならす
(defun sound-play (path)
  (play-sound path '(:filename :async)))

;;画面スクロール
(defun update-scroll ()
  (with-slots (x scroll) *p*
    (let ((p-center (+ x *w/2*)))
      (when (or (> p-center (+ scroll *screen-center-x*))
             (< p-center (+ scroll *screen-center-x*)))
	(setf scroll (max 0 (- p-center *screen-center-x*)))))))


;;歩行グラフィックタイプ設定
(defun set-walk-img-type ()
  (setf (walk-img-type *p*)
	(case (state *p*)
	  (:small +small+)
	  (:big +big+)
	  (:fire +fire+))))

;;zキー押した場合
(defun z-keydown ()
  (with-slots (fire-time maxvx state dir x x2 y) *p*
    (when (and (= fire-time 0)
	       (eq state :fire))
      (sound-play *fire-wav*)
      (let ((fire-x (if (eq dir :right) x2 (- x *w/2*)))
	    (fire-y  (+ y *fb-h*))
	    (fire-vx (if (eq dir :right) 5 -5)))
	(push (make-instance 'move-obj :x fire-x :x2 (+ x2 *fb-w*) :y fire-y :y2 (+ fire-y *fb-h*)
			     :lasty fire-y :vx fire-vx :vy 0 :obj-type :fire :walk-p t
			     :state :alive :w *fb-w* :h *fb-h* :w/2 *fb-w/2* :h/2 *fb-h/2*
			     :img (aref *objects-imgs* +fireball+))
	      *fire*)
	(setf fire-time *fire-time*)))
    (setf maxvx *dash-speed*))) ;;ダッシュ)

;;downキー
(defun down-keydown ()
  (with-slots (crouch? dir img walk-img-type star-time vy y y2 lasty h h/2) *p*
    (when (and (not (or (= walk-img-type +small+)
			(= walk-img-type +small-star+))) ;;small状態ではなく
	       (null crouch?)) ;;しゃがみ状態でもない時
      (let ((crouch-type (cond
			   ((> star-time 0) +star-crouch+)
			   ((= walk-img-type 3) +big-crouch+)
			   ((= walk-img-type 6) +fire-crouch+))))
	 (setf y  (+ y *obj-h*) ;;大きさ変更
	       h *obj-h*
	       y2 (+ y h) 
	       h/2 *h/2*
	       crouch? t)
	 (if (= vy 0) ;;空中？の時と地面にいる時ではちがう
	     (setf lasty y)
	     (setf lasty (+ lasty *obj-h*)))
	(if (eq dir :right)
	    (setf img (aref *mogeo-crouch-right-imgs* crouch-type))
	    (setf img (aref *mogeo-crouch-left-imgs* crouch-type)))))))

;;downキー放す
(defun down-keyup ()
  (with-slots (crouch? dir img walk-img-type y y2 lasty h h/2) *p*
    (when crouch? ;;しゃがみ状態なら
      (setf y  (- y *obj-h*) ;;大きさ変更
	    lasty y
	    h (* *obj-h* 2)
	    h/2 *obj-h*
	    y2 (+ y h) 
	    crouch? nil)
      (if (eq dir :right)
	  (setf img (aref *mogeo-walk-right-imgs* walk-img-type))
	  (setf img (aref *mogeo-walk-left-imgs* walk-img-type))))))
			 
    

;;leftキー押した場合
(defun left-keydown ()
  (with-slots (vx ax maxvx jump? state dir walk-c walk-state img walk-img-type) *p*
    (when (> vx 0)
      (setf ax 0.2))
    (cond ;; maxvxまで加速or減速
      ((> vx (- maxvx)) (setf vx (min -1 (- vx ax))))
      ((> (- maxvx) vx) (incf vx ax)))
    (when (null jump?)
      (incf walk-c)
      (when (eq dir :right)
	(setf dir :left
	      walk-state +normal+
	      walk-c 0
	      img (aref *mogeo-walk-left-imgs* (+ walk-state walk-img-type)))))))

;;rightキー押した場合
(defun right-keydown ()
  (with-slots (vx ax maxvx jump? state dir walk-c walk-state img walk-img-type) *p*
    (when (< vx 0)
      (setf ax 0.2))
    (cond ;; maxvxまで加速or減速
      ((> maxvx vx) (setf vx (max 1 (+ vx ax))))
      ((> vx maxvx) (decf vx ax)))
    (when (null jump?)
      (incf walk-c)
      (when (eq dir :left)
	(setf dir :right
	      walk-state +normal+
	      walk-c 0
	      img (aref *mogeo-walk-right-imgs* (+ walk-state walk-img-type)))))))

;;leftとrightどちらも押されてない
(defun right-and-left-keyup ()
  (with-slots (vx ax dir walk-c walk-state img walk-img-type) *p*
    (cond ;;徐々に減速
      ((> vx 0) (setf vx (max 0 (- vx ax))))
      ((< vx 0) (setf vx (min 0 (+ vx ax)))))
    (setf walk-c 0
	  walk-state +normal+)
    (when (null (down *keystate*))
      (if (eq dir :left)
	  (setf img (aref *mogeo-walk-left-imgs* (+ walk-state walk-img-type)))
	  (setf img (aref *mogeo-walk-right-imgs* (+ walk-state walk-img-type)))))))

;;歩行グラフィック更新
(defun update-walk-img ()
  (with-slots (dir walk-c walk-state walk-func img walk-img-type) *p*
    ;;walk-counterが10を超えたら画像更新
    (when (> walk-c 10)
      (cond ;;walk-stateが 0 1 2 1 0 1 2 ってなるようにする
	((= walk-state +migi+)   (setf walk-func #'+))
	((= walk-state +normal+))
	((= walk-state +hidari+) (setf walk-func #'-)))
      (setf walk-state (+ (funcall walk-func walk-state 1)))
      (setf walk-c 0)
      (if (eq dir :left)
	  (setf img (aref *mogeo-walk-left-imgs* (+ walk-state walk-img-type)))
	  (setf img (aref *mogeo-walk-right-imgs* (+ walk-state walk-img-type)))))))

;;次に移動するxy座標更新
(defun update-next-pos ()
  (with-slots (nextx nexty x y lasty vx vy dy) *p*
    ;;nextx座標更新
    (setf nextx (+ x (gonyu vx)))
    ;;プレイヤーのnexty座標更新
    (setf dy (+ (- y lasty) vy)
	  nexty (+ y dy))))

;;スター状態更新
(defun update-star-time ()
  (with-slots (star-time walk-img-type) *p*
    (when (> star-time 0)
      (decf star-time)
      (if (= walk-img-type +small+)
	  (setf walk-img-type +small-star+)
	  (setf walk-img-type +big-star+)))))

;;プレイヤーの状態更新
(defun update-mogeo ()
  (with-slots (left right down) *keystate*
    (with-slots (vx vy ax fire-time muteki-time jump? crouch?) *p*
      (set-walk-img-type)
      (setf ax 0.1)
      (update-star-time) ;;スター状態の更新
      (when (> muteki-time 0) ;;無敵状態
	(decf muteki-time))
      ;;zキー
      (if (z *keystate*)
	  (z-keydown)
	  (setf (maxvx *p*) *walk-speed*))
      ;;ファイア打てる間隔
      (when (> fire-time 0)
	(decf fire-time))
      (when down
	(down-keydown))
      (when (null down)
	(down-keyup))
      ;;leftキー
      (when (and left
		 (or (null crouch?)
		     jump?))
	(left-keydown))
      ;;rightキー
      (when (and right
		 (or (null crouch?)
		     jump?))
	(right-keydown))
      ;;左右キーが入力されていないとき
      (when (or (and (null left) (null right))
		(and (null jump?) crouch?))
	(right-and-left-keyup))
      ;;xキー
      (when (and (x *keystate*) (= vy 0))
	(setf vy (- *jump-power* (abs (floor (gonyu vx) 2)))
	      jump? t)
	(sound-play *jump-wav*))
      (update-walk-img)
      (update-next-pos))))

;;敵の画像更新
(defun update-enemy-img (e)
  (with-slots (obj-type walk-c walk-state walk-func img dir) e
    (incf walk-c)
    (case obj-type
      (:kuribo
       (when (> walk-c 10)
	 (cond ;;walk-stateが 0 1 2 1 0 1 2 ってなるようにする
	   ((= walk-state +migi+)   (setf walk-func #'+))
	   ((= walk-state +normal+))
	   ((= walk-state +hidari+) (setf walk-func #'-)))
	 (setf walk-state (funcall walk-func walk-state 1))
	 (setf walk-c 0)
	 (setf img (aref *kuribo-walk-imgs* walk-state))))
      (:nokonoko
       (when (> walk-c 10)
	 (setf walk-c 0
	       walk-state (- 1 walk-state))
	 (let ((temp (+ walk-state (if (eq dir :right) 2 0))))
	   (setf img (aref *nokonoko-walk-imgs* temp))))))))
	 
      
  

;;敵の座標更新
(defun update-move-obj (e)
  (with-slots (x x2 y y2 nextx nexty dy vy vx lasty w h dir walk-p state) e
    (when walk-p
      ;;x座標
      (setf nextx (+ x vx))
      ;;画像更新
      (update-enemy-img e)
      ;;y座標
      (setf dy (min (+ (- y lasty) vy) 16))
      (setf nexty (+ y dy))
      (when (> nexty *client-h*)
	(setf state :dead)))))

;;:dead状態のものをけす
(defun remove-dead ()
  (dolist (e *enemies*)
    (when (eq (state e) :dead)
      (setf *enemies* (remove e *enemies* :test #'equal))))
  (dolist (e *items*)
    (when (eq (state e) :dead)
      (setf *enemies* (remove e *enemies* :test #'equal)))))


;;動くもの座標更新
(defun update-move-objects ()
  (dolist (enemy *enemies*)
    (update-move-obj enemy))
  (dolist (item *items*)
    (update-move-obj item))
  (dolist (fire *fire*)
    (update-move-obj fire)))


;;新しい当たり判定
;;obj1とobj2の当たり判定
(defun obj-hit-p (obj1 obj2)
  (let ((obj1-px (+ (x obj1) (w/2 obj1)))
	(obj1-py (+ (y obj1) (h/2 obj1)))
	(obj2-px (+ (x obj2) (w/2 obj2)))
	(obj2-py (+ (y obj2) (h/2 obj2))))
    (if (and (< (abs (- obj1-px obj2-px))  (+ (w/2 obj1) (w/2 obj2)))
	     (< (abs (- obj1-py obj2-py)) (+ (h/2 obj1) (h/2 obj2))))
	t
	nil)))


;;obj1がブロックとか(obj2)にぶつかってたらnextx座標補正
(defun next-x-pos-hosei (obj1 obj2)
  (if (> (vx obj1) 0)
      (setf (nextx obj1) (- (x obj2) (w obj1)))
      (setf (nextx obj1) (x2 obj2))))

;;;;プレイヤー以外がブロックとぶつかってたら移動方向を変える 
(defun vx-change (obj1)
  (setf (vx obj1) (- (vx obj1)))
  (if (eq (dir obj1) :right)
      (setf (dir obj1) :left)
      (setf (dir obj1) :right)))

;;obj1がブロックとか(obj2)にぶつかった時のnexty座標補正
(defun next-y-pos-hosei (obj1 obj2)
  (if (>= (dy obj1) 0) ;;落下中
      (setf (y obj1) (- (y obj2) (h obj1))
	    (nexty obj1) (y obj1))
      (setf (y obj1) (y2 obj2)
	    (nexty obj1) (y obj1))))

;;y方向でなにかにぶつかってたらvy補正
(defun vy-change (obj1)
  (if (>= (dy obj1) 0) ;;落下中
      (case (obj-type obj1)
	(:star (setf (vy obj1) -16)) ;;跳ねる
	(:fire (setf (vy obj1) -10))  ;;跳ねる
	(:player
	 (if (d-hit obj1) ;;敵を踏んづけているか
	     (setf (vy obj1) -12)
	     (setf (vy obj1) 0)))
	(otherwise (setf (vy obj1) 0))) ;;着地
      ;;上昇時
      (setf (vy obj1) 1))) ;;落下させる


;;甲羅やスターで敵を倒したときのジャンプアニメーション
(defun dead-jump (e)
  (cond
    ((eq (state e) :dead-jump)
     (setf (vy e) -13
	   (state e) :dead-jump2))
    ((eq (state e) :dead-jump2)
     (setf (vy e) 1))))

;;obj1のx座標更新した場合のの当たり判定
(defun hit-x-moved (obj1 obj2)
  (let* ((obj1-nextpx (+ (nextx obj1) (w/2 obj2)))
	 (obj1-py (+ (y obj1) (h/2 obj1)))
	 (obj2-px (+ (x obj2) (w/2 obj2)))
	 (obj2-py (+ (y obj2) (h/2 obj2))))
    (if (and (< (abs (- obj1-nextpx obj2-px)) (+ (w/2 obj1) (w/2 obj2)))
	     (< (abs (- obj1-py obj2-py)) (+ (h/2 obj1) (h/2 obj2))))
	t
	nil)))

;;obj1のy座標更新した場合の当たり判定
(defun hit-y-moved (obj1 obj2)
  (let ((obj1-px (+ (x obj1) (w/2 obj1)))
	(obj1-nextpy (+ (nexty obj1) (h/2 obj1)))
	(obj2-px (+ (x obj2) (w/2 obj2)))
	(obj2-py (+ (y obj2) (h/2 obj2))))
    (if (and (< (abs (- obj1-px obj2-px)) (+ (w/2 obj1) (w/2 obj2)))
	     (< (abs (- obj1-nextpy obj2-py)) (+ (h/2 obj1) (h/2 obj2))))
	t
	nil)))


;;敵とぶつかってプレイヤー状態変化する
(defun hit-enemy-change-state ()
  (with-slots (state h h/2 y lasty muteki-time crouch? img) *p*
    (if (eq state :small) ;;ちび状態だったら
	(progn
	  (setf state :dead)) ;;死亡
	(progn
	  (sound-play *hit-enemy-wav*)
	  (setf state :small ;;ちび状態にする
		muteki-time *muteki-time*
		y  (- y *obj-h*)
		lasty (- y *obj-h*)
		h *obj-h*
		h/2 *h/2*
		crouch? nil
		img (aref *mogeo-walk-right-imgs* 1))))))

;;プレイヤーがx方向に移動後に甲羅とぶつかったとき 甲羅の位置をここで変更しちゃう
(defun hit-player-x-moved-koura (e)
  (if (= (vx e) 0) ;;甲羅が止まっているとき
      (if (> (vx *p*) 0) ;;プレイヤーが右方向に移動してたら
	  (setf (vx e) *koura-speed*
		(x e) (+ (x2 *p*) (vx e))
		(x2 e) (+ (x e) (w e))
		(nextx e) (x e))
	  (setf (vx e) (- *koura-speed*)
		(x e) (- (x *p*) (w e) (- (vx e)))
		(x2 e) (+ (x e) (w e))
		(nextx e) (x e)))
      ;;甲羅が動いてるとき
      (hit-enemy-change-state)))

;;プレイヤーがx方向に移動後に敵とぶつかったとき
(defun hit-player-x-moved-enemy (e)
  (with-slots (state star-time muteki-time) *p*
    (cond
      ((> star-time 0) ;;プレイヤーがスターだったら
       (sound-play *humu-wav*)
       (setf (state e) :dead-jump))
      ((= muteki-time 0)
       ;;(setf *moge* "xhoge")
       (case (obj-type e)
	 (:koura (hit-player-x-moved-koura e))
	 ((:kuribo :nokonoko)
	  (cond
	    ((eq state :small)
	     (setf state :dead))
	    (t (hit-enemy-change-state)))))))))

;;プレイヤーがy方向に移動後に甲羅とぶつかったとき 甲羅の位置をここで変更しちゃう
(defun hit-player-y-moved-koura (e)
  (if (= (vx e) 0) ;;甲羅が止まっているとき
      (when (> (dy *p*) 0) ;;プレイヤーが落下中
	(if (>= (+ (x *p*) (w/2 *p*)) (+ (x e) (w/2 e))) ;;プレイヤーと甲羅の位置関係
	    (setf (vx e) -6
		  (x e) (- (x *p*) (w e) (- (vx e)))
		  (x2 e) (+ (x e) (w e))
		  (nextx e) (x e))
	    (setf (vx e) 6
		  (x e)  (+ (x2 *p*) (vx e))
		  (x2 e) (+ (x e) (w e))
		  (nextx e) (x e))))
      ;;甲羅が動いてるとき
      (if (> (dy *p*) 0) ;;プレイヤーが落下
	  (setf (vx e) 0 ;;甲羅を止める
		(d-hit *p*) t)
	  (hit-enemy-change-state))))

;;甲羅を*enemies*に追加
(defun add-koura (e)
  (push
   (make-instance 'move-obj :vx 0 :vy 0 :x (x e) :x2 (x2 e) :y (y e) :y2 (y2 e)
		  :w *obj-w* :h *obj-h* :lasty (y e) :w/2 *w/2* :h/2 *h/2*
		  :state :alive :img (aref *objects-imgs* +koura+)
		  :obj-type :koura :walk-p nil)
   *enemies*))

;;eを*enemies*から消す
(defun delete-enemy (e)
  (setf *enemies* (remove e *enemies* :test #'equal)))

;;fを*fire*から消す
(defun delete-fire (f)
  (setf *fire* (remove f *fire* :test #'equal)))

;;itemを*items*から消す
(defun delete-item (item)
  (setf *items* (remove item *items* :test #'equalp)))



;;objを*objects*から消す
(defun delete-obj (obj)
  (setf *objects* (remove obj *objects* :test #'equal)))

;;スターを*items*に追加
(defun add-star (obj)
  (with-slots (x x2 y y2 h) obj
    (push
     (make-instance 'move-obj :x x :x2 x2 :y (- y *obj-h*) :y2 y
		    :lasty (- y *obj-h*) :vx 2 :vy 0 :obj-type :star
		    :state nil :w *obj-w* :h *obj-h* :walk-p t
		    :w/2 *w/2* :h/2 *h/2*
		    :img (aref *objects-imgs* +star+))
     *items*)))

(defun add-1up (obj)
  (with-slots (x x2 y y2 h) obj
    (push
     (make-instance 'move-obj :vx 1 :vy 0
			 :w *obj-w* :h *obj-h* :img (aref *objects-imgs* +1up+)
			 :x x :x2 x2 :y (- y *obj-h*) :y2 y
			 :obj-type :1up :state :alive :walk-p t
			 :w/2 *w/2* :h/2 *h/2*
			 :lasty (- y *obj-h*))
     *items*)))

(defun add-kinoko (obj)
  (with-slots (x x2 y y2 h) obj
    (push (make-instance 'move-obj :vx 1 :vy 0
			 :w *obj-w* :h *obj-h* :img (aref *objects-imgs* +kinoko+)
			 :x x :x2 x2 :y (- y *obj-h*) :y2 y
			 :obj-type :kinoko :state nil :walk-p t
			 :w/2 *w/2* :h/2 *h/2*
			 :lasty (- y *obj-h*))
	  *items*)))

(defun add-flower (obj)
  (with-slots (x x2 y y2 h) obj
    (push (make-instance 'move-obj :vx 0 :vy 0 :img (aref *objects-imgs* +flower+)
			 :w *obj-w* :h *obj-h*
			 :x x :x2 x2 :y (- y *obj-h*) :y2 y
			 :obj-type :flower :state nil :walk-p nil
			 :w/2 *w/2* :h/2 *h/2*
			 :lasty (- y *obj-h*))
	  *items*)))

(defun add-coin (obj)
  (with-slots (x x2 y y2 h) obj
    (push (make-instance 'move-obj :vx 0 :vy -12 :img (aref *objects-imgs* +coin+)
			 :w *obj-w* :h *obj-h*
			 :x x :x2 x2 :y (- y *obj-h*) :y2 y
			 :obj-type :coin :state :alive :walk-p nil
			 :w/2 *w/2* :h/2 *h/2*
			 :lasty (- y *obj-h*))
	  *items*)))

;;プレイヤーとソフトブロックがぶつかった場合
(defun hit-player-softblock (obj)
  (when (not (eq (state *p*) :small))
    (delete-obj obj)))

;;ブロックを空ブロック(ハードブロック)に変える
(defun change-to-kara-block (obj)
  (setf (obj-type obj) :hard-block ;;ハードブロックに変更
	(img obj) (aref *objects-imgs* +kara-block+))) ;;画像も変更

;;プレイヤーとスターブロックがぶつかった場合
(defun hit-player-starblock (obj)
  (with-slots (obj-type img) obj
    (sound-play *item-wav*)
    (change-to-kara-block obj)
    (add-star obj)))

;;プレイヤーと1コインブロックがぶつかった場合
(defun hit-player-1coinblock (obj)
  (change-to-kara-block obj)
  (sound-play *coin-wav*)
  (add-coin obj)
  (incf (coin *p*)))

;;プレイヤーと10コインブロックがぶつかった場合
(defun hit-player-10coinblock (obj)
  (if (>= (hit obj) 10) ;;10回叩かれるまで
      (change-to-kara-block obj)
      (progn
	(sound-play *coin-wav*)
	(add-coin obj)
	(incf (hit obj))
	(incf (coin *p*)))))

;;プレイヤーと1upブロックがぶつかった場合
(defun hit-player-1upblock (obj)
  (sound-play *item-wav*)
  (change-to-kara-block obj)
  (add-1up obj))

;;プレイヤーとアイテムブロックがぶつかった場合
(defun hit-player-itemblock (obj)
  (sound-play *item-wav*)
  (change-to-kara-block obj)
  (case (state *p*) ;;プレイヤーの状態で出るアイテム変わる
    (:small (add-kinoko obj))
    ((:big :fire) (add-flower obj))))

;; プレイヤーとブロックがあたったら
(defun hit-player-block (obj)
  (with-slots (obj-type x x2 y y2 h hit img) obj
    (with-slots (state coin zanki) *p*
      (case obj-type
	(:hard-block (sound-play *block-hit-wav*))
	(:soft-block (hit-player-softblock obj)) ;;壊れるブロック
	(:star   (hit-player-starblock obj)) ;;スターブロック
	(:1coin  (hit-player-1coinblock obj)) ;;１コイン
	(:10coin (hit-player-10coinblock obj)) ;;10コイン
	((:1up :hide-1up) (hit-player-1upblock obj))
	(:item (hit-player-itemblock obj))))))

;;甲羅以外の敵とぶつかった場合
(defun hit-player-y-moved-normal-enemy (e)
  (if (> (dy *p*) 0) ;;踏んづけてたら
      (progn
	(setf (d-hit *p*) t)
	(when (eq (obj-type e) :nokonoko)
	  (add-koura e)))
      (hit-enemy-change-state)))

;;プレイヤーがy方向に移動後に敵とぶつかったとき
(defun hit-player-y-moved-enemy (e)
  (with-slots (state star-time muteki-time) *p*
    (when (not (eq (state e) :dead-jump)) ;;敵が死亡状態ではない
      (cond
	((> star-time 0) ;;プレイヤーがスターだったら
	 (sound-play *humu-wav*)
	 (setf (state e) :dead-jump))
	((= muteki-time 0)
	 (case (obj-type e)
	   (:koura (hit-player-y-moved-koura e))
	   ((:kuribo :nokonoko)
	    (sound-play *humu-wav*)
	    (hit-player-y-moved-normal-enemy e)
	    (delete-enemy e))))))))


;;x座標更新 nextxをxに入れる
(defun update-x-pos (obj1)
  (setf (x obj1) (nextx obj1)
	(x2 obj1) (+ (x obj1) (w obj1))))

;;y座標更新 nextyをyに入れる
(defun update-y-pos (obj1)
  (setf (lasty obj1) (y obj1)
	(y obj1) (nexty obj1)
	(y2 obj1) (+ (y obj1) (h obj1))))

;;x方向の敵と敵の当たり判定
(defun hit-x-enemy-enemy (e1 e2)
  (when (not (or (equal e1 e2)
		 (eq (state e1) :dead-jump)
		 (eq (state e1) :dead-jump2)
		 (eq (state e2) :dead-jump)
		 (eq (state e2) :dead-jump2)))
    (cond
      ((and (eq (obj-type e1) :koura) ;;動いてる甲羅と敵がぶつかる時
	    (/= (vx e1) 0))
       (sound-play *humu-wav*)
       (setf (state e2) :dead-jump))
      ((and (eq (obj-type e2) :koura) ;;動いてる甲羅と敵がぶつかる時
	    (/= (vx e2) 0))
       (sound-play *humu-wav*)
       (setf (state e1) :dead-jump))
      (t (next-x-pos-hosei e1 e2)
	 (vx-change e1)
	 (vx-change e2)
	 (update-x-pos e1)))))





;;obj1がx方向へ移動した場合のオブジェクトとの当たり判定
(defun hit-obj-x-moved-objects (obj1)
  (loop for obj in *objects*
     do (when (and (hit-x-moved obj1 obj)
		   (not (eq (state obj1) :dead-jump2))
		   (not (eq (state obj1) :dead-jump)))
	  (cond
	    ((eq (obj-type obj1) :fire) (delete-fire obj1))
	    ((and (eq (obj-type obj1) :player)
		  (eq (obj-type obj) :goal))
	     (setf (goal obj1) t))
	    (t
	     (next-x-pos-hosei obj1 obj)
	     (when (not (eq (obj-type obj1) :player))
	       (vx-change obj1))))
	  (return))))

;;obj1がx方向へ移動した後の敵との当たり判定
(defun hit-obj-x-moved-enemies (obj1)
  (loop for e in *enemies* ;;TODO
     do (when (and (obj-hit-p obj1 e)
		   (not (eq (state obj1) :dead-jump2))
		   (not (eq (state obj1) :dead-jump))
		   (not (eq (state e) :dead-jump2))
		   (not (eq (state e) :dead-jump)))
	  (cond
	    ((and (eq (obj-type obj1) :player)
		  (not (eq (state obj1) :dead)))
	     (hit-player-x-moved-enemy e))
	    ((eq (obj-type obj1) :fire)
	     (sound-play *humu-wav*)
	     (delete-fire obj1)
	     (delete-enemy e))
	    (t (hit-x-enemy-enemy obj1 e)))
	  (return))))

;;obj1がy方向へ移動した場合のオブジェクトとの当たり判定
(defun hit-obj-y-moved-objects (obj1)
  (let ((hoge nil))
    (loop for obj in *objects*
       do (when (and (hit-y-moved obj1 obj)
		     (not (eq (state obj1) :dead-jump2))
		     (not (eq (state obj1) :dead-jump)))
	    (cond
	      ((and (eq (obj-type obj1) :fire)
		    (< (dy obj1) 0))
	       (delete-fire obj1))
	      ((and (eq (obj-type obj1) :coin)
		    (> (dy obj1) 0))
	       (delete-item obj1))
	      (t
	       (setf hoge t)
	       (next-y-pos-hosei obj1 obj)
	       (vy-change obj1)
	       (when (eq (obj-type obj1) :player)
		 (when (>= (dy obj1) 0)
		   (setf (jump? *p*) nil))
		 (when (< (dy obj1) 0) ;;上昇中にブロックにぶつかったら
		   (hit-player-block obj)))))
	    (return)))
    (when (null hoge) ;;どれにもぶつかっていない時落下させる
      (setf (vy obj1) 1))))

;;obj1がy方向へ移動した後の敵との当たり判定
(defun hit-obj-y-moved-enemies (obj1)
  (loop for e in *enemies*
     do (when (and (obj-hit-p obj1 e)
		   (not (eq (state obj1) :dead-jump2))
		   (not (eq (state obj1) :dead-jump))
		   (not (eq (state e) :dead-jump2))
		   (not (eq (state e) :dead-jump)))
	  (cond
	    ((eq (obj-type obj1) :fire)
	     (sound-play *humu-wav*)
	     (delete-fire obj1)
	     (delete-enemy e))
	    ((and (eq (obj-type obj1) :player)
		  (not (eq (state obj1) :dead)))
	     (hit-player-y-moved-enemy e)
	     (when (d-hit obj1) ;;敵を踏んづけていたら
	       (next-y-pos-hosei obj1 e)
	       (vy-change obj1))))
	  (return))))

;;敵とぶつかっていた場合の位置更新
(defun update-y-pos-hit-enemy (obj1)
  (when (d-hit obj1)
    (setf (lasty obj1) (nexty obj1)
	  (y obj1) (nexty obj1)
	  (y2 obj1) (+ (y obj1) (h obj1)))))

;;敵とぶつかっていたフラグ初期化
(defun init-hit-enemy-flag (obj1)
  (setf (d-hit obj1) nil
	(u-hit obj1) nil))

;;obj1と全てのブロックと全ての敵との当たり判定
(defun new-hit-objects-and-enemies (obj1)
  (hit-obj-x-moved-objects obj1)
  (update-x-pos obj1) ;;x更新
  (hit-obj-x-moved-enemies obj1)
  (hit-obj-y-moved-objects obj1)
  (update-y-pos obj1) ;;y更新
  (hit-obj-y-moved-enemies obj1)
  (update-y-pos-hit-enemy obj1)
  (dead-jump obj1)
  (init-hit-enemy-flag obj1))
  
  
  
;;obj1と全てのブロックとの当たり判定
(defun new-hit-objects (obj1)
  (hit-obj-x-moved-objects obj1)
  (update-x-pos obj1)
  (hit-obj-y-moved-objects obj1)
  (update-y-pos obj1))

;;動くものと動かないもの全ての当たり判定
(defun all-hit-objects ()
  (dolist (item *items*)
    (when (walk-p item)
      (new-hit-objects item)
      (when (> (y item) *screen-h*)
	(delete-item item))))
  (new-hit-objects-and-enemies *p*)
  (when (eq (state *p*) :dead)
    (sound-play *dead-wav*))
  (when (> (y *p*) *screen-h*)
    (sound-play *dead-wav*)
    (setf (state *p*) :dead))
  (dolist (f *fire*)
    (new-hit-objects-and-enemies f)
    (when (> (y f) *screen-h*)
      (delete-fire f)))
  (dolist (e *enemies*)
    (when (walk-p e)
      (new-hit-objects-and-enemies e)
      (when (> (y e) *screen-h*)
	(delete-enemy e)))))




;;プレイヤーとキノコがぶつかった場合
(defun hit-player-kinoko ()
  (with-slots (state h h/2 img y lasty y2 star-time dir) *p*
    (when (eq state :small)
      (setf y (- y *obj-h*)
	    lasty (- lasty *obj-h*)
	    h (* *obj-h* 2)
	    y2 (+ y h)
	    h/2 *obj-h*
	    state :big)
      (when (= star-time 0) ;;スター状態ではないとき画像変更
	(if (eq dir :right)
	    (setf img (aref *mogeo-walk-right-imgs* 4))
	    (setf img (aref *mogeo-walk-left-imgs* 4)))))))

;;プレイヤーとフラワーがぶつかった場合
(defun hit-player-flower ()
  (with-slots (state h h/2 img y lasty y2 star-time dir) *p*
    (when (eq state :small)
      (setf y (- y *obj-h*)
	    lasty (- lasty *obj-h*)
	    h (* *obj-h* 2)
	    h/2 *obj-h*
	    y2 (+ y h)))
    (setf state :fire)
    (when (= star-time 0)
      (if (eq dir :right)
	  (setf img (aref *mogeo-walk-right-imgs* 7))
	  (setf img (aref *mogeo-walk-left-imgs* 7))))))

;;プレイヤーとスターがぶつかった場合
(defun hit-player-star ()
  (with-slots (img star-time dir) *p*
    (setf star-time *star-time*)
    (if (eq dir :right)
	(setf img (aref *mogeo-walk-right-imgs* 10))
	(setf img (aref *mogeo-walk-left-imgs* 10)))))

;;プレイヤーとアイテムの当たり判定
;;プレイヤーとアイテムの当たり判定
(defun hit-player-items ()
  (dolist (item *items*)
    (when (obj-hit-p *p* item)
      (sound-play *get-item-wav*)
      (with-slots (obj-type) item
	(case obj-type
	  (:kinoko (hit-player-kinoko))
	  (:flower (hit-player-flower))
	  (:1up (incf (zanki *p*)))
	  (:star (hit-player-star))))
      (delete-item item))))



         

;;時間経過
(defun update-timer ()
  (let ((now (get-internal-real-time)))
    (when (>= (- now (timer-c *p*)) 1000)
      (decf (timer *p*))
      (setf (timer-c *p*) now)
      (when (= (timer *p*) 0)
	(setf (state *p*) :dead)))))



;;test
(defun render-test ()
  (loop for i in '(1 2 4 5 6 7 8 9 a b r s u v w x y)
       for j from 0
     do
       (select-object *hogememdc* (get-obj-img i))
       (transparent-blt *hmemdc* 100 (* j 32) *hogememdc* 0 0 :width-source 32 :height-source 32
		     :width-dest 32 :height-dest 32
		     :transparent-color (encode-rgb 0 255 0))))

(defun render-mogeo () 
  (with-slots (x x2 y y2 w h img scroll dir muteki-time) *p*
    (select-object *hogememdc* img)
    (if (> muteki-time 0)
	(when (/= 0 (mod muteki-time 10))
	  (transparent-blt *hmemdc* (- x scroll) y *hogememdc* 0 0 :width-source w :height-source h
			   :width-dest w :height-dest h
			   :transparent-color (encode-rgb 0 255 0)))
	(transparent-blt *hmemdc* (- x scroll) y *hogememdc* 0 0 :width-source w :height-source h
			 :width-dest w :height-dest h
			 :transparent-color (encode-rgb 0 255 0)))))


;;objsの描画
(defun render-objects (objs)
  (when objs
    (with-slots (scroll) *p*
      (loop for obj in objs
	 do
	   (with-slots (x y w h img) obj
	     (let ((new-x  (- x scroll)))
	       (when (and (< new-x (rect-right *c-rect*))
			  (<= (rect-left *c-rect*) (+ new-x w)))
		 (when (and (eq (type-of obj) 'move-obj)
			    (null (walk-p obj)))
		   (setf (walk-p obj) t))
		 (select-object *hogememdc* img)
		 (transparent-blt *hmemdc* new-x y *hogememdc* 0 0 :width-source w :height-source h
				  :width-dest w :height-dest h
				  :transparent-color (encode-rgb 0 255 0)))))))))

;;バックグラウンドカラー
(defun render-background ()
  (select-object *hmemdc* (aref *brush* 5))
  (rectangle *hmemdc* 0 0 (rect-right *c-rect*) (rect-bottom *c-rect*)))

;;ゴールテキスト表示
(defun render-goal-text ()
  (when (goal *p*)
    (select-object *hmemdc* *font140*)
    (set-bk-mode *hmemdc* :transparent)
    (text-out *hmemdc* "GOAL!" 580 200)
    (select-object *hmemdc* *font40*)
    (text-out *hmemdc* "press enter key" 620 340)
    (select-object *hmemdc* (get-stock-object :system-font))))

(defun render-dead-message ()
  (when (eq (state *p*) :dead)
    (select-object *hmemdc* *font140*)
    (set-bk-mode *hmemdc* :transparent)
    (text-out *hmemdc* "死亡！!" 370 100)
    (select-object *hmemdc* *font40*)
    (text-out *hmemdc* "press enter key" 410 220)
    (select-object *hmemdc* (get-stock-object :system-font))))

;;ゴールした後のキー入力
(defun goal-key-input ()
  (when (enter *keystate*)
    (incf (score *p*) (timer *p*))
    (if (= (stage *p*) (1- (length *stage-list*)))
	(setf (state *p*) :clear
	      (goal *p*) nil)
	(next-game))))

(defun render-game-info ()
  (select-object *hmemdc* *font20*)
  (set-bk-mode *hmemdc* :transparent)
  (text-out *hmemdc* (format nil "score ~4,'0d  time：~3,'0d  コイン：~2,'0d  残機：~2,'0d"
			     (score *p*) (timer *p*) (coin *p*)  (zanki *p*)) 600 10)
  (select-object *hmemdc* (get-stock-object :system-font)))

;;クリアメッセージ
(defun render-clear-message ()
  (select-object *hmemdc* *font140*)
  (set-bk-mode *hmemdc* :transparent)
  (text-out *hmemdc* "全クリ！！" 200 100)
  (text-out *hmemdc* (format nil "スコア：~d" (score *p*)) 200 300)
  (select-object *hmemdc* (get-stock-object :system-font)))

;;ゲーム画面描画
(defun render-game (hdc)
  (render-background)
  (cond
    ((eq (state *p*) :clear)
     (render-clear-message))
    (t
     (render-objects *objects*)
     (render-objects *enemies*)
     (render-objects *items*)
     (render-objects *fire*)
     (render-game-info)
     (render-goal-text)
     (render-dead-message)
     ;;(text-out *hmemdc* (format nil "x:~d y:~d vx:~d vy:~d dy:~d scroll:~d state:~a score:~a" (x *p*) (y *p*) (vx *p*)  (vy *p*) (dy *p*) (scroll *p*) (state *p*) (score *p*)) 100 100) ;;debug
  ;;(render-test) ;;debug
     (render-mogeo)))
  (transparent-blt hdc 0 0 *hmemdc* 0 0
   		   :width-dest (rect-right *c-rect*) :height-dest (rect-bottom *c-rect*)
   		   :width-source (rect-right *c-rect*) :height-source (rect-bottom *c-rect*)
   		   :transparent-color (encode-rgb 0 255 0)))
  ;;;(bit-blt hdc 0 0 *hmemdc* 0 0 :width (rect-right *c-rect*)
  ;;         :height (rect-bottom *c-rect*)  :raster-op :srccopy))
;;プレイヤー死んだら初期化
(defun player-dead-key-input ()
  (when (enter *keystate*)
    (when (>= (score *p*) 30)
      (decf (score *p*) 30))
    (decf (zanki *p*))
    (init-game)))


;;てすと
(defun reload-stage-data (stage)
  (setf *enemies* nil *objects* nil)
  (create-field stage))
  


;;更新
(defun update-frame (hwnd)
  (cond
    ((goal *p*)
     (goal-key-input))
    ((eq (state *p*) :dead)
     (player-dead-key-input))
    ((eq (state *p*) :clear)
     )
    (t
     ;;(reload-stage-data *test-stage*) ;;test
     (update-mogeo)
     (update-scroll)
     (update-move-objects)
     ;;(all-hit-enemies)
     (all-hit-objects)
     
     (hit-player-items)
     (remove-dead)
     (update-timer)))
  (invalidate-rect hwnd nil nil))

;;クライアント領域を*client-w* *client-h*に設定
(defun set-client-size (hwnd)
  (let* ((rc (get-client-rect hwnd))
	 (rw (get-window-rect hwnd))
	 (new-w (+ *client-w* (- (- (rect-right rw) (rect-left rw))
				 (- (rect-right rc) (rect-left rc)))))
	 (new-h (+ *client-h* (- (- (rect-bottom rw) (rect-top rw))
				 (- (rect-bottom rc) (rect-top rc))))))
    (set-window-pos hwnd nil 0 0 new-w new-h '(:no-move :no-zorder))))
    
    

(defwndproc moge-wndproc (hwnd msg wparam lparam)
  (switch msg
    ((const +wm-create+)
     (set-brush)
     (set-font)
     (load-images)
     (init-mogeo)
     (set-client-size hwnd)
     (setf *c-rect* (get-client-rect hwnd))
     (setf *screen-center-x* (+ (rect-right *c-rect*)
				(floor (- (rect-left *c-rect*) (rect-right *c-rect*)) 2)))
     
     (set-layered-window-attributes hwnd (encode-rgb 0 255 0) 0 (const +lwa-colorkey+))
     (with-dc (hdc hwnd)
       (setf *hmemdc* (create-compatible-dc hdc)
             *hbitmap* (create-compatible-bitmap hdc (rect-right *c-rect*) (rect-bottom *c-rect*))
	     *hogememdc* (create-compatible-dc hdc)
             *hogebitmap* (create-compatible-bitmap hdc (rect-right *c-rect*) (rect-bottom *c-rect*)))
       (select-object *hmemdc* *hbitmap*)
       (select-object *hogememdc* *hogebitmap*)))
    ((const +wm-paint+)
     (with-paint (hwnd hdc)
       (render-game hdc)))
    ((const +wm-close+)
     (destroy-window hwnd))
    ;;((const +wm-timer+)
    ;; (invalidate-rect hwnd nil nil))
    ((const +wm-keydown+)
     (moge-keydown hwnd wparam))
    ((const +wm-keyup+)
     (moge-keyup wparam))
    ((const +wm-destroy+)
     (delete-dc *hmemdc*)
     (delete-object *hbitmap*)
     (delete-dc *hogememdc*)
     (delete-object *hogebitmap*)
     (delete-brush)
     (delete-images)
     (delete-font)
     (post-quit-message)))

  (default-window-proc hwnd msg wparam lparam))

(defun moge ()
  (register-class "MOGE" (callback moge-wndproc)
                  :cursor (load-cursor :arrow)
                  :background (create-solid-brush (encode-rgb 0 255 0)))
  (let ((hwnd (create-window "MOGE"
                             :window-name "スーパーもげおワールド"
                             :ex-styles  (logior-consts +WS-EX-LAYERED+ +ws-ex-composited+) ;;透明
                             :styles (logior-consts +ws-overlappedwindow+ +ws-visible+)
                             :x 400 :y 100 :width 960 :height 720))
        (msg (make-msg)))
    
    (show-window hwnd)
    (update-window hwnd)
    (do ((done nil))
        (done)
      (let ((m (ftw:peek-message msg :remove-msg :remove :error-p nil)))
        (cond
          (m
            ;;(let ((r (get-message msg)))
            (cond
              ((= (msg-message msg) (const +wm-quit+))
               (setf done t))
              (t
               (translate-message msg)
               (dispatch-message msg))))
          (t
            (sleep 0.01)
            (update-frame hwnd)))))
    (msg-wparam msg)))

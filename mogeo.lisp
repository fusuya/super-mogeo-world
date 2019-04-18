
(ql:quickload '(:sdl2 :sdl2-image :sdl2-gfx :sdl2-ttf))

(load "keystate.lisp")

(defparameter *obj-w* 32)
(defparameter *obj-h* 32)

(defparameter *tate* 20)
(defparameter *yoko* 30)

(defparameter +screen-w+ (* *obj-w* *yoko*))
(defparameter +screen-h+ (* *obj-h* *tate*))

(defparameter *font* nil)

;; 画面更新間隔、ミリ秒。
(defparameter *fps* 14)

(defparameter *field1-obj*
  ;; (make-array (list *tate* *yoko*)
  ;; 	      :initial-contents
        '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1)
          (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1)
          (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1)
          (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1)))

(defclass obj ()
  ((x
    :accessor x
    :initform 0
    :initarg :x)
   (y
    :accessor y
    :initform 0
    :initarg :y)
   (color
    :accessor color
    :initform nil
    :initarg :color)
   (obj-type
    :accessor obj-type
    :initform 0
    :initarg :obj-type)
   (rectan
    :accessor rectan
    :initform nil
    :initarg :rectan)))

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
   (lastx
    :accessor lastx
    :initform 0
    :initarg :lastx)
   (lasty
    :accessor lasty
    :initform 0
    :initarg :lasty)
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
    :initarg :fall)))

;; 読み込んだデータを保持しておくためのクラスを定義
(defclass tex ()
  ((renderer
    :initarg :renderer
    :initform (error "Must supply a renderer"))
   (width
    :accessor tex-width
    :initform 0)
   (height
    :accessor tex-height
    :initform 0)
   (texture
    :accessor tex-texture
    :initform nil)))

;; sdl2-image/sdl2-ttfを初期化
(defun initialize ()
  (sdl2-image:init '(:png))
  (sdl2-ttf:init)
  ;; ついでにフォントファイルもロードしておく
  (setf *font* (sdl2-ttf:open-font "./font/Myrica.TTC" 28)))

;; 各初期化処理
(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "スーパーもげオワールド"
                        :w     +screen-w+
                        :h     +screen-h+
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated :presentvsync))
         ,@body))))


;; 文字列をテクスチャクラスにデータを取得
(defmethod load-texture-from-text (renderer text)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface  (sdl2-ttf:render-text-solid *font* text 0 0 0 0)))
        (setf width   (sdl2:surface-width surface))
        (setf height  (sdl2:surface-height surface))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

;; レンダリング処理
(defmethod render (tex x y)
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy renderer texture :dest-rect (sdl2:make-rect x y width height))))

(defun render-debug (renderer p x y)
  (with-slots (rectan) p
    (let* ((x1 (sdl2:rect-x rectan))
           (y1 (sdl2:rect-y rectan))
           (tex (load-texture-from-text renderer (format nil "x:~d y:~d" x1 y1))))
      (render tex x y))))

;; 待機処理
(defun game-delay (frame-limit)
  (let ((ticks (sdl2:get-ticks)))
    (unless (< frame-limit ticks)
      (if (> frame-limit (+ ticks *fps*))
          (sdl2:delay *fps*)
          (sdl2:delay (- frame-limit ticks))))))

;;ball描画
(defun render-ball (renderer &rest pe)
  (dolist (p pe)
    (dolist (ball (p-ball p))
      (with-slots (x y r) ball
       (sdl2-ffi.functions:filled-circle-rgba renderer x y r 255 255 255 255)))))

(defparameter *colors*
  '((255 0 0 255)
    (0 255 0 255)
    (0 0 255 255)
    (255 255 0 255)
    (255 0 255 255)
    (0 255 255 255))) 


(defun gonyu (n)
  (floor (+ n 0.5)))

(defun rect-center-x (rect)
  (+ (sdl2:rect-x rect) (floor (sdl2:rect-width rect) 2))) ; w じゃないの？

(defun update-player (p keystate)
  (with-slots (left right key-z) keystate
    (with-slots (rectan vx vy jump lasty field-w-max fall) p
      ;;(when fall
      ;;  (setf vy 1))
      (when left
        (decf (sdl2:rect-x rectan) vx))
      (when right
        (incf (sdl2:rect-x rectan) vx))
      (when (and key-z (null jump))
        (setf jump t
              fall t
              vy -18))
      ;;(when fall
        (let ((temp (sdl2:rect-y rectan)))
          (incf (sdl2:rect-y rectan) (+ (- (sdl2:rect-y rectan) lasty) vy))
          (setf lasty temp)))))
      ;;(when (> (sdl2:rect-y rectan) 480)
      ;;  (setf (sdl2:rect-y rectan) 480
      ;;        jump nil)))))

(defun update-scroll (p)
  (with-slots (rectan scroll) p
    (if (or (> (rect-center-x rectan) (+ scroll (floor +screen-w+ 2)))
            (< (rect-center-x rectan) (+ scroll (floor +screen-w+ 2))))
      (setf scroll (max 0 (- (rect-center-x rectan) (floor +screen-w+ 2)))))))

;;キャラの当たり判定
(defun chara-secchi (p field)
  (with-slots (rectan scroll jump fall lasty vy field-w-max) p
    ;;(when fall
      (let* ((p-center (rect-center-x rectan))
             (p-x (sdl2:rect-x rectan))
             (p-x2 (+ p-x *obj-w*))
             (p-y (sdl2:rect-y rectan))
             (p-y2 (+ p-y *obj-h*))) ;;キャラの下部分のy座標
        (loop for y from 0 below *tate*
            do (loop for x from 0 below field-w-max
                    do (let* ((obj (aref field y x))
                              (obj-x (- (sdl2:rect-x (rectan obj)) scroll))
                              (obj-x2 (+ obj-x *obj-w*))
                              (obj-y (sdl2:rect-y (rectan obj)))
                              (obj-y2 (+ obj-y *obj-h*))) ;;objの上部分のy座標
                          (when (or (= (obj-type obj) 1) (= (obj-type obj) 2))
                            (cond 
                              ((and (>= obj-x2 p-center obj-x) ;;キャラの下部分とobjの上部分の当たり判定
                                    (>= p-y2 obj-y)
                                    (< p-y obj-y)
                                    (>= obj-y2 p-y2)
                                    (>= obj-y2 p-y))
                              ;;(let ((temp (sdl2:rect-y rectan)))
                              ;;  (incf (sdl2:rect-y rectan) (+ (- (sdl2:rect-y rectan) lasty) vy))
                              ;;  (setf lasty temp))
                                (setf fall nil jump nil
                                      lasty (- obj-y *obj-h*)
                                      (sdl2:rect-y rectan) (- obj-y *obj-h*)
                                      vy 0)
                                (return-from chara-secchi))
                              ((and (>= obj-x2 p-center obj-x) ;;キャラのue部分とobjのsita部分の当たり判定
                                    (> p-y2 obj-y)
                                    (> p-y obj-y)
                                    (< obj-y2 p-y2)
                                    (>= obj-y2 p-y))
                                (setf (sdl2:rect-y rectan) obj-y2
                                      vy (+ 18 (- (sdl2:rect-y rectan) lasty)))
                                (return-from chara-secchi))
                              ((and (>= p-x2 obj-x) ;;p-right  obj-left
                                    (< p-x obj-x)
                                    (< p-x2 obj-x2)
                                    (< p-x obj-x2)
                                    (>= obj-y2 p-y2 obj-y))
                                (setf (sdl2:rect-x rectan) (- obj-x *obj-w*))
                                (return-from chara-secchi))
                              (t (setf fall t 
                                       vy 1))))))))))

(defun create-field (field-obj p)
  (with-slots (field-w-max) p
    (let* ((tate (length field-obj))
           (yoko (length (car field-obj)))
           (arr (make-array (list tate yoko) :initial-contents field-obj))
           (arr2 (make-array (list tate yoko))))
      (setf field-w-max yoko)
      (loop for y below tate
            do (loop for x below yoko
                     do (let* ((obj-type (aref arr y x))
                               (obj-rect (sdl2:make-rect (* x 32) (* y 32)
                                                         *obj-w* *obj-h*))
                               (color
                                (cond
                                  ((= obj-type 0)
                                   (list 0 255 255 255))
                                  ((= obj-type 1)
                                   (list 115 64 41 255))
                                  ((= obj-type 2)
                                   (list 255 255 0 255)))))
                          (setf (aref arr2 y x)
                           (make-instance 'obj :rectan obj-rect
                                               :color color
                                               :obj-type obj-type)))))
      arr2)))
  
;;フィールド表示
(defun render-field (renderer field p)
  (with-slots (scroll field-w-max) p
    (loop for y from 0 below *tate*
          do (loop for x from 0 below field-w-max
                   do (with-slots (rectan color obj-type) (aref field y x)
                        (apply #'sdl2:set-render-draw-color renderer color)
                        (sdl2:render-fill-rect renderer (sdl2:make-rect (- (* x *obj-w*) scroll) (* y *obj-h*)
                                                                        *obj-w* *obj-h*)))))
    ;;debug
    (loop for y from 0 below *tate*
          do (loop for x from 0 below field-w-max
                   do (with-slots (rectan color obj-type) (aref field y x)
                        (when (= obj-type 2)
                          (let ((tex (load-texture-from-text renderer (format nil "x:~d y:~d" (- (* x *obj-w*) scroll) (* y *obj-h*)))))
                            (render tex (- (* x *obj-w*) scroll) (* y *obj-h*)))))))))

;;キャラクター表示？
(defun render-obj (renderer p)
  (with-slots (rectan scroll) p
    (sdl2:set-render-draw-color renderer 0 0 255 255)
    (sdl2:render-fill-rect renderer (sdl2:make-rect (- (sdl2:rect-x rectan) scroll)
                                                    (sdl2:rect-y rectan)
                                                    (sdl2:rect-width rectan)
                                                    (sdl2:rect-height rectan)))))

;; メイン関数
(defun main ()
  (with-window-renderer (window renderer)
    (initialize)
    (let* ((current-key-state (make-instance 'key-state))
           (p (make-instance 'chara :vx 2 :vy 0 :lasty 480 ;;キャラ作成
                                    :rectan (sdl2:make-rect 0 (- 640 160) 32 32)))
           (field1 (create-field *field1-obj* p)) ;;フィールドの配列作成
           (fps (+ (sdl2:get-ticks) *fps*)))
      (sdl2:with-event-loop (:method :poll)
        (:keydown (:keysym keysym)
                  (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit)
                      (update-key-state keysym t current-key-state)))
        (:keyup (:keysym keysym)
                (update-key-state keysym nil current-key-state))
        (:idle ()
         ;; 画面を黒色でクリア
          (sdl2:set-render-draw-color renderer 255 255 255 255)
          (sdl2:render-clear renderer)

         (render-field renderer field1 p)
         (render-obj renderer p)
         (update-player p current-key-state)
         (chara-secchi p field1)
         (update-scroll p)
        
          (render-debug renderer p 50 50)
               ;; レンダリング結果を画面に反映
         (sdl2:render-present renderer)

         (game-delay fps)
         (setf fps (+ (sdl2:get-ticks) *fps*)))
        (:quit ()
          (sdl2-image:quit)
          (sdl2-ttf:close-font *font*)
          (sdl2-ttf:quit)
          t)))))

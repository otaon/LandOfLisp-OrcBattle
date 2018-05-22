;;; ----------------------------------------------------------------
;;; ゲーム状態
;;; ----------------------------------------------------------------
;;; プレイヤーのグローバル変数
(defparameter *player-health* nil)      ; プレイヤーの体力
(defparameter *player-agility* nil)     ; プレイヤーの素早さ
(defparameter *player-strength* nil)    ; プレイヤーの力

;;; モンスターのグローバル変数
(defparameter *monsters* nil)           ; モンスター達の情報
(defparameter *monster-builders* nil)   ; モンスターを作成する関数のリスト
(defparameter *monster-num* 12)         ; モンスターの数


;;; ----------------------------------------------------------------
;;; ユーティリティ
;;; ----------------------------------------------------------------
(defun randval (n)
  "1からnまでのランダム値を返す"
  (1+ (random (max 1 n))))


;;; ----------------------------------------------------------------
;;; ゲームのメイン関数
;;; ----------------------------------------------------------------
(defun orc-battle ()
  "ゲームを実行する"
  ;; モンスター達の情報を初期化する
  (init-monsters)
  ;; プレーヤーの情報を初期化する
  (init-player)
  ;; ゲームを実行する
  (game-loop)
  ;; プレーヤーが死んでたらゲームオーバー
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  ;; モンスター達が死んでたら勝利
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  "ゲームを終了までループする"
  ;; プレーヤーかモンスター達が死ぬまでゲームを繰り返す
  (unless (or (player-dead) (monsters-dead))
    ;; プレーヤーを表示する
    (show-player)
    ;; プレーヤーの素早さにあわせて1ターンで複数回攻撃する
    ;; 素早さを適切な値で割って丸めた回数だけ攻撃する
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      ;; モンスター達が死ぬまで攻撃を繰り返す
      (unless (monsters-dead)
        ;; モンスター達を表示する
        (show-monsters)
        ;; プレーヤーが攻撃する
        (player-attack)))
    ;; 改行する
    (fresh-line)
    ;; まだ死んでいないモンスターは攻撃する
    (map 'list
         (lambda (m)
           (unless (monster-dead m)
             (monster-attack m)
             (princ (concatenate 'string " (health:" (write-to-string *player-health*) ")"))
             (fresh-line)))
         *monsters*)
    (game-loop)))


;;; ----------------------------------------------------------------
;;; プレーヤー
;;; ----------------------------------------------------------------
(defun init-player ()
  "プレーヤーを管理する関数"
  ;; プレーヤーの体力を設定
  (setf *player-health* 30)
  ;; プレーヤーの素早さを設定
  (setf *player-agility* 30)
  ;; プレーヤーの力を設定
  (setf *player-strength* 30))

(defun player-dead ()
  "プレーヤーが死んだかどうか判定する"
  (<= *player-health* 0))

(defun show-player ()
  "プレーヤー情報を表示"
  (fresh-line)
  (princ "[ You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*)
  (princ " ]"))

(defun player-attack ()
  "プレーヤーの攻撃を処理する"
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble [r]oundhouse:")
  (case (read)
    ;; s: stab(突く)
    ;; 突きは選択したモンスター1体のみに対して攻撃する
    ;; 攻撃力は最も高い
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    ;; d: double swing(ダブルスイング)
    ;; ダブルスイングは2回攻撃するが突きより威力が低い
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         ;; モンスターが全滅していたら2回目の攻撃はしない
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    ;; r(他のコマンドミスもここ): roundhouse swing(なぎ倒す)
    ;; プレーヤーの力に応じた回数攻撃するが、攻撃力は1
    ;; 攻撃対象は毎回ランダム
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))


;; プレーヤーの攻撃に使う補助関数
(defun random-monster ()
  "生きているモンスターからランダムに1体選ぶ"
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  "生きているモンスターをプレーヤーに選ばせる"
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "This is not a valid monster number.")
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (princ "That monster is already dead.")
                     (pick-monster))
              m)))))


;;; ----------------------------------------------------------------
;;; モンスターの一般情報
;;; ----------------------------------------------------------------
(defun init-monsters ()
  "モンスターを作成して*monsters*に格納する"
  (setf *monsters*
        ;; このmapではmake-arrayした配列を使わず、
        ;; 新たにモンスターの配列を作って返す
        (map 'vector
             (lambda (x)
               (declare (ignore x))
               ;; モンスタービルダをランダムに選んで、モンスターを生成する
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (m)
  "モンスターが死んだかどうか確認する"
  (<= (monster-health m) 0))

(defun monsters-dead ()
  "モンスターが全滅したか確認する"
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  "全てのモンスターを表示する"
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "  ")
           (princ (incf x)) ; モンスターのID
           (princ ". ")
           ;; モンスターが死んでいたら**dead**とだけ表示する
           ;; モンスターが生きていたら体力と諸情報を表示する
           (if (monster-dead m)
               (princ "**dead**")
               (progn (princ "(Health=")
                      (princ (monster-health m))
                      (princ ") ")
                      (monster-show m))))
         *monsters*)))

;;; モンスターのジェネリックな部分を定義
(defstruct monster
  "モンスターの情報を定義する"
  ;; health: 体力 既定値は1〜10までのランダム値
  (health (randval 10)))

(defmethod monster-hit (m x)
  "モンスターが攻撃を受けた時の処理をする
   引数の型にマッチするmonster-hitを呼び出す
   本関数は仮想メソッド"
  ;; モンスターの体力をxだけ減らす
  (decf (monster-health m) x)
  ;; モンスターが死んでいたらその旨を表示する
  ;; モンスターが生きていたら削った体力を表示する
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "! "))
      (progn (princ "You hit the ")
             (princ (type-of m))
             (princ ", knocking off ")
             (princ x)
             (princ " health points! "))))

(defmethod monster-show (m)
  "モンスターの名前を表示する"
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m)
  "モンスターが攻撃した時の処理をする
   ジェネリックな場合は何もしない
   各モンスターで必ず定義すべし"
  (declare (ignore m)))


;;; ----------------------------------------------------------------
;;; 各モンスターの情報
;;; ----------------------------------------------------------------

;;; オークの情報 ...................................................
;;; 体力…普通
;;; 攻撃…棍棒のレベルに応じた攻撃を繰り出す

(defstruct (orc (:include monster))
  "モンスターの情報を継承してオークの情報を定義する"
  ;; クラブのレベルを1〜8で設定する
  (club-level (randval 8)))

;;; モンスター生成器にオーク生成器を追加
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  "モンスターの情報を表示する"
  ;; オークは棍棒を持っているため棍棒のレベルも併記する
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  "オークが攻撃した時の処理"
  ;; 1〜棍棒のレベルまでの範囲で攻撃力が決まる
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))


;;; ヒドラの情報 ...................................................
;;; 体力…普通（ただし体力が頭の数と連動する）
;;; 攻撃…頭の数に応じて攻撃力が変わる
(defstruct (hydra (:include monster)))

;;; モンスター生成器にヒドラ生成器を追加
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  "ヒドラの情報を表示する"
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  "ヒドラが攻撃を受けた時の処理"
  ;; ダメージをもとにヒドラの頭を減らす
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
      (progn (princ "You lop off ")
             (princ x)
             (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  "ヒドラが攻撃した時の処理"
  ;; ヒドラの体力=ヒドラの頭の数に応じて攻撃力を決める
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    ;; ヒドラが攻撃する毎(つまり毎ターン)に頭を1つ増やす
    (incf (monster-health m))
    (decf *player-health*)))

;;; べたべたスライムの情報 .........................................
;;; 体力…普通
;;; 攻撃…毒を吐きかけて攻撃するか、べたべたでプレイヤーの素早さを奪う

(defstruct (slime-mold (:include monster))
  "モンスターの情報を継承してスライムの情報を定義する"
  ;; べたべた度を決める
  (sliminess (randval 5)))

;;; モンスター生成器にスライム生成器を追加
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  "スライムの情報を表示する"
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  "スライムが攻撃した時の処理"
  (let ((x (randval (slime-mold-sliminess m))))
    ;; ベタベタをかけてプレーヤーの素早さを奪う
    (princ "A slime mold wraps around your legs and decreases you agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    ;; 2分の1の確率で毒を吐きかける
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

;;; 狡猾なブリガンドの情報 .........................................
;;; 体力…普通
;;; 攻撃…スリングショットか鞭で攻撃する
;;;       体力、素早さ、力のうち、一番高いパラメータを攻撃して減らす

;;; ブリガンドのパラメータは体力のみ
(defstruct (brigand (:include monster)))

;;; モンスター生成器にブリガンド生成器を追加
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  "ブリガンドが攻撃した時の処理"
  ;;; プレーヤーの最も高いパラメータを攻撃する
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (decf *player-agility* 2))
          ((- x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))



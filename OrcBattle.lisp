;;; プレイヤーのグローバル変数
(defparameter *player-health* nil)      ; プレイヤーの体力
(defparameter *player-agility* nil)     ; プレイヤーの素早さ
(defparameter *player-strength* nil)    ; プレイヤーの力

;;; モンスターのグローバル変数
(defparameter *monsters* nil)           ; モンスター達の情報
(defparameter *monster-builders* nil)   ; モンスターを作成する関数のリスト
(defparameter *monster-num* 12)         ; モンスターの数

;;; ゲームのメイン関数
(defun orc-battle ()
  "ゲームを実行する関数"
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
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

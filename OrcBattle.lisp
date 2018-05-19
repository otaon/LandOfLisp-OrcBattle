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
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

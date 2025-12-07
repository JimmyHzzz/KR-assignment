(define (domain phone-heist-adversary)
  (:requirements :strips :typing)
  (:types location)

  (:predicates
    ;; 守卫与玩家的位置
    (guard-at ?r - location)
    (player-at ?r - location)

    ;; 地图连通性，和 Prolog 里的 path/2 对应
    (adj ?from ?to - location)

    ;; 玩家是否被抓
    (captured)
  )

  ;; 守卫在地图上移动
  (:action move_guard
    :parameters (?from ?to - location)
    :precondition (and
      (guard-at ?from)
      (adj ?from ?to)
    )
    :effect (and
      (not (guard-at ?from))
      (guard-at ?to)
    )
  )
)

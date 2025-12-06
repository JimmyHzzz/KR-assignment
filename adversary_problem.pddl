(define (problem phone-heist-chaser-dyn)
  (:domain phone-heist-adversary)

  (:objects
    gate corridor office classroom storage security - location
  )

  (:init
    (guard-at classroom)
    (player-at corridor)
    (adj gate corridor)
    (adj corridor gate)
    (adj corridor office)
    (adj office corridor)
    (adj corridor classroom)
    (adj classroom corridor)
    (adj corridor storage)
    (adj storage corridor)
    (adj storage security)
    (adj security storage)
  )

  (:goal (captured))
)

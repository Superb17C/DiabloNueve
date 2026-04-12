#lang racket

(require math/statistics)

;; computed-records: (list (game-state . number))
(define computed-records empty) ; only global variable in program, used for massive performance improvement

(define IDENTITY (lambda (x) x))

(define (max-from-list lizt)
  (argmax IDENTITY lizt))

;; make-game-state: number number number number -> game-state
(define-struct game-state (mod9-total num-cards num-aces num-antes))

;; game-state=?: game-state game-state -> boolean
;; tell whether contents inside two game states are identical
(define (game-state=? game-state-1 game-state-2)
  (and (= (game-state-mod9-total game-state-1) (game-state-mod9-total game-state-2))
       (= (game-state-num-cards game-state-1) (game-state-num-cards game-state-2))
       (= (game-state-num-aces game-state-1) (game-state-num-aces game-state-2))
       (= (game-state-num-antes game-state-1) (game-state-num-antes game-state-2))))

;; ACTION-CRITERIA: (list (symbol . (game-state -> boolean)))
;; all possible actions in game variant, along with when each action is allowed to be taken, given that player is on turn
(define ACTION-CRITERIA (let ((can-bet? (lambda (game-state)
                                          (and (> (game-state-num-cards game-state) 2)
                                               (< (game-state-num-cards game-state) 6)))))
                          (list (cons 'deal (lambda (game-state)
                                              (< (game-state-num-cards game-state) 3)))
                                (cons 'stop (lambda (game-state)
                                              (> (game-state-num-cards game-state) 2)))
                                (cons 'bet-sm can-bet?)
                                (cons 'bet-md can-bet?)
                                (cons 'bet-lg can-bet?))))

;; actions-for: game-state -> (list symbol)
(define (actions-for game-state)
  (map (lambda (criterion)
         (car criterion)) ; extract only the first item, which is the name of the action, from each criterion dotted pair
       (filter (lambda (criterion)
                 ((cdr criterion) game-state)) ; check whether the criterion is currently true given the game state
               ACTION-CRITERIA)))

;; add-bet: number game-state -> game-state
;; increase number of antes in play by given amount
(define (add-bet bet-size game-state)
  (make-game-state (game-state-mod9-total game-state)
                   (game-state-num-cards game-state)
                   (game-state-num-aces game-state)
                   (+ (game-state-num-antes game-state) bet-size)))

;; can-total?: game-state number -> boolean
;; determine whether hand total can be treated as target number, using aces as value 1 or 2 as necessary
(define (can-total? game-state target)
  (or (= (game-state-mod9-total game-state) target)
      (and (> (game-state-num-aces game-state) 0)
           (can-total? (make-game-state (modulo (+ (game-state-mod9-total game-state) 1) 9)
                                        (game-state-num-cards game-state)
                                        (- (game-state-num-aces game-state) 1)
                                        (game-state-num-antes game-state))
                       target))))

;; ev-add-card: game-state -> number
;; calculate average EV of all possible 1-card draws onto hand
(define (ev-add-card game-state)
  (mean (map best-ev
             (let ((m9t (game-state-mod9-total game-state))
                   (nc (game-state-num-cards game-state))
                   (n1 (game-state-num-aces game-state))
                   (na (game-state-num-antes game-state)))
               (list (make-game-state (modulo (+ m9t 1) 9) (+ nc 1) (+ n1 1) na) ; Ace increments Ace count
                     (make-game-state (modulo (+ m9t 2) 9) (+ nc 1) n1 na) ; Two
                     (make-game-state (modulo (+ m9t 3) 9) (+ nc 1) n1 na) ; Three
                     (make-game-state (modulo (+ m9t 4) 9) (+ nc 1) n1 na) ; Four
                     (make-game-state (modulo (+ m9t 5) 9) (+ nc 1) n1 na) ; Five
                     (make-game-state (modulo (+ m9t 6) 9) (+ nc 1) n1 na) ; Six
                     (make-game-state (modulo (+ m9t 7) 9) (+ nc 1) n1 na) ; Seven
                     (make-game-state (modulo (+ m9t 8) 9) (+ nc 1) n1 na) ; Eight
                     (make-game-state m9t (+ nc 1) n1 na) ; Nine does not affect mod-9 total
                     (make-game-state (modulo (+ m9t 1) 9) (+ nc 1) n1 na)))) ; any face card has value 1
        (list 1 1 1 1 1 1 1 1 1 3))) ; relative weights of card ranks

;; ev: symbol game-state -> number
;; calculate EV of taking given action now, assuming all future actions will be optimal
(define (ev action game-state)
  (cond ((symbol=? action 'deal)
         (ev-add-card game-state))
        ((symbol=? action 'stop)
         (cond ((can-total? game-state 0) ; if multiple of 9 is reached as exact hand sum
                (if (and (= (game-state-num-cards game-state) 3) ; if no cards beyond initial deal have been drawn
                         (= (game-state-num-aces game-state) 0))
                    (/ 10041.0 9408.0) ; win automatically, potentially with bonus payout for special combo; see computation in block comment
                    (game-state-num-antes game-state))) ; else win 1:1 on ante and all subsequent bets
               ((and (can-total? game-state 6)
                     (= (game-state-num-cards game-state) 6)) ; if "6-card 6"
                0) ; push
               (else ; if player voluntarily or involuntarity ends hand without reaching winning total
                (* (game-state-num-antes game-state) -1)))) ; lose ante and all subsequent bets
        ((symbol=? action 'bet-sm)
         (ev-add-card (add-bet 1 game-state)))
        ((symbol=? action 'bet-md)
         (ev-add-card (add-bet 2 game-state)))
        ((symbol=? action 'bet-lg)
         (ev-add-card (add-bet 3 game-state)))
        (else
         (error "unrecognized game action"))))

;; lookup-best-ev: game-state -> number | boolean
;; find optimal EV for game state if previously found, false otherwise
(define (lookup-best-ev game-state)
  (let ((matching-records (filter (lambda (record)
                                    (game-state=? (car record)
                                                  game-state))
                                  computed-records)))
    (if (empty? matching-records)
        false
        (cdr (first matching-records)))))

;; best-ev: game-state -> number
;; find current EV given that player takes optimal action
(define (best-ev game-state)
  (let ((lookup-result (lookup-best-ev game-state)))
    (if (number? lookup-result)
        lookup-result
        (let ((best-ev-result (max-from-list (map (lambda (action)
                                                    (ev action game-state))
                                                  (actions-for game-state)))))
          (begin (printf "Computing EV for mod-9 total of ~a after ~a cards (~a of which are Aces) with ~a antes in play.~n"
                         (game-state-mod9-total game-state)
                         (game-state-num-cards game-state)
                         (game-state-num-aces game-state)
                         (game-state-num-antes game-state))
                 (set! computed-records (cons (cons game-state best-ev-result) computed-records)) ; store answer globally so does not need to be recomputed later
                 best-ev-result)))))

;; how-to-play: game-state -> symbol
(define (how-to-play game-state)
  (argmax (lambda (action)
            (ev action game-state))
          (actions-for game-state)))

;; print-complete-strategy: void -> void
(define (print-complete-strategy)
  (for* ((mod9-total (in-range 0 9))
         (num-cards (in-range 3 6))
         (num-aces (in-range 0 (+ num-cards 1)))
         (num-antes (in-range (- num-cards 2) (- (* num-cards 3) 7))))
    (printf "When you have a mod-9 total of ~a after ~a cards (~a of which are Aces) with ~a antes in play, you should ~a.~n"
            mod9-total
            num-cards
            num-aces
            num-antes
            (how-to-play (make-game-state mod9-total num-cards num-aces num-antes)))))

#|
expected value of hand given player's initial 3 cards have a mod-9 total of 0 without any Aces, solved for infinite decks

The 11 non-Ace ranks have mod-9 values of (2 3 4 5 6 7 8 0 1 1 1).
A starting-hand rank-permutation consists of any 3 of these values. The same value may be used more than once, and different orderings count as distinct.

To find the number of starting-hand rank-permutations totaling [x], run the following program and count the length of the output list:
(constitute-target [x]111 (list 2100 3100 4100 5100 6100 7100 8100 0100 1100 1100 1100
                                2010 3010 4010 5010 6010 7010 8010 0010 1010 1010 1010
                                2001 3001 4001 5001 6001 7001 8001 0001 1001 1001 1001))

# 3-card-starting-hand rank-permutations with mod-9 total of 0
= (# rank-permutations totaling 0) + (# rank-permutations totaling 9) + (# rank-permutations totaling 18) + (# rank-permutations totaling 27, 36, 45,...)
= 1 + 118 + 28 + 0
= 147

total # ways to reach starting mod-9 total of 0
= 147 rank-permutations * 64 suit-permutations = 9408

Of these 9408 ways,
1 pays 9:1 (1 spaded 666),
3 pay 6:1 (3 suited 666),
74 pay 3:1 (60 mixed 666 + 6 spaded 234 + 6 spaded 567 + 1 spaded 333 + 1 spaded 999),
42 pay 2:1 (18 suited 234 + 18 suited 567 + 3 suited 333 + 3 suited 999),
840 pay 3:2 (360 mixed 234 + 360 mixed 567 + 60 mixed 333 + 60 mixed 999),
and the remaining 8448 pay 1:1 (9408 - (1 + 3 + 74 + 42 + 840) = 9408 - 960).

EV from title
= sum(# ways[i] * payout[i]) / total # ways
= (1 * 9 + 3 * 6 + 74 * 3 + 42 * 2 + 840 * 3/2 + 8448 * 1) / 9408
= 10041 / 9408, or about 1.0673 antes.
|#
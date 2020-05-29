(define DECK '(0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 20 20 20 20 30 40))
(define NAMES  '(#\3 #\4 #\5 #\6 #\7 #\8 #\9 "10" #\J #\Q #\K #\A #\2 #\B #\R))
(define VALUES '(  0   1   2   3   4   5   6   7    8   9  10  11  20  30  40))
(define CARDS   (map cons NAMES VALUES))
(define SHUFFLE_TIMES 100)
(define shuffled_deck '())
(define john_inhand '())
(define mary_inhand '())
(define lord_inhand '())

(define print
  (lambda (l)
    (for-each display l)
    (newline)))

(define print_cards
  (lambda (cards)
    (print (vl2nl cards '() 0))))

(define shuffle
  (lambda (lst times)
    (if (= times 0) lst
      (shuffle (xchange lst (random (length lst)) (random (length lst))) (- times 1)))))

(define xchange
  (lambda (lst index_a index_b)
    (let ((item_a (list-ref lst index_a)) (item_b (list-ref lst index_b)))
      (replace (replace lst index_a item_b) index_b item_a))))

(define replace
  (lambda (lst index item)
    (append (append (list-head lst index) (list item)) (list-tail lst (+ index 1)))))

(define one2ten
  (lambda (lst)
    (if (= (get_one_index lst 0) -1) lst
      (one2ten (replace lst (get_one_index lst 0) "10")))))

(define get_one_index
  (lambda (lst index)
    (if (= index (length lst)) -1    
      (begin
        (if (and (char? (list-ref lst index)) (char=? (list-ref lst index) #\1)) index
          (get_one_index lst (+ 1 index)))))))
      
(define remove_zero
  (lambda (lst index)
    (if (= index (length lst)) lst    
      (begin
        (if (char=? (list-ref lst index) #\0)
          (remove_zero (append (list-head lst index) (list-tail lst (+ index 1))) index)
          (remove_zero lst (+ index 1)))))))  
          
(define n2v
  (lambda (name index)
    (let ((item (list-ref CARDS index)))
      (if (or (and (char? name) (char? (car item)) (char=? (car item) name)) (and (string? name) (string? (car item)) (string=? (car item) name)))
        (cdr item)
        (n2v name (+ index 1))))))

(define v2n
  (lambda (value index)
    (if (= (cdr (list-ref CARDS index)) value)
      (car (list-ref CARDS index))
      (v2n value (+ index 1)))))

(define nl2vl
  (lambda (nl vl index)
    (if (= index (length nl)) vl
      (nl2vl nl (append vl (list (n2v (list-ref nl index) 0))) (+ index 1)))))

(define vl2nl
  (lambda (vl nl index)
    (if (= index (length vl)) nl
      (vl2nl vl (append nl (list (v2n (list-ref vl index) 0))) (+ index 1)))))   

(define str2nl
  (lambda (str)
    (one2ten (remove_zero (string->list str) 0))))

(define str2vl
  (lambda (str)
    (nl2vl (str2nl str) '() 0)))

(define remove_element
  (lambda (lst element index)
    (if (= index (length lst)) lst
      (begin
        (if (= element (list-ref lst index))
          (append (list-head lst index) (list-tail lst (+ index 1)))
          (remove_element lst element (+ index 1)))))))
    
(define remove_cards
  (lambda (lst sub_lst)
    (if (= (length sub_lst) 0) lst
      (remove_cards (remove_element lst (list-ref sub_lst 0) 0) (list-tail sub_lst 1)))))

(define loop_play
  (lambda ()
    (print_cards john_inhand)
    (set! john_inhand (remove_cards john_inhand (str2vl (read-line))))
    (if (= (length john_inhand) 0) (exit 0))
    (print_cards john_inhand)
    (loop_play)))

;main
(set! shuffled_deck (shuffle DECK SHUFFLE_TIMES))
(set! john_inhand (sublist shuffled_deck 0 17))
(set! mary_inhand (sublist shuffled_deck 17 34))
(set! lord_inhand (sublist shuffled_deck 34 54))
(set! john_inhand (sort john_inhand <))
(set! mary_inhand (sort mary_inhand <))
(set! lord_inhand (sort lord_inhand <))
(newline)
(loop_play)
      

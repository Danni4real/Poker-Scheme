(define DECK '(0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 20 20 20 20 30 40))
(define NAMES  '(#\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K #\A #\2 #\B #\R))
(define VALUES '(  0   1   2   3   4   5   6   7   8   9  10  11  20  30  40))
(define CARDS   (map cons NAMES VALUES))
(define CONS_TEN '("10" . #\T))
(define dealed_cards_tmp  '())
(define lord        '())
(define farmer_a    '())
(define farmer_b    '())
(define output_list '())
(define name_list   '())
(define value_list  '())
(define deck_remain '())

(define deal
  (lambda ()
    (let* ((card_index (random (length deck_remain))) (card (list-ref deck_remain card_index))) 
      (set! deck_remain (append (list-head deck_remain card_index) (list-tail deck_remain (+ card_index 1))))
      card)))

(define deal_cards
  (lambda (cards_num)
    (let ((i cards_num))
      (if (= i 0) 0
        (begin
          (set! dealed_cards_tmp (append dealed_cards_tmp (list (deal))))
          (set! i (- i 1))
          (deal_cards i))))))
      
(define deal_and_sort
  (lambda (cards_num)
    (set! dealed_cards_tmp  '())
    (deal_cards cards_num)
    (set! dealed_cards_tmp  (sort dealed_cards_tmp <))
    (list-copy dealed_cards_tmp)))
    
(define print
  (lambda (l)
    (for-each display l)
    (newline)))

(define replace10
  (lambda (card)
    (case card
      ((#\0) 0)
      ((#\1) (set! name_list (append name_list '(#\T))))
      (else  (set! name_list (append name_list (list card)))))))
          
(define replaceT
  (lambda (card)
    (case card
      ((#\T) (set! output_list (append output_list '(#\1 #\0))))
      (else  (set! output_list (append output_list (list card)))))))   
    
(define n2v
  (lambda (name index)
    (let ((i index))
      (if (char=? (car (list-ref CARDS i)) name)
        (cdr (list-ref CARDS i))
        (begin 
          (set! i (+ i 1))
          (n2v name i))))))

(define v2n
  (lambda (value index)
    (let ((i index))
      (if (= (cdr (list-ref CARDS i)) value)
        (car (list-ref CARDS i))
        (begin 
          (set! i (+ i 1))
          (v2n value i))))))

(define name2value
  (lambda (name)
    (set! value_list (append value_list (list (n2v name 0))))))

(define value2name
  (lambda (value)
    (set! name_list (append name_list (list (v2n value 0))))))

(define il2nl
  (lambda (l)
    (set! name_list  '())
    (for-each replace10 l)
    (list-copy name_list)))

(define nl2ol
  (lambda (l)
    (set! output_list  '())
    (for-each replaceT l)
    (list-copy output_list)))

(define nl2vl
  (lambda (l)
    (set! value_list  '())
    (for-each name2value l)
    (list-copy value_list)))

(define vl2nl
  (lambda (l)
    (set! name_list  '())
    (for-each value2name l)
    (list-copy name_list)))   

(define input2values
  (lambda (in)
    (nl2vl (il2nl in))))

(define values2output
  (lambda (val)
    (nl2ol (vl2nl val))))

(define print_cards
  (lambda (cards)
    (print (values2output cards))))

(define loop-read 
  (lambda ()
    (print_cards farmer_a)
    (print (input2values (string->list (read-line))))
    

    (loop-read)))



(set! deck_remain (list-copy DECK))
(newline)
(set! farmer_a (deal_and_sort 17))
(set! farmer_b (deal_and_sort 17))
(set! lord (deal_and_sort 20))
(loop-read)

#lang racket
(require redex)

(provide iswim-pict
         !->v-pict)

(define-language iswim
  [(M N L K) V
             (M M)
             (o M ...)]
  [(X Y Z) variable-not-otherwise-mentioned]
  [b number]
  [o o1 o2]
  [o1 add1 sub1 iszero]
  [o2 + - * ↑]
  [(V U) b
         (λ X M)
         X]
  [E hole
     (E M)
     (V E)
     (o V ... E M ...)])

(define-metafunction iswim
  [(subst X_1 X_1 any_1) any_1]
  [(subst (λ X_1 any_2) X_1 any_1) (λ X_1 any_2)]
  [(subst (λ X_2 any_2) X_1 any_1)
   (λ X_3 (subst (subst any_2 X_2 X_3) X_1 any_1))
   (where X_3 ,(variable-not-in (term (X_1 any_1 any_2))
                                (term X_2)))]
  
  [(subst b X_1 any_1) b]
  [(subst X_2 X_1 any_1) X_2]
  
  [(subst (o any_2 ...) X_1 any_1)
   (o (subst any_2 X_1 any_1) ...)]
  [(subst (any_2 any_3) X_1 any_1)
   ((subst any_2 X_1 any_1) (subst any_3 X_1 any_1))]
  
  ;; Instead of the above 4, the book uses these 2:
  ;; [(subst (any_2 ...) X_1 any_1) ((subst any_2 X_1 any_1) ...)]
  ;; [(subst any_2 X_1 any_1) any_2]
  )

(define-metafunction iswim
  [(δ (+ b_1 b_2)) ,(+ (term b_1) (term b_2))]
  [(δ (* b_1 b_2)) ,(* (term b_1) (term b_2))]
  [(δ (- b_1 b_2)) ,(- (term b_1) (term b_2))]
  [(δ (↑ b_1 b_2)) ,(expt (term b_1) (term b_2))]
  [(δ (add1 b_1)) ,(+ (term b_1) 1)]
  [(δ (sub1 b_1)) ,(- (term b_1) 1)]
  [(δ (iszero 0)) (λ x (λ y x))]
  [(δ (iszero b_1)) (λ x (λ y y))])
    
(define !->v
  (reduction-relation
   iswim
   (-> ((λ X M) V)
       (subst M X V)
       βv)
   (-> (o b ...)
       (δ (o b ...))
       δ)
   with
   [(--> (in-hole E M) (in-hole E N)) (-> M N)]))

(test-equal (apply-reduction-relation* !->v '((λ x (* x x)) (+ 1 2)))
            (list (term 9)))

(define sqr (term (λ x (* x x))))

(define iswim-pict (language->pict iswim #:nts '(M V E)))
(define !->v-pict (reduction-relation->pict !->v #:style 'horizontal))

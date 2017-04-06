(module ex sobj

  (obj [o
        (obj [x 2]
             [y 3]
             [f (block
                 (assign this arg (+ (select this arg) (select this x)))
                 (+ (select this arg) (select this y)))])]
       [main (block
              (assign (select this o) arg (select (select this o) y))
              (select (select this o) f))]))

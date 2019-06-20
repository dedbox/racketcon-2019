#lang slideshow/widescreen

(require algebraic/racket/base
         (prefix-in algebraic- (only-in algebraic/prelude $))
         pict/code
         pict/color
         slideshow/text)

(define $$ algebraic-$)

;;; ----------------------------------------------------------------------------
;;; Colors

(define dark-orange (<< colorize "orange"))
(define dark-green (<< colorize (dark "green")))
(define dark-gray (<< colorize (dark "gray")))
(define dark-red (<< colorize (dark "red")))
(define commented (<< colorize comment-color))
(define-syntax base-color (μ0 (<< colorize (current-base-color))))
(define-syntax id-color (μ0 (<< colorize (current-id-color))))

(define emph (.. dark-orange bit))
(define defname (.. dark-green bit))
(define name (.. dark-red bt))
(define attention (.. base-color it))
(define code-comment (.. commented tt))
(define-syntax alert (μ str (dark-red (big (bold (t str))))))

(define (highlight pict)
  (refocus (cc-superimpose (filled-rounded-rectangle (pict-width pict)
                                                     (pict-height pict)
                                                     #:draw-border? #f
                                                     #:color "yellow")
                           pict)
           pict))

;;; ----------------------------------------------------------------------------
;;; Characters

(define-syntax || (make-code-transformer #'pipes))
(define-syntax … (make-code-transformer #'ldots))
(define-syntax ⋮ (make-code-transformer #'vdots))
(define-syntax $ (make-code-transformer #'dollar))

(define ldots (base-color (t "…")))
(define vdots (base-color (t "⋮")))
(define dollar (id-color (t "$")))
(define implies (bit "⇒"))

(define-syntax pipes (μ0 (with-font (current-code-font)
                           (with-size ((get-current-code-font-size))
                             (colorize (t "||") (current-id-color))))))

(define-syntax-rule (sans x)
  (with-font "Noto Sans" (colorize x (dark "gray"))))

;;; ----------------------------------------------------------------------------
;;; Helpers

(define-code CODE typeset-code UNSYNTAX)

(define github-logo (scale-to-fit (bitmap "github.png") (scale (t "X") 1.25)))
(define twitter-logo (scale-to-fit (bitmap "twitter.png") (scale (t "X") 2)))

;;; ----------------------------------------------------------------------------
;;; Run-Time Configuration

(current-main-font "Roboto Mono")

(get-current-code-font-size (λ () (current-font-size)))

;;; ############################################################################

;; (slide
;;  (titlet "Algebraic Racket in Action")
;;  (blank-line)
;;  (sans (t "RacketCon 2019")))

(require slideshow/play)

(play (.. (>> cellophane
              (vc-append gap-size 
                         (titlet "Algebraic Racket in Action")
                         (blank-line)
                         (sans (t "RacketCon 2019"))))
          (>> - 1.0)))

;;; ============================================================================

(define a-dialect-for
  (list (para #:fill? #f "A" (name "dialect") "for")
        (para #:fill? #f (emph "optimal") "programming" (emph "experience"))))

(define programming-is-flow-control
  (list (para #:fill? #f "Programming" (defname "is") "flow control")))

(define the-flow-book
  (frame (scale-to-fit (bitmap "./flow-book.jpg") (scale full-page 1/2))))

($$ slide #:title "Algebraic Racket is ..."
    (list 'alts
          (list ($$ list `(next
                           ,@a-dialect-for
                           next
                           ,(blank-line)
                           ,(blank-line)
                           ,@programming-is-flow-control))
                (list (hc-append (* 5 gap-size)
                                 ($$ vc-append gap-size
                                     `(,@a-dialect-for
                                       ,(blank-line)
                                       ,(blank-line)
                                       ,@programming-is-flow-control))
                                 the-flow-book)))))

;;; ............................................................................

(slide
 #:title "The Big Idea"
 'next
 (t "1. Types describe interesting structures")
 'next
 (blank-line)
 (blank-line)
 (t "2. Structures are useful without types")
 'next
 (blank-line)
 (blank-line)
 (para (it "Programming should be fun. Programs should be beautiful.")))

;;; ............................................................................

(slide (titlet "Think Structurally"))

;;; ............................................................................

(slide
 #:title "Think Structurally"
 #:timeout 1.5
 'alts
 (list (list (code ⋮)
             (code (define app1
                     (function
                       [(App t1 t2)
                        (let ([t1* (step t1)])
                          (and t1* (App t1* t2)))]
                       [_ #f]))
                   (define app2
                     (function
                       [(App v1 t2)
                        (let ([t2* (step t2)])
                          (and t2* (App v1 t2*)))]
                       [_ #f])))
             (code ⋮))
       (list (code ⋮)
             (code (define app1 (function
                                  [(App t1 t2) (let ([t1* (step t1)])
                                                 (and t1* (App t1* t2)))]
                                  [_ #f]))
                   (define app2 (function
                                  [(App v1 t2) (let ([t2* (step t2)])
                                                 (and t2* (App v1 t2*)))]
                                  [_ #f])))
             (code ⋮))
       (parameterize ([get-current-code-font-size
                       (λ () (round (* (current-font-size) 5/7)))])
         (list (code ⋮)
               (code (define app1 (function [(App t1 t2) (let ([t1* (step t1)]) (and t1* (App t1* t2)))] [_ #f]))
                     (define app2 (function [(App v1 t2) (let ([t2* (step t2)]) (and t2* (App v1 t2*)))] [_ #f])))
               (code ⋮)))))

;;; ............................................................................

(slide
 #:title "Think Structurally"
 (code (define-steps
         ⋮
         [app1 (App t1 t2) (sub-step t1 t1* (App t1* t2))]
         [app2 (App v1 t2) (sub-step t2 t2* (App v1 t2*))]
         ⋮)))

;;; ............................................................................

(define prelude-long-names
  (vc-append (code curry)
             (code curryr)
             (code values)
             (code cons)
             (code list*)
             (code append)
             (code compose)
             (code conjoin)
             (code disjoin)))

(define prelude-short-names
  (vl-append (code (code:line >> (code:comment "or >>*")))
             (code (code:line << (code:comment "or <<*")))
             (code id)
             (code ::)
             (code ::*)
             (code ++)
             (code ..)
             (code &&)
             (code ||)))

(slide (titlet "Prelude"))

;;; ............................................................................

(slide
 #:title "Common Function Aliases"
 'alts
 (list (list prelude-long-names)
       (list (ht-append prelude-long-names
                        (blank (* 10 gap-size) 0)
                        prelude-short-names))))

;;; ............................................................................

(slide
 #:title "Composed Curry Totem"
 (code ((.. (>> $ id)
            (>> map add1)
            (>> map syntax-e)
            list)
        #'1 #'2 #'3)))

;;; ............................................................................

(slide
 #:title "Growing a Totem"
 #:timeout 1.5
 'alts
 (list (list (code #'(1 2 3))
             (blank-line)
             (code-comment "#<syntax (1 2 3)>"))
       (list (code (syntax-e #'(1 2 3)))
             (blank-line)
             (code-comment "'(#<syntax 1> #<syntax 2> #<syntax 3>)"))
       (list (code (map syntax-e (syntax-e #'(1 2 3))))
             (blank-line)
             (code-comment "'(1 2 3)"))
       (list (code (map add1 (map syntax-e (syntax-e #'(1 2 3)))))
             (blank-line)
             (code-comment "'(2 3 4)"))
       (list (code ($ id (map add1 (map syntax-e (syntax-e #'(1 2 3))))))
             (blank-line)
             (code-comment "2")
             (code-comment "3")
             (code-comment "4"))))

;;; ............................................................................

(slide
 #:title "Growing a Totem"
 #:timeout 1.5
 'alts
 (list (list (code ($ id (map add1 (map syntax-e (syntax-e #'(1 2 3)))))))
       (list (code ((>> $ id)
                    (map add1 (map syntax-e (syntax-e #'(1 2 3)))))))
       (list (code ((.. (>> $ id)
                        (>> map add1))
                    (map syntax-e (syntax-e #'(1 2 3))))))
       (list (code ((.. (>> $ id)
                        (>> map add1)
                        (>> map syntax-e))
                    (syntax-e #'(1 2 3)))))
       (list (code ((.. (>> $ id)
                        (>> map add1)
                        (>> map syntax-e)
                        list)
                    #'1 #'2 #'3)))))

;;; ............................................................................

(slide
 #:title "Growing a Totem"
 (code (define args+1->values
         (.. (>> $ id)
             (>> map add1)
             (>> map syntax-e)
             list))
       (args+1->values #'1 #'2 #'3)))

;;; ............................................................................

(slide
 #:title "Conjunctions and Disjunctions"
 (para (code (define (syntax-singleton? x)
               ((|| (&& syntax?
                        (.. syntax-singleton? syntax-e))
                    (&& pair?
                        (.. syntax? car)
                        (.. null? cdr)))
                x))))
 (blank-line)
 (para (code (code:line (syntax-singleton? #'(1)) (code:comment "↝ #t"))
             (code:line (syntax-singleton? #'1) (code:comment "↝ #f")))))

;;; ============================================================================

(slide
 #:title "A New Beginning"
 (para #:fill? #f "Racket is a" (attention "programming language"))
 (blank-line)
 (blank-line)
 (dark-gray (t "Code is code, and data is data.")))

;;; ============================================================================

(slide
 #:title "The Call to Adventure"
 'next
 (t "What Is Language-Oriented Programming?")
 'next
 (blank-line)
 (alert "MAGIC"))

;;; ............................................................................

(slide
 #:title "The Call to Adventure"
 (t "How Do I Make Racket Work for Me?")
 'next
 (blank-line)
 'alts
 (list (list (t "Algebraic Data Types     "))
       (list (para #:fill? #f "Algebraic Data" (dark-red (t "Structures"))))))

;;; ............................................................................

(slide
 #:title "Algebraic Data"
 #:layout 'top
 'next
 (para #:fill? #f "A" (defname "sum") "is an enumeration of" (defname "product constructors"))
 'next
 (blank-line)
 (code (data Void ())
       (data Unit (Unit))
       (data Maybe (Nothing Just)))
 'next
 (blank-line)
 (ht-append
  (* 8 gap-size)
  (vl-append (item #:fill? #f "complete" (dark-gray implies) (name "Eq"))
             (item #:fill? #f "ordered" (dark-gray implies) (name "Ord")))
  (vl-append (item #:fill? #f "parsable" (dark-gray implies) (name "Read"))
             (item #:fill? #f "printable" (dark-gray implies) (name "Show")))))

;;; ............................................................................

(slide
 #:title "Data Leads to Functions"
 'next
 (code (data VizCommand ((code:comment "run-time state")
                         Dump Load Reset Pause Unpause Zoom Pan
                         (code:comment "nodes")
                         Node Set Add Drop
                         (code:comment "edges")
                         Edge Update Connect Disconnect)
             VizResult (FAIL OK))))

;;; ............................................................................

(slide
 #:title "Data Leads to Functions"
 (parameterize ([get-current-code-font-size (λ () (round (* (current-font-size) 3/5)))])
   (code
    (define/public command
      (function
        (code:comment "run-time environment")
        [Dump (get-state)]
        [(Load state) (set-state state)]
        [Reset (reset!)]
        [Pause (set! paused? #t) (OK)]
        [Unpause (set! paused? #f) (OK)]
        [(Zoom 'in) (zoom Δzoom) (OK)]
        [(Zoom 'out) (zoom (- Δzoom)) (OK)]
        [(Pan Δx Δy) (pan Δx Δy)]
        (code:comment "nodes")
        [(Node name) (get-node name)]
        [(Set name proc pos) (set-node name proc pos)]
        [(Add name proc pos) (add-node name proc pos)]
        [(Drop name) (drop-node name)]
        (code:comment "edges")
        [(Edge from-node to-node) (get-edge from-node to-node)]
        [(Update from-node to-node proc) (set-edge from-node to-node proc)]
        [(Connect from-node to-node proc) (add-edge from-node to-node proc)]
        [(Disconnect from-node to-node) (drop-edge from-node to-node)]
        (code:comment "else")
        [_ FAIL])))))

;;; ............................................................................

(slide
 #:title "Data Leads to Functions"
 (code (send surface Pause))
 (blank-line)
 (code (send surface (Zoom 'in)))
 (blank-line)
 (code (send surface (Add 'node1 (λ (canvas) …) #f))))

;;; ............................................................................

(slide
 #:title "Functions Lead to Macros"
 'next
 (code (define-syntax event-let
         (μ* (([var:id evt] ...+) body-evt ...+)
           (bind evt ... (λ (var ...) (seq body-evt ...)))))))

;;; ............................................................................

(slide
 #:title "Functions Lead to Macros"
 (code (define-syntax event-let*
         (macro*
           [(() body-evt ...+) (seq body-evt ...)]
           [(([var:id evt] . bindings) body-evt ...+)
            (bind evt (φ x (event-let* bindings body-evt ...)))]))))

;;; ............................................................................

(get-current-code-font-size (λ () (round (* (current-font-size) 5/6))))

(slide
 #:title "Macros Lead to Suffering"
 'next
 'alts
 (list
  (list (code
         (define-syntax with-instance
           (macro*
             [(instance-id:id expr ...+)
              (with-instance [|| instance-id] expr ...)]
             [([prefix:id instance-id:id] expr ...+)
              #:if (instance-id? #'instance-id)
              #:do [(define members (instance-members #'instance-id))
                    (define ids (map car members))]
              #:with (id ...) ids
              #:with (id/prefix ...) (map (prepend this-syntax #'prefix) ids)
              #:with (def ...) (map cadr members)
              (letrec-values ([(id/prefix ...)
                               (letrec-syntax ([id (μ0 def)] ...)
                                 (values id ...))])
                expr ...)]))))

  (list (CODE
         (define-syntax with-instance
           (macro*
             [(instance-id:id expr ...+)
              #,((UNSYNTAX (highlight (code replace-context this-syntax)))
                 #'(with-instance [|| instance-id] expr ...))]
             [([prefix:id instance-id:id] expr ...+)
              #:if (instance-id? #'instance-id)
              #:do [(define members (instance-members #'instance-id))
                    (define ids (map car members))
                    (UNSYNTAX (highlight (code (define re-context (>> replace-context this-syntax)))))]
              #:with (id ...) ((UNSYNTAX (highlight (code map re-context))) ids)
              #:with (id/prefix ...) (map (prepend this-syntax #'prefix) ids)
              #:with (def ...) (map ((UNSYNTAX (highlight (code .. re-context))) cadr) members)
              (letrec-values ([(id/prefix ...)
                               (letrec-syntax ([id (μ0 def)] ...)
                                 (values id ...))])
                expr ...)]))))))

(get-current-code-font-size (λ () (current-font-size)))

;;; ============================================================================

(slide
 #:title "The Inmost Cave"
 (para #:fill? #f "Racket is a" (attention "meta-programming language"))
 (blank-line)
 (blank-line)
 (dark-gray (t "Code is not code, and data is not data.")))

;;; ============================================================================

(slide
 #:title "An Ordeal"
 'next
 (t "What Is Language-Oriented Programming?")
 'next
 (blank-line)
 (alert "EXHAUSTING"))

(slide
 #:title "An Ordeal"
 (t "How Do I Stop Working for Racket?")
 'next
 (blank-line)
 'alts
 (list (list (t "     Type Classes"))
       (list (para #:fill? #f (dark-red (t "Structure")) "Classes"))))

;;; ............................................................................

(define the-monad-class
  (parameterize ([get-current-code-font-size (λ () (round (* (current-font-size) 5/6)))])
    (code (class Monad
            [>>=]
            [>>M (λ (m k) (>>= m (λ _ k)))]
            [return pure]
            [fail error]
            minimal ([>>=])))))

(define the-maybe-monad
  (parameterize ([get-current-code-font-size (λ () (round (* (current-font-size) 5/6)))])
    (code (define-syntax MaybeMonad
            (instance Monad
              [return Just]
              [>>= (function*
                     [((Just x) k) (k x)]
                     [( Nothing _) Nothing])]
              [fail (φ _ Nothing)])))))

(define maybe-monad-examples
  (parameterize ([get-current-code-font-size (λ () (round (* (current-font-size) 5/6)))])
    (code (with-instance MaybeMonad (>>= (Just 2) (.. return add1))) (code:comment "↝ (Just 3)")
          (with-instance MaybeMonad (>>=  Nothing (.. return add1))) (code:comment "↝ Nothing"))))

(slide
 #:title "Algebraic Classes"
 'next
 (para #:fill? #f "A" (defname "class") "is a collection of names")
 'next
 (blank-line)
 'alts
 (list (list the-monad-class)
       (list (ht-append (* 4 gap-size)
                        the-monad-class
                        the-maybe-monad))
       (list (ht-append (* 4 gap-size)
                        the-monad-class
                        the-maybe-monad)
             (blank-line)
             maybe-monad-examples)))

;;; ............................................................................

(slide
 #:title "Algebraic Classes: Do-Notation"
 'next
 (code (with-instance ListMonad
         (do (x) <- '(1 2)
             (y) <- '(A B)
             (return x y))) (code:comment "↝ '(1 A 1 B 2 A 2 B)"))
 'next
 (blank-line)
 (code (with-instance ValuesMonad
         (do (x '! . y) <- (λ () (id 1 '! 2))
             zs <- (λ () (id 'A 'B))
             (return x y zs))) (code:comment "↝ 1 '(2) '(A B)")))

;;; ............................................................................

(slide
 #:title "The Event Monad"
 'next
 (para (code (define-syntax EventFunctor
               (instance Functor
                 [fmap (flip handle-evt)]))))
 (blank-line)
 (para (code (define-syntax EventMonad
               (instance Monad
                 extends (EventFunctor)
                 [>>= replace-evt]
                 [return (λ xs (fmap (λ _ ($ id xs)) always-evt))])))))

;;; ............................................................................

(slide
 #:title "The Event Monad"
 (para (code (define-syntax EventApplicative
               (instance Applicative
                 extends (EventMonad)
                 [pure return]
                 [liftA2 (λ (f a b)
                           (do xs <- a
                               ys <- b
                               (return ($ f (++ xs ys)))))]))))
 'next
 (blank-line)
 (para (code (with-instance EventApplicative
               (sync (async-values
                      (pure 1) (pure 2) (pure 3) (pure 4)))))))

;;; ............................................................................

(get-current-code-font-size (λ () (round (* (current-font-size) 4/5))))

(slide
 #:title "Asynchronous Values"
 'alts
 (list (list (para (code (with-instance EventApplicative
                           (sync (async-values (pure 1) (pure 2) (pure 3) (pure 4))))))
             'next
             (blank-line)
             (para (code (define (async-values . as)
                           (if (null? as)
                               (handle-evt always-evt (λ _ (values)))
                               (replace-evt
                                (apply choice-evt
                                       (map (λ (a) (handle-evt a (λ (x) (cons a x))))
                                            as))
                                (λ (a+x)
                                  (handle-evt
                                   (apply async-values (remq (car a+x) as))
                                   (λ xs (apply values (cons (cdr a+x) xs)))))))))))

       (list (para (code (with-instance EventApplicative
                           (sync (async-values (pure 1) (pure 2) (pure 3) (pure 4))))))
             (blank-line)
             (para (code (instantiate EventApplicative))))

       (list (para (code (instantiate EventApplicative)))
             (blank)
             (para (code (sync (async-values (pure 1) (pure 2) (pure 3) (pure 4)))))
             'next
             (blank)
             (para (code (define (async-values . as)
                           (if (null? as)
                               (return)
                               (do let identified (φ a (fmap (>> :: a) a))
                                   ((a . x)) <- ($ choice-evt (map identified as))
                                   xs <- ($ async-values (remq a as))
                                   ($ return (:: x xs))))))))))

;;; ............................................................................

(define the-async-let-example
  (code (async-let ([a ch] [b ch] [c ch] [d ch])
          (list a b c d))))

(slide
 #:title "Asynchronous Let"
 #:layout 'top
 'next
 (para (code (define-syntax async-let
               (μ* (([var:id evt] ...) body ...+)
                 (let-values ([(var ...) (sync (async-values evt ...))])
                   body ...)))))
 'next
 (blank-line)
 (para (code (define ch (make-channel))
             (for/list ([x 4])
               (thread (λ () (sleep (random)) (channel-put ch x))))))
 'next
 (blank-line)
 'alts
 (list (list (para the-async-let-example))
       (list (para (ht-append (* 6 gap-size)
                              the-async-let-example
                              (code (code:comment "'(1 2 0 3)")
                                    (code:comment "'(2 0 1 3)")
                                    (code:comment "'(3 0 2 1)")))))))

(get-current-code-font-size (λ () (current-font-size)))

;;; ============================================================================

(slide
 #:title "The Elixir"
 'next
 (para #:fill? #f "Racket is a" (attention "language-oriented programming language"))
 (blank-line)
 (blank-line)
 (dark-gray (t "Code is code, and data is data.")))

;;; ============================================================================

(slide
 #:title "An Epiphany"
 'next
 (t "What Is Language-Oriented Programming?")
 'next
 (blank-line)
 (alert "TRUTH"))

(slide
 #:title "Begin the Journey"
 'next
 (t "What Can I Do for Racket?")
 'next
 (blank-line)
 (bit "Seek Truth")
 'next
 (bit "Share Stories")
 'next
 (bit "Document Modules"))

(slide
 #:title "Begin the Journey"
 (ht-append
  (* 8 gap-size)
  (vl-append (sans (t "Eric Griffis"))
             (sans (t "dedbox@gmail.com")))
  (vr-append (hc-append github-logo twitter-logo (sans (t "@dedbox")))
             (sans (t "https://dedbox.github.io")))))

#lang slideshow/widescreen

(require algebraic/racket/base
         (prefix-in algebraic- (only-in algebraic/prelude $))
         pict/code
         pict/color
         slideshow/play
         slideshow/text
         (for-syntax (only-in syntax/parse this-syntax)))

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
(define typer (.. dark-green bt))
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

(define (simple-slide title)
  (play-n
   #:name title
   #:skip-first? #t
   #:skip-last? #t
   (fade-in-out (titlet title))))

(define (simple-slide/static-title title . ps)
  (play-n
   #:title title
   #:skip-first? #t
   #:skip-last? #t
   (fade-in-out ($$ vc-append ps))))

(define (play-fade-in str . args)
  (play
   #:title str
   #:skip-first? #t
   (fade-in ($$ vc-append args))))

(define (play-fade-out str . args)
  (play
   #:title str
   #:skip-first? #t
   (fade-out ($$ vc-append args))))

(define (play-fade-in-out str . args)
  ($$ play-fade-in str args)
  (play #:title str (fade-out ($$ vc-append args))))

(define (play-fade-in-title str)
  (play
   #:title (fade-in (titlet str))
   #:name str
   #:skip-first? #t
   (λ _ (blank))))

(define (play-fade-out/title str . args)
  (play
   #:title (fade-out (titlet str))
   #:name str
   (fade-out ($$ vc-append args))))

(define (play-fade-in+out str . args)
  ($$ play-fade-in str args)
  ($$ play-fade-out str args))

(define (play-fade-in+out/title str . args)
  ($$ play-fade-in str args)
  ($$ play-fade-out/title str args))

(define (play-N str proc)
  (play-n
   #:title str
   #:skip-first? #t
   #:skip-last? #t
   proc))

;;; ----------------------------------------------------------------------------
;;; Animation

(define fade-in (φ p (>> cellophane p)))
(define fade-out (φ p (.. (>> cellophane p) (>> - 1.0))))

(define ((fade-in-out p) t1 t2)
  ((fade-in p) (* t1 (- 1.0 t2))))

;;; ----------------------------------------------------------------------------
;;; Run-Time Configuration

(current-main-font "Roboto Mono")

(get-current-code-font-size (λ () (current-font-size)))

(set-page-numbers-visible! #f)

;;; ############################################################################

(play-n
 #:name "Algebraic Racket in Action"
 #:skip-first? #t
 #:skip-last? #t
 (fade-in-out
  (vc-append gap-size 
             (titlet "Algebraic Racket in Action")
             (blank-line)
             (sans (t "RacketCon 2019")))))

;;; ============================================================================

(simple-slide "Racket is ...")

(define a-dialect-for
  (vc-append
   gap-size
   (para #:fill? #f "A" (name "dialect") "for")
   (para #:fill? #f (emph "optimal") "programming" (emph "experience"))))

(define programming-is-flow-control
  (vc-append gap-size
             (blank-line)
             (blank-line)
             (para #:fill? #f "Programming" (defname "is") (emph "flow") "control")))

(define the-flow-book
  (frame (scale-to-fit (bitmap "./flow-book.jpg") (scale full-page 1/2))))

;;; ----------------------------------------------------------------------------

(play-fade-in-title "Algebraic Racket is ...")

;;; ----------------------------------------------------------------------------

(play-N
 "Algebraic Racket is ..."
 (λ (t1 t2)
   (hc-append (* 5 gap-size)
              (vc-append ((fade-in a-dialect-for) t1)
                         (blank-line)
                         (blank-line)
                         ((fade-in programming-is-flow-control) t2))
              ((fade-in the-flow-book) t2))))

(play-fade-out/title
 "Algebraic Racket is ..."
 (hc-append (* 5 gap-size)
            (vc-append a-dialect-for
                       (blank-line)
                       (blank-line)
                       programming-is-flow-control)
            the-flow-book))

;;; ............................................................................

(play-fade-in-title "The Big Idea")

(play-n
 #:title "The Big Idea"
 #:skip-last? #t
 (λ (t1 t2 t3)
   (vc-append
    ((fade-in (para #:fill? #f "1." (typer "Types")
                    "describe interesting" (name "structures"))) t1)
    (blank-line)
    (blank-line)
    ((fade-in (para #:fill? #f "2." (name "Structures")
                    "are useful without" (typer "types"))) t2)
    (blank-line)
    (blank-line)
    ((fade-in (para (bit "Programming should be fun. Programs should be beautiful."))) t3))))

(play-fade-out/title
 "The Big Idea"
 (para #:fill? #f "1." (typer "Types") "describe interesting" (name "structures"))
 (blank-line)
 (blank-line)
 (para #:fill? #f "2." (name "Structures") "are useful without" (typer "types"))
 (blank-line)
 (blank-line)
 (para (bit "Programming should be fun. Programs should be beautiful.")))

;;; ............................................................................

(simple-slide "Think Structurally")

;;; ............................................................................

(play-fade-in-title "Think Structurally")

(play-N
 "Think Structurally"
 (fade-in-out
  (vc-append (code ⋮)
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
             (code ⋮))))

(play-N
 "Think Structurally"
 (fade-in-out
  (vc-append (code ⋮)
             (code (define app1 (function
                                  [(App t1 t2) (let ([t1* (step t1)])
                                                 (and t1* (App t1* t2)))]
                                  [_ #f]))
                   (define app2 (function
                                  [(App v1 t2) (let ([t2* (step t2)])
                                                 (and t2* (App v1 t2*)))]
                                  [_ #f])))
             (code ⋮))))

(play-N
 "Think Structurally"
 (parameterize ([get-current-code-font-size
                 (λ () (round (* (current-font-size) 5/7)))])
   (fade-in-out
    (vc-append
     (code ⋮)
     (code
      (define app1 (function [(App t1 t2) (let ([t1* (step t1)]) (and t1* (App t1* t2)))] [_ #f]))
      (define app2 (function [(App v1 t2) (let ([t2* (step t2)]) (and t2* (App v1 t2*)))] [_ #f])))
     (code ⋮)))))

(play-fade-in+out/title
 "Think Structurally"
 (code (define-steps
         ⋮
         [app1 (App t1 t2) (sub-step t1 t1* (App t1* t2))]
         [app2 (App v1 t2) (sub-step t2 t2* (App v1 t2*))]
         ⋮)))

;;; ============================================================================

(define prelude-long-names
  (vc-append (code apply)
             (code curry)
             (code curryr)
             (code values)
             (code cons)
             (code list*)
             (code append)
             (code compose)
             (code conjoin)
             (code disjoin)))

(define prelude-short-names
  (vl-append (code $)
             (code >>)
             (code <<)
             (code id)
             (code ::)
             (code ::*)
             (code ++)
             (code ..)
             (code &&)
             (code ||)))

(play-fade-in-title "Common Functions")

(play-N
 "Common Functions"
 (λ (t1 t2)
   (ht-append ((fade-in prelude-long-names) t1)
              (blank (* 10 gap-size) 0)
              ((fade-in prelude-short-names) t2))))

(play-fade-out/title
 "Common Functions"
 (ht-append prelude-long-names
            (blank (* 10 gap-size) 0)
            prelude-short-names))

;;; ............................................................................

(play-fade-in-title "Composed-Curry Totem")

(play-fade-in-out
 "Composed-Curry Totem"
 (code (.. (>> $ id)
           (>> map add1)
           (>> map syntax-e)
           list)))

(play-fade-in+out/title
 "Composed-Curry Totem"
 (code ((.. (>> $ id)
            (>> map add1)
            (>> map syntax-e)
            list)
        #'1 #'2 #'3)))

;;; ............................................................................

(play-fade-in-title "Growing a Totem")

(simple-slide/static-title
 "Growing a Totem"
 (code (syntax-e #'(1 2 3)))
 (blank-line)
 (code-comment "'(#<syntax 1> #<syntax 2> #<syntax 3>)"))

(simple-slide/static-title
 "Growing a Totem"
 (code (map syntax-e (syntax-e #'(1 2 3))))
 (blank-line)
 (code-comment "'(1 2 3)"))

(simple-slide/static-title
 "Growing a Totem"
 (code (map add1 (map syntax-e (syntax-e #'(1 2 3)))))
 (blank-line)
 (code-comment "'(2 3 4)"))

(simple-slide/static-title
 "Growing a Totem"
 (code ($ id (map add1 (map syntax-e (syntax-e #'(1 2 3))))))
 (blank-line)
 (code-comment "2")
 (code-comment "3")
 (code-comment "4"))

;;; ............................................................................

(simple-slide/static-title
 "Growing a Totem"
 (code ($ id (map add1 (map syntax-e (syntax-e #'(1 2 3)))))))

(simple-slide/static-title
 "Growing a Totem"
 (code ((>> $ id)
        (map add1 (map syntax-e (syntax-e #'(1 2 3)))))))

(play-fade-in+out
 "Growing a Totem"
 (code ((>> $ id)
        ((>> map add1)
         (map syntax-e (syntax-e #'(1 2 3)))))))

(play-fade-in+out
 "Growing a Totem"
 (code ((>> $ id)
        ((>> map add1)
         ((>> map syntax-e)
          (syntax-e #'(1 2 3)))))))

(play-fade-in+out
 "Growing a Totem"
 (code ((>> $ id)
        (>> map add1)
        (.. (>> map syntax-e))
        (syntax-e #'(1 2 3)))))

(play-fade-in+out
 "Growing a Totem"
 (code ((>> $ id)
        (.. (>> map add1)
            (>> map syntax-e))
        (syntax-e #'(1 2 3)))))

(simple-slide/static-title
 "Growing a Totem"
 (code ((.. (>> $ id)
            (>> map add1)
            (>> map syntax-e))
        (syntax-e #'(1 2 3)))))

(simple-slide/static-title
 "Growing a Totem"
 (code ((.. (>> $ id)
            (>> map add1)
            (>> map syntax-e)
            list)
        #'1 #'2 #'3)))

;;; ----------------------------------------------------------------------------

(play-fade-in-title "A New Beginning")

(play-fade-in+out/title
 "A New Beginning"
 (para #:fill? #f "Racket is a" (attention "programming language")))

(play-n
 #:name "A New Beginning"
 #:skip-first? #t
 #:skip-last? #t
 (fade-in-out
  (dark-gray (vl-append gap-size
                        (t "Before one studies macros,")
                        (t "code is code and data is data.")))))

;;; ============================================================================

(simple-slide "The Call to Adventure")

;;; ============================================================================

(play-fade-in-title "The Call to Adventure")

(play-N
 "The Call to Adventure"
 (λ (t1 t2 t3)
   (vc-append ((fade-in-out (t "What Is Language-Oriented Programming?")) t1 t3)
              (blank-line)
              ((fade-in-out (alert "MAGIC")) t2 t3))))

;;; ............................................................................

(play-N
 "The Call to Adventure"
 (λ (t1 t2 t3)
   (vc-append
    ((fade-in (t "How Do I Make Racket Work for Me?")) t1)
    (blank-line)
    (cc-superimpose
     ((fade-in-out (para #:fill? #f "Algebraic Data" (typer "Types     "))) t2 t3)
     ((fade-in     (para #:fill? #f "Algebraic Data" (name  "Structures"))) t3)))))

(play-fade-out/title
 "The Call to Adventure"
 (t "How Do I Make Racket Work for Me?")
 (blank-line)
 (para #:fill? #f "Algebraic Data" (name  "Structures")))

;;; ............................................................................

(define equiv-derived-classes
  (ht-append
   (* 8 gap-size)
   (vl-append (item #:fill? #f "complete" (dark-gray implies) (name "Eq"))
              (item #:fill? #f "ordered" (dark-gray implies) (name "Ord")))
   (vl-append (item #:fill? #f "parsable" (dark-gray implies) (name "Read"))
              (item #:fill? #f "printable" (dark-gray implies) (name "Show")))))

(define data-predicates
  (vl-append (* 2 gap-size)
             (code (data Playback (Reset Pause Unpause Zoom Pan)
                         Viewport (Zoom Pan)))
             (code (let ([input (sync ch)])
                     (cond
                       [((sum Playback?) input) (playback-handler input)]
                       [(Zoom? input) (zoom-handler input)]
                       [(Pan? Input) (pan-handler input)])))))

(define data-to-list
  (code (data->list (Zoom 'in 2.5)) (code:comment "'(Zoom in 2.5)")))

(play-fade-in-title "Algebraic Data")

(play-N
 "Algebraic Data"
 (λ (t1 t2)
   (vl-append
    gap-size
    ((fade-in
      (vl-append gap-size
                 (para #:fill? #f "A" (defname "sum")
                       "is like a list of" (attention "product") "names")
                 (blank)
                 (ht-append (* 4 gap-size)
                            (code (code:comment "Data")
                                  (data Unit (Unit)
                                        Maybe (Nothing Just)))
                            (code (code:comment "Sums")
                                  (sum Unit)
                                  (sum Maybe))))) t1)
    (blank)
    ((fade-in
      (vl-append gap-size
                 (para #:fill? #f "A" (defname "product")
                       "is like a list of unnamed" (attention "fields"))
                 (blank)
                 (ht-append (* 6 gap-size)
                            (code (code:comment "Constructors")
                                  Unit
                                  Nothing)
                            (code (code:comment "Instances")
                                  (Unit)
                                  (Just 'something))))) t2))))

(play
 #:title "Algebraic Data"
 (fade-out
  (vl-append
   gap-size
   (para #:fill? #f "A" (defname "sum")
         "is like a list of" (attention "product") "names")
   (blank)
   (ht-append (* 4 gap-size)
              (code (code:comment "Data")
                    (data Unit (Unit)
                          Maybe (Nothing Just)))
              (code (code:comment "Sums")
                    (sum Unit)
                    (sum Maybe)))
   (blank)
   (vl-append gap-size
              (para #:fill? #f "A" (defname "product")
                    "is like a list of unnamed" (attention "fields"))
              (blank)
              (ht-append (* 6 gap-size)
                         (code (code:comment "Constructors")
                               Unit
                               Nothing)
                         (code (code:comment "Instances")
                               (Unit)
                               (Just 'something)))))))

(play-fade-in-out
 "Algebraic Data"
 (vl-append (* 2 gap-size)
            (code (data A-D (A B C D)))
            (code (sort (list D C B D A) data-less-than?))
            (code (code:comment "'(A B C D D)"))))

(play-fade-in+out/title
 "Algebraic Data"
 equiv-derived-classes)

;;; ............................................................................

(simple-slide "Data Leads to Functions")

;;; ............................................................................

;; (define viz-data
;;   (code (data VizCommand ((code:comment "run-time state")
;;                           Dump Load Reset Pause Unpause Zoom Pan
;;                           (code:comment "nodes")
;;                           Node Set Add Drop
;;                           (code:comment "edges")
;;                           Edge Update Connect Disconnect)
;;               VizResult (FAIL OK))))

(define viz-command-handler
  (code
   (define/public command
     (function
      ⋮
      [Reset           (reset!)           OK]
      [Pause           (set! paused? #t)  OK]
      [Unpause         (set! paused? #f)  OK]
      [(Zoom 'in Δz)   (zoom Δz)          OK]
      [(Zoom 'out Δz)  (zoom (- Δz))      OK]
      [(Pan Δx Δy)     (pan Δx Δy)        OK]
      ⋮
      [_ FAIL]))))

(define define-http-request-parser
  (vl-append (* 2 gap-size)
             (code (data HttpRequest (GET PUT POST)))
             (code (define parse-HttpRequest
                     (φ (#rx"^([^ ]+) ([^ ]+) HTTP/([^\r\n]+)"
                         (#rx"^(?:GET|PUT|POST)$"    method)
                         (#rx"^([^?]+)(?:\\?(.*))?$" uri-path uri-params)
                         (#rx"^[0-9]\\.[0-9]$"       http-version))
                       ((eval (read (open-input-string method))
                              (variable-reference->namespace
                               (#%variable-reference parse-HttpRequest)))
                        uri-path uri-params http-version))))))

(define use-http-request-parser
  (code (parse-HttpRequest "GET /r/s?q=123&p=4 HTTP/1.0\r\n\r\n")
        (code:comment "(GET \"/r/s\" \"q=123&p=4\" \"1.0\")")))

(play-fade-in-title "Data Leads to Functions")

;; (play-fade-in-out
;;  "Data Leads to Functions"
;;  viz-data)

(play-fade-in-out
 "Data Leads to Functions"
 viz-command-handler)

(play-N
 "Data Leads to Functions"
 (λ (t1 t2)
   (vl-append (* 2 gap-size)
              ((fade-in define-http-request-parser) t1)
              ((fade-in use-http-request-parser) t2))))

(play-fade-out/title
 "Data Leads to Functions"
 (vl-append (* 2 gap-size)
            define-http-request-parser
            use-http-request-parser))

;;; ............................................................................

(simple-slide "Functions Lead to Macros")

;;; ............................................................................

(define use-event-let
  (code (event-let ([a (event 1)]
                    [b (event 2)]
                    [c (event 3)])
          (+ a b c)) (code:comment "↝ 6")))

(define def-event-let
  (code (define-syntax event-let
          (μ* (([x:id evt] ...+) body ...+)
            (sync (handle-evt (list-evt evt ...)
                              (λ (x ...) body ...)))))))

(define use-event-let*
  (code (event-let* ([a (event 1)]
                     [b (event (+ a 2))]
                     [c (event (+ b 3))])
          c) (code:comment "↝ 6")))

(define def-event-let*
  (code (define-syntax event-let*
          (macro*
            [(() body ...+) (begin body ...)]
            [(([x:id evt] . rest) body ...+)
             (sync (handle-evt evt (φ x (event-let* rest body ...))))]))))

(play-fade-in-title "Functions Lead to Macros")

(play-N
 "Functions Lead to Macros"
 (λ (t1 t2 t3)
   ((fade-out (vl-append (* 2 gap-size)
                         ((fade-in use-event-let) t1)
                         ((fade-in def-event-let) t2)))
    t3)))

(play-fade-in
 "Functions Lead to Macros"
 (vl-append (* 2 gap-size)
            use-event-let*
            (ghost def-event-let*)))

(play-N
 "Functions Lead to Macros"
 (λ (t1)
   (vl-append (* 2 gap-size)
              use-event-let*
              ((fade-in def-event-let*) t1))))

(play-fade-out/title
 "Functions Lead to Macros"
 (vl-append (* 2 gap-size)
            use-event-let*
            def-event-let*))

;;; ............................................................................

(simple-slide "Macros Lead to Suffering")

;;; ............................................................................

(get-current-code-font-size (λ () (round (* (current-font-size) 5/6))))

(define broken-with-instance
  (code
   (define-syntax with-instance
     (macro*
       [(instance-id:id expr ...+)
        (with-instance [|| instance-id] expr ...)]
       (code:line)
       [([prefix:id instance-id:id] expr ...+)
        #:if (instance-id? #'instance-id)
        #:do [(define ids …)]
        #:with (id ...) ids
        #:with (def ...) …
        #:with (id/prefix ...) …
        (letrec-values ([(id/prefix ...)
                         (letrec-syntax ([id (μ0 def)] ...)
                           (values id ...))])
          expr ...)]))))

(define patched-with-instance
  (CODE
   (define-syntax with-instance
     (macro*
       [(instance-id:id expr ...+)
        #,((UNSYNTAX (highlight (code replace-context this-syntax)))
           #'(with-instance [|| instance-id] expr ...))]
       (code:line)
       [([prefix:id instance-id:id] expr ...+)
        #:if (instance-id? #'instance-id)
        #:do [(define ids …)
              (UNSYNTAX (highlight (code (define re-context (>> replace-context this-syntax)))))]
        #:with (id ...) ((UNSYNTAX (highlight (code map re-context))) ids)
        #:with (def ...) (map ((UNSYNTAX (highlight (code .. re-context))) cadr) …)
        #:with (id/prefix ...) …
        (letrec-values ([(id/prefix ...)
                         (letrec-syntax ([id (μ0 def)] ...)
                           (values id ...))])
          expr ...)]))))

(define fixed-with-instance
  (CODE
   (define-syntax with-instance
     (macro*
       [(instance-id:id body ...+)
        #,((UNSYNTAX (highlight (CODE #%rewrite this-syntax)))
            `(with-instance [|| ,#'instance-id] (UNSYNTAX (tt ".")) ,#'(body ...)))]
       (code:line)
       [([prefix:id instance-id:id] body ...+)
        #:if (instance-id? #'instance-id)
        #:do [(define ids …)]
        #:with (id ...) ids
        #:with (def ...) …
        #:with (id/prefix ...) …
        (let-values ([(id/prefix ...)
                      ((UNSYNTAX (highlight (code syntax-parameterize))) ([id (μ0 def)] ...)
                        (values id ...))])
          body ...)]))))

(play-fade-in-title "Macros Lead to Suffering")

(play-fade-in-out
 "Macros Lead to Suffering"
 broken-with-instance)

(play-fade-in+out/title
 "Macros Lead to Suffering"
 patched-with-instance)

(get-current-code-font-size (λ () (current-font-size)))

(play-fade-in+out/title
 "Macros Lead to Suffering"
 fixed-with-instance)

;;; ============================================================================

(define racket-is-a-meta-programming-language
  (para #:fill? #f "Racket is a" (attention "meta-programming language")))

(define code-is-not-code/data-is-not-data
  (dark-gray
   (vl-append gap-size
              (t "After a glimpse at the truth of macros,")
              (t "code is not code and data is not data."))))

(play-fade-in-title "The Inmost Cave")

(play-fade-in+out/title
 "The Inmost Cave"
 racket-is-a-meta-programming-language)

(play-n
 #:name "The Inmost Cave"
 #:skip-first? #t
 #:skip-last? #t
 (fade-in-out code-is-not-code/data-is-not-data))

;;; ............................................................................

(play-fade-in-title "An Ordeal")

(play-N
 "An Ordeal"
 (λ (t1 t2 t3)
   (vc-append ((fade-in-out (t "What Is Language-Oriented Programming?")) t1 t3)
              (blank-line)
              ((fade-in-out (alert "EXHAUSTING")) t2 t3))))

(define how-do-I-stop-working
  (t "How Do I Stop Working for Racket?"))

(play-N
 "An Ordeal"
 (λ (t1 t2 t3)
   (vc-append ((fade-in how-do-I-stop-working) t1)
              (blank-line)
              (para #:fill? #f
                    ((fade-in (cc-superimpose
                               ((fade-in-out (typer "     Type")) t2 t3)
                               ((fade-in     (name  "Structure")) t3)))
                     t2)
                    ((fade-in (t "Classes")) t2)))))

(play-fade-out/title
 "An Ordeal"
 (vc-append how-do-I-stop-working
            (blank-line)
            (para #:fill? #f (name  "Structure") "Classes")))

;;; ............................................................................

(play-fade-in-title "Algebraic Classes")

(play-fade-in-out
 "Algebraic Classes"
 (vc-append
  (* 4 gap-size)
  (para #:fill? #f "A" (defname "class") "is a collection of" (emph "names"))
  (parameterize
      ([get-current-code-font-size (λ () (round (* (current-font-size) 5/6)))])
    (code (class Monad
            [>>=]
            [>>M …]
            [return …]
            [fail …]
            minimal ([>>=]))))))

(play-fade-in-out
 "Algebraic Classes"
 (vc-append
  (* 4 gap-size)
  (para #:fill? #f "An" (defname "instance") "is a collection of" (emph "bindings"))
  (parameterize
      ([get-current-code-font-size (λ () (round (* (current-font-size) 5/6)))])
    (code (define-syntax MaybeMonad
            (instance Monad
              extends (MaybeApplicative)
              [return …]
              [>>= …]
              [fail …]))))))

(play-fade-in+out/title
 "Algebraic Classes"
 (vc-append
  (* 4 gap-size)
  (para #:fill? #f "Classes and instances are" (emph "lexically scoped"))
  (parameterize
      ([get-current-code-font-size (λ () (round (* (current-font-size) 5/6)))])
    (vl-append
     gap-size
     (code (with-instance MaybeMonad
             (>>= (Just 2) (.. return add1))) (code:comment "↝ (Just 3)"))
     (code (with-instance MaybeMonad
             (>>=  Nothing (.. return add1))) (code:comment "↝ Nothing"))))))

;;; ............................................................................

(play-fade-in-title "Do-Notation")

(play-fade-in-out
 "Do-Notation"
 (vl-append gap-size
            (code (with-instance ListMonad
                    (do (x) <- '(1 2)
                        (y) <- '(A B)
                        (return x y))))
            (code (code:comment "↝ '(1 A 1 B 2 A 2 B)"))))

(play-fade-in+out/title
 "Do-Notation"
 (vl-append gap-size
            (code (with-instance ValuesMonad
                    (lazy-do (x . ys) <- (id 1 2 3)
                             (return x ys))))
            (code (code:comment "↝ 1 '(2 3)"))))

;;; ............................................................................

;; (simple-slide "Truthiness")

;; (play-fade-in-title "Truthiness")

;; (play-fade-in-out
;;  "Truthiness"
;;  (code (define (world-server max-fps)
;;          (define agents (make-hash))
;;          (define next-key 1)
;;          (code:line)
;;          (define (start-agent initial-state)
;;            (hash-set! agents next-key initial-state)
;;            (begin0 next-key
;;              (set! next-key (add1 next-key))))
;;          (code:line)
;;          (define (get-state key)
;;            (and (hash-has-key? agents key)
;;                 (hash-ref agents key)))
;;          (code:line)
;;          ⋮ )))

;; (play-fade-in-out
;;  "Truthiness"
;;  (vl-append gap-size
;;             (code (define agents (make-hash))
;;                   (define next-key 1))
;;             (code (define (start-agent initial-state)
;;                     (hash-set! agents next-key initial-state)
;;                     (begin0 next-key
;;                       (set! next-key (add1 next-key)))))))

;; (play-fade-in-out
;;  "Truthiness"
;;  (code (define (get-state key)
;;          (and (hash-has-key? agents key)
;;               (hash-ref agents key)))))

;; (play-N
;;  "Truthiness"
;;  (λ (t1 t2 t3)
;;    (vl-append
;;     gap-size
;;     ((fade-in (code (start-agent #f) (code:comment "↝ 1"))) t1)
;;     ((fade-in (code (get-state  1) (code:comment "↝ #f"))) t2)
;;     ((fade-in (code (get-state -5) (code:comment "↝ #f"))) t3))))

;; (play-fade-out/title
;;  "Truthiness"
;;  (vl-append
;;   gap-size
;;   (code (start-agent #f) (code:comment "↝ 1"))
;;   (code (get-state  1) (code:comment "↝ #f"))
;;   (code (get-state -5) (code:comment "↝ #f"))))

;; ;;; ............................................................................

;; (simple-slide "The Truthy Monad")

;; (play-N
;;  "The Truthy Monad"
;;  (λ (t1 t2 t3)
;;    (vl-append
;;     gap-size
;;     ((fade-in (code (data Truthy (Fail)))) t1)
;;     (blank)
;;     ((fade-in (code (define truthy (φ x (or x Fail))))) t2)
;;     (blank)
;;     ((fade-in (code (define-syntax TruthyMonad
;;                       (instance Monad
;;                         [>>= (λ (x~ f)
;;                                (λ ()
;;                                  (let ([x (x~)])
;;                                    (if (Fail? x) x ((f x))))))]
;;                         [return thunk<-])))) t3))))

;; (play
;;  #:title "The Truthy Monad"
;;  (fade-out
;;   (vl-append
;;    gap-size
;;    (code (data Truthy (Fail)))
;;    (blank)
;;    (code (define truthy (φ x (or x Fail))))
;;    (blank)
;;    (code (define-syntax TruthyMonad
;;            (instance Monad
;;              [>>= (λ (x~ f)
;;                     (λ ()
;;                       (let ([x (x~)])
;;                         (if (Fail? x) x ((f x))))))]
;;              [return thunk<-]))))))

;; ;; (play-fade-in-out
;; ;;  "The Truthy Monad"
;; ;;  (vl-append
;; ;;   gap-size
;; ;;   (code (data Truthy (Fail)))
;; ;;   (code (define truthy (φ x (or x Fail))))
;; ;;   (code (define-syntax TruthyMonad
;; ;;           (instance Monad
;; ;;             [>>= (λ (x~ f)
;; ;;                    (λ ()
;; ;;                      (let ([x (x~)])
;; ;;                        (if (Fail? x) x ((f x))))))]
;; ;;             [return thunk<-])))))

;; (play-fade-in-out
;;  "The Truthy Monad"
;;  (code (define-syntax TruthyFunctor
;;          (instance Functor
;;            extends (TruthyMonad)
;;            [fmap (λ (f x~) (>>= x~ (φ x (return (f x)))))]))))

;; (play-fade-in-out
;;  "The Truthy Monad"
;;  (code (define-syntax TruthyApplicative
;;          (instance Applicative
;;            extends (TruthyFunctor)
;;            [pure return]
;;            [<*> (λ (f~ x~)
;;                   (do~ (f) <- f~
;;                        (x) <- x~
;;                        (return (f x))))]))))

;; ;;; ............................................................................

;; (simple-slide "Truthiness, Revisited")

;; (play-fade-in-title "Truthiness, Revisited")

;; (play-N
;;  "Truthiness, Revisited"
;;  (λ (t1 t2 t3)
;;    (lt-superimpose
;;     ((fade-in-out (code (define (get-state key)
;;                           (and (hash-has-key? agents key)
;;                                (hash-ref agents key))))) t1 t2)
;;     (vl-append
;;      gap-size
;;      ((fade-in (code (define (get-state key)
;;                        (lazy-do (truthy (hash-has-key? agents key))
;;                                 (hash-ref agents key))))) t2)
;;      ((fade-in (code (start-agent #f) (code:comment "↝ 1")
;;                      (get-state  1) (code:comment "↝ #f")
;;                      (get-state -5) (code:comment "↝ Fail"))) t3)))))

;; (play-fade-out/title
;;  "Truthiness, Revisited"
;;  (vl-append
;;   gap-size
;;   (code (define (get-state key)
;;           (lazy-do (truthy (hash-has-key? agents key))
;;                    (hash-ref agents key))))
;;   (code (start-agent #f) (code:comment "↝ 1")
;;         (get-state  1) (code:comment "↝ #f")
;;         (get-state -5) (code:comment "↝ Fail"))))

;;; ............................................................................

(play-fade-in-title "The Event Monad")

(play-n
 #:title "The Event Monad"
 #:skip-first? #t
 (λ (t1 t2)
   (vl-append
    gap-size
    ((fade-in
      (code (define-syntax EventFunctor
              (instance Functor
                [fmap (flip handle-evt)])))) t1)
    (blank)
    ((fade-in
      (code (define-syntax EventMonad
              (instance Monad
                extends (EventFunctor)
                [>>= replace-evt]
                [return (λ xs (fmap (λ _ ($ id xs)) always-evt))])))) t2))))

(play-fade-out
 "The Event Monad"
 (vl-append
  gap-size
  (code (define-syntax EventFunctor
          (instance Functor
            [fmap (flip handle-evt)])))
  (blank)
  (code (define-syntax EventMonad
          (instance Monad
            extends (EventFunctor)
            [>>= replace-evt]
            [return (λ xs (fmap (λ _ ($ id xs)) always-evt))])))))

(play-fade-in+out/title
 "The Event Monad"
 (code (define-syntax EventApplicative
         (instance Applicative
           extends (EventMonad)
           [pure return]
           [liftA2 (λ (f a b)
                     (do xs <- a
                         ys <- b
                         (return ($ f (++ xs ys)))))]))))

;;; ............................................................................

(play-fade-in-title "Async Values Event")

(play
 #:title "Async Values Event"
 #:skip-first? #t
 (fade-in
  (vl-append
   gap-size
   (code (sync ($ async-values-evt (build-list 10 return))))
   (blank)
   (ghost (code (code:comment "↝ 3 0 6 5 8 7 9 4 1 2"))))))

(play-n
 #:title "Async Values Event"
 #:skip-first? #t
 (λ (t1)
   (vl-append
    gap-size
    (code (sync ($ async-values-evt (build-list 10 return))))
    (blank)
    ((fade-in (code (code:comment "↝ 3 0 6 5 8 7 9 4 1 2"))) t1))))

(play-fade-out
 "Async Values Event"
 (vl-append
  gap-size
  (code (sync ($ async-values-evt (build-list 10 return))))
  (blank)
  (code (code:comment "↝ 3 0 6 5 8 7 9 4 1 2"))))

(play-fade-in-out
 "Async Values Event"
 (code (define (async-values-evt . evts)
         (if (null? evts)
             (handle-evt always-evt (λ _ (values)))
             (replace-evt
              (apply choice-evt
                     (map (λ (e) (handle-evt e (λ (x) (cons e x))))
                          evts))
              (λ (e+x)
                (handle-evt
                 (apply async-values-evt (remq (car e+x) evts))
                 (λ xs (apply values (cons (cdr e+x) xs))))))))))

(play-fade-in+out/title
 "Async Values Event"
 (code (define (async-values-evt . evts)
         (if (null? evts)
             (return)
             (do let identified (φ e (fmap (>> :: e) e))
                 ((e . x)) <- ($ choice-evt (map identified evts))
                 xs <- ($ async-values-evt (remq e evts))
                 ($ return (:: x xs)))))))

;;; ............................................................................

(play-fade-in-title "Async Event Let")

(play-N
 "Async Event Let"
 (λ (t1 t2 t3)
   (vl-append
    gap-size
    ((fade-in (code (define ch (make-channel))
                    (for ([x 10])
                      (thread (λ () (sleep (random)) (channel-put ch x)))))) t1)
    (blank)
    (blank)
    ((fade-in (code (async-event-let ([a ch] [b ch] [c ch] [d ch] [e ch])
                      …))) t2))))

(play-fade-out
 "Async Event Let"
 (vl-append
  gap-size
  (code (define ch (make-channel))
        (for ([x 10])
          (thread (λ () (sleep (random)) (channel-put ch x)))))
  (blank)
  (blank)
  (code (async-event-let ([a ch] [b ch] [c ch] [d ch] [e ch])
          …))))

(play-fade-in
 "Async Event Let"
 (code (define-syntax async-event-let
         (μ* (([x:id evt] ...) body ...+)
           (let-values ([(x ...) (sync (async-values-evt evt ...))])
             body ...)))))

(play-fade-out/title
 "Async Event Let"
 (code (define-syntax async-event-let
         (μ* (([x:id evt] ...) body ...+)
           (let-values ([(x ...) (sync (async-values-evt evt ...))])
             body ...)))))

;;; ============================================================================

(play-fade-in-title "The Elixer")

(play-fade-in+out/title
 "The Elixer"
 (para #:fill? #f "Racket is a" (attention "language-oriented programming language")))

(play-n
 #:name "The Elixer"
 #:skip-first? #t
 #:skip-last? #t
 (fade-in-out
  (dark-gray (vl-append gap-size
                        (t "After one studies macros,")
                        (t "code is code and data is data.")))))

;;; ============================================================================

(play-fade-in-title "An Epiphany")

(play-N
 "An Epiphany"
 (λ (t1 t2 t3)
   (vc-append ((fade-in-out (t "What Is Language-Oriented Programming?")) t1 t3)
              (blank-line)
              ((fade-in-out (alert "TRUTH")) t2 t3))))

;;; ............................................................................

(simple-slide "Find your Way")

;;; ............................................................................

(play-fade-in-title "Find your Way")

(play-N
 "Find your Way"
 (λ (t1 t2 t3 t4)
   (vc-append
    gap-size
    ((fade-in (t "What Can I Do for Racket?")) t1)
    (blank)
    ((fade-in (bit "Seek Truth")) t2)
    (blank)
    ((fade-in (bit "Share Stories")) t3)
    (blank)
    ((fade-in (bit "Leave a Record")) t4))))

(play
 #:title "Find your Way"
 (fade-out
  (vc-append
   gap-size
   (t "What Can I Do for Racket?")
   (blank)
   (bit "Seek Truth")
   (blank)
   (bit "Share Stories")
   (blank)
   (bit "Leave a Record"))))

;;; ............................................................................

(simple-slide "Up Next")

;;; ............................................................................

(play-fade-in-title "Up Next")

(play-N
 "Up Next"
 (λ (t1 t2 t3)
   (vc-append
    gap-size
    ((fade-in (bit "Real-time visualizations")) t1)
    (blank)
    ((fade-in (bit "Diagrams and Animation")) t2)
    (blank)
    ((fade-in (bit "Professional Audio")) t3))))

(play-fade-out/title
 "Up Next"
 (vc-append
  gap-size
  (bit "Real-time visualizations")
  (blank)
  (bit "Diagrams and Animation")
  (blank)
  (bit "Professional Audio")))

(play-fade-in-title "Begin the Journey")

(play-N
 "Begin the Journey"
 (λ (t1 t2)
   (vc-append
    (* 4 gap-size)
    ((fade-in
      (vc-append
       (* 4 gap-size)
       (ht-append
        (* 8 gap-size)
        (vl-append (sans (t "Eric Griffis"))
                   (sans (t "dedbox@gmail.com")))
        (vr-append (ghost (sans (t "EG")))
                   (hc-append github-logo twitter-logo (sans (t "@dedbox"))))))) t2)
    ((fade-in (sans (t "https://www.patreon.com/dedbox"))) t1)
    ((fade-in (sans (t "https://github.com/dedbox/racketcon-2019"))) t2))))

;; (play-fade-in
;;  "Begin the Journey"
;;  (vc-append
;;   (* 4 gap-size)
;;   (vc-append
;;    (* 4 gap-size)
;;    (ht-append (* 8 gap-size)
;;               (vl-append (sans (t "Eric Griffis"))
;;                          (sans (t "dedbox@gmail.com")))
;;               (vr-append (ghost (sans (t "EG")))
;;                          (hc-append github-logo twitter-logo (sans (t "@dedbox"))))))
;;   (sans (t "https://www.patreon.com/dedbox"))
;;   (sans (t "https://github.com/dedbox/racketcon-2019"))))

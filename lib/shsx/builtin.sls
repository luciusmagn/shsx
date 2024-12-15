;; TODO: +i is the complex number unit fuck lol
(library (shsx builtin)
  (export +a +abbr +address +area +article +aside +audio +b +base +bdi +bdo
          +blockquote +body +br +button +canvas +caption +cite +code +col
          +colgroup +data +datalist +dd +del +details +dfn +dialog +div +dl
          +dt +em +embed +fieldset +figcaption +figure +footer +form
          +h1 +h2 +h3 +h4 +h5 +h6 +head +header +html +hr +iframe
          +img +input +ins +kbd +label +legend +li +link +main +map +mark
          +meta +meter +nav +noscript +object +ol +optgroup +option +output
          +p +param +picture +pre +progress +q +rp +rt +ruby +s +samp +script
          +section +select +small +source +span +strong +style +sub +summary
          +sup +svg +table +tbody +td +template +textarea +tfoot +th +thead
          +time +title +tr +track +u +ul +var +video +wbr +<>)
  (import (rnrs)
          (shsx element))

  (define-syntax define-builtin-tags
    (syntax-rules ()
      [(_ name ...)
       (begin
         (define (name . args)
           (make-element (string->symbol
                          ;; TODO: This is retarded
                          (substring (symbol->string 'name) 1 (string-length (symbol->string 'name))))  ; remove +
                         args
                         '()))
         ...)]))

  ;; Define all the HTML elements
  (define-builtin-tags
    +a +abbr +address +area +article +aside +audio +b +base +bdi +bdo
    +blockquote +body +br +button +canvas +caption +cite +code +col
    +colgroup +data +datalist +dd +del +details +dfn +dialog +div +dl
    +dt +em +embed +fieldset +figcaption +figure +footer +form
    +h1 +h2 +h3 +h4 +h5 +h6 +head +header +html +hr +iframe
    +img +input +ins +kbd +label +legend +li +link +main +map +mark
    +meta +meter +nav +noscript +object +ol +optgroup +option +output
    +p +param +picture +pre +progress +q +rp +rt +ruby +s +samp +script
    +section +select +small +source +span +strong +style +sub +summary
    +sup +svg +table +tbody +td +template +textarea +tfoot +th +thead
    +time +title +tr +track +u +ul +var +video +wbr +<>))

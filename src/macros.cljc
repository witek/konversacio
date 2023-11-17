(ns macros)

(defmacro DEBUG [message & [data]]
  (if data
    `(js/console.log "[konversacio]" ~message ~data)
    `(js/console.log "[konversacio]" ~message)))

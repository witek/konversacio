(ns macros)

(defmacro DEBUG [message & [data]]
  (if data
    `(js/console.log "[konversacio]" ~message ~data)
    `(js/console.log "[konversacio]" ~message)))

(defmacro ERROR [message & [data]]
  (if data
    `(js/console.error "[konversacio]" ~message ~data)
    `(js/console.error "[konversacio]" ~message)))

(defmacro try> [form on-error on-success]
  `(try
     (let [result# (js/await ~form)]
       (~on-success result#))
     (catch :default error#
       (ERROR "do-with-catch--failed" {:form ~(str form)
                                       :error error#
                                       ;; :DEBUG ~(str &env)
                                       })
       (~on-error error#)
       nil)))

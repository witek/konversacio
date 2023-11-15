(ns user
  (:require
   [konversacio :as konversacio]))

(js/console.log "user.cljs")

(def $ konversacio/$)

(defn example [id presentation]
  (-> (js/document.getElementById "examples")
      (.appendChild ($ :div {}
                       ($ :h2 {} id)
                       ($ :div {:id id}))))
  (konversacio/present id presentation))

;;; examples

(example "simple"
         {:rows [{:cells [{:text "Just Text"}]}
                 {:cells [{:text "Action Button"
                           :action #(js/alert "Action Button clicked!")}]}
                 {:cells [{:text "More Text"}]}]})

(example "dense"
         {:rows [{:cells [{:text "Hello"}
                          {:text "World"}]}
                 {:cells [{:text "abc"}
                          {:text "Button" :action #(js/alert "clicked!")}
                          {:text "abc"}]}
                 {:cells [{:text "abc"}
                          {:text "Button 1" :action #(js/alert "clicked!")}
                          {:text "Button 2" :action #(js/alert "clicked!")}
                          {:text "abc"}]}
                 {:cells [{:text "This is the End"}]}]})

(example "from promise"
         (js/Promise. (fn [resolve reject]
                        (js/setTimeout
                         #(resolve
                           {:rows [{:cells [{:text "Promise resolved!"}]}]})
                         3000))))

(example "contact entity"
         {:rows [{:cells [{:text "Witoslaw Koczewski"}]}
                 {:indent 1
                  :cells [{:text "First Name"}
                          {:text "Witoslaw"}]}
                 {:indent 1
                  :cells [{:text "Last Name"}
                          {:text "Koczewski"}]}]})

(example "outlined notes"
         {:rows [{:cells [{:text "Some sample notes"}
                          {:text "#notes"}]}]})

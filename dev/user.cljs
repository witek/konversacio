(ns user
  (:require
   [konversacio :as konversacio]))

(js/console.log "user.cljs")

(def $ konversacio/$)

(defn example [id presentation]
  (-> (js/document.getElementById "examples")
      (.appendChild ($ :div {}
                       ($ :div {:style {:text-align "center"
                                        :font-weight "bold"
                                        :padding "16px 0 8px 0"
                                        }}
                          id)
                       ($ :div {:id id
                                :style {:height "300px"}}))))
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
                 {:cells [{:text "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum."
                           :embedded true}]}
                 {:cells [{:text "This is the End"}]}]})

(example "from promise"
         (js/Promise. (fn [resolve reject]
                        (js/setTimeout
                         #(resolve
                           {:rows [{:cells [{:text "Promise resolved!"}]}
                                   {:cells [{:text "Again!"
                                             :action (fn []
                                                       (js/Promise.
                                                        (fn [resolve]
                                                          (js/setTimeout (fn [] (resolve nil))
                                                                         1000))))}]}]})
                         3000))))

(example "input text"
         {:rows [{:cells [{:text "What is your E-Mail?"
                           :embedded true}]}
                 {:cells [{:input {:type :email
                                   :name "email"}}]}]
          :action (fn [data]
                    (js/alert (js/JSON.stringify data)))})

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

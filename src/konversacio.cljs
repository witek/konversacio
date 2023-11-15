(ns konversacio
  (:require
   [clojure.string :as str]))

;;; dom

(defn coerce-style-map [m]
  (when m
    (->> m
         (map (fn [[k v]]
                (str k ": "
                     (str v)
                     ";")))
         (str/join " "))))

(defn element-set-attr [e k v]
  (cond

    (= k :on-click)
    (-> e (.addEventListener "click" v))

    :else
    (-> e (.setAttribute k
                         (cond
                           (map? v) (coerce-style-map v)
                           (string? v) v
                           (seqable? v) (->> v (str/join " "))
                           :else v)))))

(defn element-append-children [^js e child-or-children]
  (when child-or-children
    (cond

      (or (string? child-or-children)
          (number? child-or-children))
      (-> e (.appendChild (js/document.createTextNode child-or-children)))

      (seqable? child-or-children)
      (doseq [child child-or-children]
        (element-append-children e child))

      :else
      (-> e (.appendChild child-or-children)))))

(defn element
  ([type]
   (js/document.createElement type))
  ([type attrs & children]
   (let [e (js/document.createElement type)]
     (doseq [[k v] attrs]
       (element-set-attr e k v))
     (element-append-children e children)
     e)))

(def $ element)

;;; presentation

(def px--cell-spacing 2)

(defn >cell [cell exec]
  (let [action (-> cell :action)]
    ($ :div {:class "cell"
             :style {:padding "2px 4px"
                     :background-color (if action
                                         "rgba(0,0,0,0.8)"
                                         "rgba(0,0,0,0.2)")
                     ;; :border "1px solid #ddd"
                     :cursor (when action :pointer)}
             :on-click (when action #(exec action))}
       (-> cell :text))))

(defn >row [row exec]
  ($ :div {:class "row"
           :style {:padding-left (when-let [indent (-> row :indent)]
                                   (str (-> indent (* 8)) "px"))}}

     (when-let [head (-> row :head)]
       ($ :div {:class "head"}
          head))

     (when-let [cells (-> row :cells seq)]
       ($ :div {:style {:display :flex
                        :gap (str px--cell-spacing "px")}}
          (for [cell cells]
            (>cell cell exec))))))

(defn >presentation [presentation exec]
  ($ :div {:class "presentation"
           :style {:padding "8px"
                   :font-family :monospace}}

     ($ :div {:style {:display :flex
                      :flex-direction :column
                      :gap (str px--cell-spacing "px")}}
        (for [row (-> presentation :rows)]
          (>row row exec)))))

(defn exec [f e-lock]
  (-> e-lock .-style .-display (set! "block"))
  (let [result (f)]
    (if (instance? js/Promise result)
      (-> result
          (.then (fn [result]
                   (-> e-lock .-style .-display (set! "none")))))
      (-> e-lock .-style .-display (set! "none")))))

(defn present* [e-wrapper e-lock presentation]
  (-> e-wrapper .-innerHTML (set! nil))

  (if (instance? js/Promise presentation)

    (-> presentation
        (.then (fn [presentation]
                 (present* e-wrapper e-lock
                           presentation))
               (fn [error]
                 (present* e-wrapper e-lock
                           {:rows [{:cells [{:text "error"}]}
                                   {:cells [{:text (str error)}]}]}))))

    (let [e-presentation (>presentation presentation #(exec % e-lock))]
      (-> e-wrapper (.appendChild e-presentation))
      (-> e-lock .-style .-display (set! "none")))))

(defn ^:export present [element-id presentation]
  (let [e-root (js/document.getElementById element-id)
        _ (when-not e-root
            (js/throw (js/Error. (str "Element '" element-id "' does not exist."))))
        _ (-> e-root .-innerHTML (set! nil))

        e-wrapper ($ :div {:class "wrapper"})
        _ (-> e-root (.appendChild e-wrapper))

        e-lock ($ :div {:class "lock"
                        :style {:background-color "rgba(0,0,0,0.5)"
                                :position "absolute"
                                :top 0
                                :width "100%"
                                :height "100%"}}
                  ($ :div {:style {:height "100%"
                                   :display :flex
                                   :place-items :center
                                   :place-content :center
                                   :text-align :center}}
                     "loading..."))

        e-container ($ :div {:style {:height "100%"
                                     :min-height "300px"
                                     :background-color "#333"
                                     :color "#ddd"
                                     :position "relative"
                                     :display "flex"
                                     :place-items "center"}}
                       ($ :div {:style {:max-width "720px"
                                        :margin "0 auto"
                                        }}
                          e-wrapper
                          )
                       e-lock
                       )

        _ (-> e-root (.appendChild e-container))]
    (present* e-wrapper e-lock
              presentation)))

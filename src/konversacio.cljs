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

(defn >cell [cell]
  (let [action (-> cell :action)]
    ($ :div {:class "cell"
             :style {:padding 4
                     :background-color (if action
                                         "rgba(0,0,0,0.8)"
                                         "rgba(0,0,0,0.2)")
                     ;; :border "1px solid #ddd"
                     :cursor (when action :pointer)}
             :on-click action}
       (-> cell :text))))

(defn >row [row]
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
            (>cell cell))))))

(defn >presentation [presentation]
  ($ :div {:class "presentation"
           :style {:background-color "#333"
                   :color "#ddd"
                   :padding "8px"
                   :font-family :monospace}}

     ($ :div {:style {:display :flex
                      :flex-direction :column
                      :gap (str px--cell-spacing "px")}}
        (for [row (-> presentation :rows)]
          (>row row)))))

(defn ^:export present [element-id presentation]
  (let [e-parent (js/document.getElementById element-id)
        #_(when-not e-parent
            (throw (js/Error. (str "Element '" element-id "' does not exist."))))
        e-presentation (>presentation presentation)]
    (-> e-parent (.appendChild e-presentation))))

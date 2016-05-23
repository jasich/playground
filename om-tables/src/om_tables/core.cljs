(ns om-tables.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(def init-data
  {:users [{:name "Jason" :email "2jason@validurl.com" :role "admin"}
           {:name "Erin" :email "3erin@validurl.com" :role "super-user"}
           {:name "Noah" :email "1noah@validurl.com" :role "user"}
           {:name "Aiden" :email "4aiden@validurl.com" :role "user"}]
   :sort {:key :name :is-ascending? true}
   :headers [{:name "Name" :key :name}
             {:name "Email" :key :email}
             {:name "Role" :key :role}]})

;; --------------------------------------------------------------------------
;; Parsing

(defn read [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defn mutate [{:keys [state] :as env} key params]
  (if (= 'users/sort key)
    {:value [:key]
     :action #(swap! state update-in [:sort] assoc :key (:key params) :is-ascending? (:is-ascending? params))}
    {:value :not-found}))


;; ----------------------------------------------------------------------
;; Components

(defn table-header [table {:keys [headers sort] :as props}] 
  (apply dom/tr nil
         (map #(dom/th
                #js {:onClick
                     (fn [_]
                       (if (= (:key sort) (:key %))
                         (om/transact! table
                                       `[(users/sort {:key ~(:key %) :is-ascending? ~(not (:is-ascending? sort))})])
                         (om/transact! table
                                       `[(users/sort {:key ~(:key %) :is-ascending? true})])))}
                (:name  %)) headers)))


(defui Row
  Object
  (render [this]
          (let [{:keys [name email role]} (om/props this)]
            (dom/tr nil
                    (dom/td nil name)
                    (dom/td nil email)
                    (dom/td nil role)))))

(def row (om/factory Row))

(defui Table
  static om/IQuery
  (query [this]
         [:users :sort :headers])
  Object
  (render [this]
          (let [{:keys [users sort headers]} (om/props this)]
            (dom/table nil
                       (dom/thead nil
                                  (table-header this (om/props this)))
                       (apply dom/tbody nil
                              (let [sorted-users (sort-by (:key sort) (if (:is-ascending? sort) < >) users)]
                                (map row sorted-users)))
                       ))))

(def reconciler
  (om/reconciler
   {:state init-data
    :parser (om/parser {:read read :mutate mutate})}))


(om/add-root! reconciler
              Table (gdom/getElement "app"))

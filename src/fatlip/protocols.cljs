(ns fatlip.protocols)


(defprotocol Sparse
  (ps [this])
  (qs [this])
  (rs [this]))

(defprotocol Layered
  (layers [this]))

(defprotocol Directed
  (preds [this])
  (succs [this]))

(defprotocol Nodey
  (nodes [this] "Returns constituent nodes as close to ordered as possible")
  (characters [this]))

(defprotocol CrossCounted
  (crossings [this]))

(defprotocol EdgeMarked
  (marked [this]))

(defprotocol Plotted)

(defprotocol Reversible
  (rev [this] "It... reverses"))

(defrecord Node [id layer-id characters weight])

(defrecord Edge [src dest characters weight]
  Reversible
  (rev [this]
    (assoc this
           :src dest
           :dest src)))

(defrecord Segment [endpoints layer-id characters weight])


(defn Edge->Segment
  "Does what it says on the tin; Segments represent the portion of an Edge that
  crosses a particular layer"
  [edge layer-id]
  (map->Segment {:endpoints #{(:src edge) (:dest edge)}
                 :layer-id layer-id
                 :characters (:characters edge)
                 :weight (:weight edge)}))

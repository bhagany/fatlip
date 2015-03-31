(ns fatlip.plot
  (:require [cljs.core.match :refer-macros [match]]
            [clojure.core.rrb-vector :as rrb]
            [clojure.set :as set]
            [fatlip.order :as fo]
            [fatlip.protocols :refer [Reversible Node Edge->Segment rev
                                      nodes]]))


(defprotocol YPlottable
  (ys [graph node-sep char-sep] [graph node-sep char-sep delta]
    "Returns y-coordinates to nodes, given minimum separations between nodes
     and characters")
  (min-y [graph node-sep char-sep] "Minimum y-coordinate in the graph")
  (max-y [graph node-sep char-sep] "Maximum y-coordinate in the graph")
  (mid-y [graph node-sep char-sep] "Midpoint of y-coordinates in the graph")
  (width [graph node-sep char-sep] "Width of the graph in y"))

(defrecord BlockEdge [src dest weight]
  Reversible
  (rev [this]
    (assoc this
           :src dest
           :dest src)))


(defn get-block-rel-ys
  "Calculates the relative y-positions of blocks within a class. If the class
  was compacted upward, we start at the top and walk down the class; for
  downward, at the bottom, walking up."
  [class block-edges node-sep char-sep compacted]
  (let [filter-set (set class)
        rel-op ({:up + :down -} compacted)]
    (reduce (fn [ys block]
              (let [in-class-edges (filter #(contains? filter-set (:dest %))
                                           (get block-edges block))
                    neighbor-ys (map #(rel-op (get ys (:dest %))
                                              (* char-sep (dec (:weight %))))
                                     in-class-edges)
                    block-y (if (empty? neighbor-ys)
                              0
                              (if (= compacted :up)
                                (+ (apply max neighbor-ys) node-sep)
                                (- (apply min neighbor-ys) node-sep)))]
                (assoc ys block block-y)))
            {}
            class)))


(defn get-shift-ys
  "Calculates the relative y-positions of classes. If compacted upward, we walk
  the classes downward; for downward compaction, upward."
  [classes class-edges node-sep char-sep rel-ys compacted]
  (reduce (fn [ys class]
            (let [rel-op ({:up + :down -} compacted)
                  neighbor-ys (map #(rel-op (- (+ (get ys (:dest %))
                                                  (get rel-ys (:dest %)))
                                               (get rel-ys (:src %)))
                                            (* char-sep (dec (:weight %))))
                               (get class-edges class))
                  class-y (if (empty? neighbor-ys)
                            0
                            (if (= compacted :up)
                              (+ (apply max neighbor-ys) node-sep)
                              (- (apply min neighbor-ys) node-sep)))]
              (assoc ys class class-y)))
          {}
          classes))


(def memo-ys ;; heh
  ^{:doc "Given a class graph and node and char separations, calculates y
         positions for each node in the underlying graph."}
  (memoize
   (fn [class-graph node-sep char-sep]
     (let [{:keys [classes preds succs
                   block-preds block-succs compacted]} class-graph
           [block-edges class-edges] ({:up [block-preds preds]
                                       :down [block-succs succs]} compacted)
           rel-ys (reduce #(merge %1 (get-block-rel-ys %2 block-edges
                                                       node-sep char-sep
                                                       compacted))
                          {}
                          classes)
           shift-ys (get-shift-ys classes class-edges node-sep char-sep
                                  rel-ys compacted)
           block-shift-ys (into {} (mapcat (fn [[class shift]]
                                             (map #(-> [% shift]) class))
                                           shift-ys))]
       (->> rel-ys
            (map (fn [[block rel-y]]
                   [block (+ rel-y (get block-shift-ys block))]))
            (mapcat (fn [[block y]]
                      (->> (filter #(instance? Node %) block)
                           (map #(-> [% y])))))
            (into {}))))))


(defrecord ClassGraph [classes succs preds block-succs block-preds sources sinks
                       aligned compacted]
  YPlottable
  (ys [this node-sep char-sep]
    (memo-ys this node-sep char-sep))

  (ys [this node-sep char-sep delta]
    (->> (ys this node-sep char-sep)
         (map (fn [[node y]]
                [node (+ y delta)]))
         (into {})))

  (min-y [this node-sep char-sep]
    (apply min (map #(get (ys this node-sep char-sep) (ffirst %)) classes)))

  (max-y [this node-sep char-sep]
    (->> classes
         (mapcat peek)
         (map #(+ (get (ys this node-sep char-sep) %)
                  (* char-sep (dec (:weight %)))))
         (apply max)))

  (width [this node-sep char-sep]
    (- (max-y this node-sep char-sep) (min-y this node-sep char-sep))))


(defn square [x]
  "Squares a number"
  (* x x))


(defn check-alignment
  "Checks whether a predecessor is a valid alignment candidate"
  [pred last-idx marked]
  (when (and (< last-idx (:idx pred))
             (not (contains? marked (:edge pred))))
    pred))


(defn pred->segs+src
  "Takes a predecessor Edge and returns a vector of Segments, one for each
  layer this Edge crosses, plus the source Node"
  [pred]
  (let [src (:src pred)
        dest (:dest pred)
        src-layer (:layer-id src)
        dest-layer (:layer-id dest)
        segs (if (> src-layer dest-layer)
               (mapv (partial Edge->Segment pred)
                     (range (inc dest-layer) src-layer))
               (mapv (partial Edge->Segment pred)
                     (range (dec dest-layer) src-layer -1)))]
    (conj segs src)))


(defn blockify-layer
  "Assign every node in a layer to an already-existing block, or begin a new
  block with it"
  [layer pred-layer roots blocks all-preds top-idxs marked]
  (->> (:items layer)
       (filter #(instance? Node %))
       (reduce
        (fn [[roots blocks last-idx] node]
          (let [pred-layer-id (:id pred-layer)
                preds (->> (get all-preds node)
                           (map #(if (= pred-layer-id (-> % :dest :layer-id))
                                   {:edge % :item (:dest %)}
                                   {:edge %
                                    :item (Edge->Segment % pred-layer-id)}))
                           (map #(assoc % :idx (get top-idxs (:item %))))
                           (sort-by :idx)
                           (mapcat #(repeat (-> % :edge :weight) %)))
                num-preds (count preds)
                median (quot (dec num-preds) 2)
                aligned (and (pos? num-preds)
                             (or (check-alignment (nth preds median)
                                                  last-idx marked)
                                 ;; If we kind of have two medians because
                                 ;; there are an even number of predecessors,
                                 ;; then we allow the "second" median to be
                                 ;; used if the first isn't available
                                 (and (even? num-preds)
                                      (check-alignment
                                       (nth preds (inc median))
                                       last-idx marked))))
                [root items idx] (if aligned
                                   ;; If we have an alignment, then we have the
                                   ;; same root, and add this node to that
                                   ;; root's block
                                   [(get roots (-> aligned :edge :dest))
                                    (pred->segs+src (:edge aligned))
                                    (:idx aligned)]
                                   ;; No aligned nodes means it's a new block
                                   ;; root
                                   [node [node] last-idx])
                rs (reduce #(assoc-in %1 [%2] root) roots items)
                bs (update-in blocks [root] (fnil rrb/catvec []) items)]
            [rs bs idx]))
        [roots blocks -1])
       (take 2)))


(defn blockify
  "Associate nodes with their median-ly positioned parent if it exists, hasn't
  been aligned with by another node, and we haven't aligned with nodes of
  greater index in the past. Continue finding these alignments until we've
  assigned every node into a horizontally aligned block"
  [flat-graph]
  (take 2 (reduce (fn [[roots blocks pred-layer] layer]
                    (let [[rs bs] (blockify-layer layer pred-layer roots blocks
                                                  (:preds flat-graph)
                                                  (:top-idxs flat-graph)
                                                  (:marked flat-graph))]
                      [rs bs layer]))
                  [{} {} nil]
                  (:layers flat-graph))))


(defn topo-sort
  "An implementation of a Kahn topological sort, cribbed with modification from
  https://gist.github.com/alandipert/1263783"
  [sources succs]
  (loop [sources sources
         succs succs
         sorted []]
    (if (empty? sources)
      sorted
      (let [node (first sources)
            [dests succs'] ((juxt get dissoc) succs node)
            all-dests (reduce set/union (vals succs'))
            sources' (apply conj (rest sources)
                            (set/difference dests all-dests))]
        (recur sources' succs' (conj sorted node))))))


(defn block-edges
  "Uses neighbor information (:aboves) from a FlatGraph to determine where the
  edges are between blocks, given a particular blockification of that graph"
  [flat-graph roots blocks]
  (let [edge->set #(update-in %1 [(:src %2)] (fnil conj #{}) %2)]
    (reduce (fn [{:keys [succs preds] :as edges} block]
              (let [b-succs (->> (map #(get-in flat-graph [:aboves %])
                                      block)
                                 (remove nil?)
                                 (group-by #(get roots %))
                                 (map (fn [[neighbor-root neighbor-nodes]]
                                        (BlockEdge.
                                         (get blocks neighbor-root)
                                         block
                                         (apply max (map :weight
                                                         neighbor-nodes))))))
                    b-preds (map rev b-succs)]
                (-> edges
                    (update-in [:succs] #(reduce edge->set % b-succs))
                    (update-in [:preds] #(reduce edge->set % b-preds)))))
            {:succs {} :preds {}}
            (vals blocks))))


(defn FlatGraph->block-info
  "Organizes all of the Nodes and Segments in a FlatGraph into horizontally-
  aligned blocks. These blocks are then organized into a graph of their own,
  where the nodes are blocks and the edges are determined by adjacency between
  their constituent Nodes and Segments in each FlatLayer."
  [flat-graph]
  (let [[roots blocks] (blockify flat-graph)
        {:keys [succs preds]} (block-edges flat-graph roots blocks)
        [simple-succs simple-preds] (map
                                     #(into {}
                                            (map
                                             (fn [[src edges]]
                                               [src (set (map :dest edges))])
                                             %))
                                     [succs preds])
        block-set (set (vals blocks))
        layer-id-compare (if (:reversed (meta flat-graph))
                           > <)
        [sources sinks] (map (fn [edges]
                               (sort-by #(:layer-id (get % 0))
                                        layer-id-compare
                                        (set/difference block-set
                                                        (keys edges))))
                             [preds succs])
        blocks-up (topo-sort sources simple-succs)
        blocks-down (topo-sort sinks simple-preds)]
    {:blocks-up blocks-up :blocks-down blocks-down :succs succs :preds preds
     :simple-succs simple-succs :simple-preds simple-preds
     :sources sources :sinks sinks}))


(defn classify-start
  "Given a block that is a start (source or sink) in a ClassGraph, returns a set
  containing that start and all of its descendants"
  [start edges]
  (loop [seen #{}
         nodes [start]]
    (if (empty? nodes)
      seen
      (let [[n & ns] nodes
            unseen-ns (set/difference (get edges n) seen)]
        (recur (conj seen n) (apply conj ns unseen-ns))))))


(defn classify
  "Organizes blocks into classes, defined as all blocks that are reachable from
  a block that is a root in its graph, with preference given to the left-most
  roots"
  [blocks edges start-blocks]
  (reduce (fn [classes start-block]
            (let [proto-class (classify-start start-block edges)
                  class-set (set/difference proto-class (apply set (vals classes)))
                  class (filterv #(contains? class-set %) blocks)]
              (reduce #(assoc %1 %2 class) classes class)))
          {}
          start-blocks))


(defn block-info->ClassGraph
  "For a particular blockification, compacts these blocks in to a ClassGraph,
  in either an :up or :down direction"
  [block-info compacted]
  (let [block-keys (if (= compacted :up)
                     [:blocks-up :simple-succs :sources]
                     [:blocks-down :simple-preds :sinks])
        [blocks classify-edges start-blocks] (map block-info block-keys)
        block-classes (classify blocks classify-edges start-blocks)
        classes-set (set (vals block-classes))
        edge->set #(update-in %1 [(get block-classes (:src %2))]
                              (fnil conj #{}) %2)
        block-succs (:succs block-info)
        [succs preds]
        (reduce (fn [[s p] class]
                  (let [class-set (set class)
                        class-succs (->> class
                                         (mapcat #(get block-succs %))
                                         (remove #(contains? class-set (:dest %)))
                                         set)
                        class-preds (map rev class-succs)]
                    [(reduce edge->set s class-succs)
                     (reduce edge->set p class-preds)]))
                [{} {}]
                classes-set)
        sources (set/difference classes-set (set (keys preds)))
        sinks (set/difference classes-set (set (keys succs)))
        classes (if (= compacted :up)
                  (let [simple-succs (->> succs
                                          (map (fn [[src edges]]
                                                 [src (set (map #(get-in
                                                                  block-classes
                                                                  [(:dest %)])
                                                                edges))]))
                                          (into {}))]
                    (topo-sort sources simple-succs))
                  (let [simple-preds (->> preds
                                          (map (fn [[src edges]]
                                                 [src (set (map #(get-in
                                                                  block-classes
                                                                  [(:dest %)])
                                                                edges))]))
                                          (into {}))]
                    (topo-sort sinks simple-preds)))]
    (map->ClassGraph {:classes classes
                      :succs succs :preds preds
                      :sources sources :sinks sinks
                      :block-succs block-succs
                      :block-preds (:preds block-info)
                      :compacted compacted})))


(defn FlatGraph->UpDownClassGraphs
  "Organizes nodes and edges into blocks into classes and then constructs a
  ClassGraph, where the nodes are classes and the edges are BlockEdges that
  span classes. This means there can be multiple edges per class pair."
  [flat-graph]
  (let [block-info (FlatGraph->block-info flat-graph)]
    [(block-info->ClassGraph block-info :up)
     (block-info->ClassGraph block-info :down)]))


(defn FlatGraph->ClassGraphs
  "Takes a FlatGraph and creates four variations of it by aggregating nodes and
  segments into blocks in forward and reverse directions, and then organizes
  these blocks both upward and downward"
  [flat-graph]
  (mapcat (fn [fg]
            (let [class-graphs (FlatGraph->UpDownClassGraphs fg)]
              (if (:reversed (meta fg))
                (map #(vary-meta % {:aligned-right true}) class-graphs)
                (map #(vary-meta % {:aligned-left true}) class-graphs))))
          [flat-graph ^:reversed (rev flat-graph)]))


(defn plot-ys
  "Given a FlatGraph, generate four ClassGraphs, which are used to assign
  y-values for each node in each graph variation. The four y-positions for each
  node are then averaged together to give a final value.

  This approach is very heavily based on Fast and Simple Horizontal Coordinate
  Assignment by Ulrik Brandes and Boris Köpf (BK), even though the algorithm
  is pretty much entirely dissimilar. I've taken BK's basic idea (which is to
  align nodes into 'blocks' group these blocks into 'classes' and then
  recursively position each class and block on the x axis), discarded the
  algorithm they provided, and made my own. Their algorithm operated on blocks
  and classes as sort of emergent properties from relationships between nodes,
  whereas mine explicity constructs blocks and classes and organizes each into
  graphs, which are then used for positioning. I just found my approach easier
  to reason about.

  It is also important to note that BK (and pretty much all of the academic
  literature, for that matter), is concerned with drawing graphs vertically,
  while we are drawing them horizontally. The practical result of this is that
  our graphs are reflected across the upper-left-to-lower-right diagonal as
  compared to the graphs in BK, and as a result we sometimes do things in the
  opposite manner. The most obvious example is that while BK-style classes are
  defined by their sinks in a block graph, ours are defined by sources in the
  block graph. If you reflect the graph though, you'll see that we're working
  from the same block and getting the same result."
  [flat-graph node-sep char-sep]
  (let [class-graphs (FlatGraph->ClassGraphs flat-graph)
        narrowest (apply min-key #(width % node-sep char-sep) class-graphs)
        narrow-min-y (min-y narrowest node-sep char-sep)
        narrow-max-y (max-y narrowest node-sep char-sep)]
    (->> class-graphs
         (map #(let [delta (if (:aligned-down (meta %))
                             (- narrow-max-y (max-y % node-sep char-sep))
                             (- narrow-min-y (min-y % node-sep char-sep)))]
                 (ys % node-sep char-sep delta)))
         (apply merge-with #(if (vector? %1) (conj %1 %2) [%1 %2]))
         (map (fn [[node ys]]
                (let [sort-ys (sort ys)]
                  [node (/ (+ (nth sort-ys 1) (nth sort-ys 2)) 2)])))
         (into {}))))


(defn arc-distance
  "Calculates the distance traveled by an arc given the radius of that arc and
  the the radius slope, or the slope of the tangent to that radius"
  [radius slope]
  {:pre [(every? number? [radius slope])]}
  (/ radius (.sqrt js/Math (+ 1 (square slope)))))


(defn arc-x-distance
  "Calculates the x-distance covered by an arc given that arc's radius and
  slope"
  [radius radius-slope]
  {:pre [(every? number? [radius radius-slope])]}
  (arc-distance radius radius-slope))


(defn arc-y-distance
  "Calculates the y-distance covered by an arc given that arc's radius and
  slope"
  [radius radius-slope]
  {:pre [(every? number? [radius radius-slope])]}
  (arc-distance radius (/ 1 radius-slope)))


(defn layer-x-distance
  "Calculates the minimum distance between two layers based on the maximum
  allowed slope of a line between them"
  ;; Came up with this formula in the following manner: we have the
  ;; y-coordinates of two circles and the slope of the line that is internally
  ;; tangent to both.  We want to find the horizontal distance between the
  ;; circle centers for this system. This distance can be divided up into three
  ;; parts: the x-distance between the center of the first circle and its
  ;; tangent point, the x-distance between the tangent points on each circle,
  ;; and the x-distance between the second tangent point and the center of the
  ;; second circle.

  ;; The central insight here is that because we know the slope of the tangent
  ;; (max-slope, or m), we also know that the slope of the radius between the
  ;; center of each circle and their respective tangent points is -1/m due to
  ;; the radius and the tangent being perpendicular. I've just used 1/m because
  ;; the term ends up getting squared anyway. Put another way, since we're only
  ;; interested in x values, it doesn't matter whether the slope of the tangent
  ;; is positive or negative as long as it rises or falls at the same
  ;; rate. Given this, we can solve this system of equations to get formulas
  ;; for the tangent point in terms of the slope and radius:

  ;; y - y_1 = (1/m) (x - x_1)           ; the point-slope forumla
  ;; (x - x_1)^2 + (y - y_1)^2 = r^2     ; formula for a circle
  ;; where (x_1, y_1) is the center of a circle and r is the length of the
  ;; radius.

  ;; You solve the point-slope formula in terms of x and y (separately) and
  ;; substitute in the circle formula to get the x-value of the tangent point.
  ;; Solving point-slope for y and substituting gets you the y-value.

  ;; m(y - y_1) = x - x_1
  ;; x = m(y - y_1) + x_1

  ;; substituting into the circle formula:

  ;; (m(y - y_1) + x_1 - x_1)^2 + (y - y_1)^2 = r^2
  ;; m^2(y - y_1)^2 + (y - y_1)^2 = r^2
  ;; (y - y_1)^2 (m^2 + 1) = r^2
  ;; (y - y_1)^2 = r^2 / (m^2 + 1)
  ;; y - y_1 = ±r / √(m^2 + 1)
  ;; y = y_1 ± r / √(m^2 + 1)

  ;; Solving point-slope for x is almost exactly the same, and substituting
  ;; gives you

  ;; x = x_1 ± r / √((1 / m^2) + 1)

  ;; Since we're really only interested in the relative horizontal distances,
  ;; and we know that the contribution from the arcs is positive for both
  ;; circles, we can simplify to:

  ;; x = r / √((1 / m^2) + 1) y = y_1 ± r / √(m^2 + 1)

  ;; Then since we want the values for both circles, we do this:

  ;; arcs dx = (r_1 / √((1 / m^2) + 1)) + (r_2 / √((1 / m^2) + 1))
  ;; arcs dx = (r_1 + r_2) / √((1 / m^2) + 1)

  ;; Now that we have the contribution from the arcs, we only need to figure
  ;; out the horizontal distance that the tangent itself covers. This is pretty
  ;; easy; for a straight line, we know that dx = dy / m, where dx is the
  ;; horizontal distance and dy is the vertical distance.  We also know that
  ;; the vertical distance of the tangent line is equal to the distance between
  ;; the circle centers, plus the vertical distance from each center to the
  ;; respective tangent point. Following the same logic as above, we can derive
  ;; that:

  ;; arcs dy = (r_1 + r_2) / √(m^2 + 1)

  ;; Then all we need to do is add the distance between centers and divide the
  ;; whole thing by m:

  ;; tangent dx = (|y_1 - y_2| + (r_1 + r_2) / √(m^2 + 1)) / m.

  ;; Putting it all together:

  ;; total dx = ((r_1 + r_2) / √((1 / m^2) + 1))
  ;;            + ((|y_1 - y_2| + (r_1 + r_2) / √(m^2 + 1)) / m)
  [max-slope sum-radii centers-y-dist]
  {:pre [(every? number? [max-slope sum-radii centers-y-dist])]}
  (let [radius-slope (/ 1 max-slope)
        arcs-x-dist (arc-x-distance sum-radii radius-slope)
        arcs-y-dist (arc-y-distance sum-radii radius-slope)
        tangent-y-dist (+ arcs-y-dist centers-y-dist)
        tangent-x-dist (/ tangent-y-dist max-slope)]
    (+ arcs-x-dist tangent-x-dist)))


(defn arc-center-up
  "Given node info map (must have :node-y) and a min-arc-radius, returns the
  center for the upward arcs on that node"
  [{:keys [node-y]} min-arc-radius]
  {:pre [(vector? node-y) (number? min-arc-radius)]}
  (- (node-y 0) min-arc-radius))


(defn arc-center-down
  "Given node info map (must have :node-y) and a min-arc-radius, returns the
  center for the downward arcs on that node"
  [{:keys [node-y]} min-arc-radius]
  {:pre [(vector? node-y) (number? min-arc-radius)]}
  (+ (node-y 1) min-arc-radius))


(defn arc-radius-up
  "Given node info map (must have :order) a min-arc-radius and the number of
  pixels characters should be separated by, and returns the radius of the arc
  for that node for that character"
  [{:keys [order]} min-arc-radius char-sep]
  {:pre [(integer? order) (every? number? [min-arc-radius char-sep])]}
  (+ min-arc-radius
     (* order char-sep)))


(defn arc-radius-down
  "Given node info map (must have :order and :node, which in turn must
  have :weight) a min-arc-radius and the number of pixels characters should be
  separated by, and returns the radius of the arc for that node for that
  character"
  [{:keys [order] {weight :weight} :node} min-arc-radius char-sep]
  {:pre [(every? integer? [order weight])
         (every? number? [min-arc-radius char-sep])]}
  (+ min-arc-radius
     (* (- (dec weight) order)
        char-sep)))


(defn arc-y-info
  "Returns the y values of the arc centers, as well as the radii for each arc
  for a pair of character nodes on adjacent layers"
  [src dest dir min-arc-radius char-sep]
  {:pre [(contains? #{:up :down} dir)]}
  (if (= dir :up)
    {:src-arc-y (arc-center-up src min-arc-radius)
     :src-arc-radius (arc-radius-up src min-arc-radius char-sep)
     :dest-arc-y (arc-center-down dest min-arc-radius)
     :dest-arc-radius (arc-radius-down dest min-arc-radius char-sep)}
    {:src-arc-y (arc-center-down src min-arc-radius)
     :src-arc-radius (arc-radius-down src min-arc-radius char-sep)
     :dest-arc-y (arc-center-up dest min-arc-radius)
     :dest-arc-radius (arc-radius-up dest min-arc-radius char-sep)}))


(defn add-y-info
  "Adds :arc-y and :arc-radius information to character path pairs, plus
  :total-arc-radius, for convenience"
  [pair-maps min-arc-radius char-sep]
  (map (fn [{:keys [dir], [src dest] :pair, :as info}]
         (if (= dir :level)
           info
           (let [{:keys [src-arc-y dest-arc-y
                         src-arc-radius dest-arc-radius]}
                 (arc-y-info src dest dir min-arc-radius char-sep)]
             (-> info
                 (assoc :total-arc-radius (+ src-arc-radius dest-arc-radius))
                 (assoc-in [:pair 0 :arc-y] src-arc-y)
                 (assoc-in [:pair 0 :arc-radius] src-arc-radius)
                 (assoc-in [:pair 1 :arc-y] dest-arc-y)
                 (assoc-in [:pair 1 :arc-radius] dest-arc-radius)))))
       pair-maps))


(defn add-x-info
  "Adds :xs to character path pairs, and if the pairs are not :level, also
  adds :arc-x for each pair"
  [pair-maps layer-xs]
  (map (fn [{:keys [dir], [src dest] :pair, :as info}]
         (let [src-xs (-> src :node :layer-id layer-xs)
               dest-xs (-> dest :node :layer-id layer-xs)
               info' (-> info
                         (assoc-in [:pair 0 :xs] src-xs)
                         (assoc-in [:pair 1 :xs] dest-xs))]
           (if (= dir :level)
             info'
             (-> info'
                 (assoc-in [:pair 0 :arc-x] (src-xs 1))
                 (assoc-in [:pair 1 :arc-x] (dest-xs 0))))))
       pair-maps))


(defn pair-up
  "Given a list of character positions, pairs each with the one following for a
  representation of transistions between layers. Also adds information about
  the direction this transition goes."
  [infos]
  (->> (map vector infos (drop 1 infos))
       (reduce (fn [pairs [src dest]]
                 (let [src-y (:y src)
                       dest-y (:y dest)]
                   (conj pairs
                         {:dir (if (= src-y dest-y)
                                 :level
                                 (if (> src-y dest-y)
                                   :up
                                   :down))
                          :pair [src dest]})))
               [])))


(defn relative-layer-xs
  "Generates a map of layers to the pixel distance to the preceding layer"
  [path-info layers max-slope layer-sep]
  (->> (map second path-info)
       (mapcat #(remove (fn [{:keys [dir]}] (= dir :level)) %))
       (reduce (fn [min-seps {:keys [dir total-arc-radius],
                              [src dest] :pair}]
                 (let [layer (get layers (-> dest
                                             :node
                                             :layer-id))
                       src-arc-y (:arc-y src)
                       dest-arc-y (:arc-y dest)
                       centers-y-dist (.abs js/Math (- src-arc-y dest-arc-y))
                       x-dist (layer-x-distance max-slope
                                                total-arc-radius
                                                centers-y-dist)]
                   (update-in min-seps [layer]
                              (fnil conj #{})
                              x-dist)))
               {})
       (map (fn [[layer x-ds]]
              [layer (apply max layer-sep x-ds)]))
       (into {})))


(defn absolute-layer-xs
  "Generates a map of layers to their absolute x-values, including duration"
  [path-info layers max-slope layer-sep]
  (let [x-seps (relative-layer-xs path-info layers max-slope layer-sep)]
    (into [] (reductions (fn [[_ last-x] layer]
                           (let [start-x (+ last-x (get x-seps layer))
                                 end-x (+ start-x (:duration layer))]
                             [start-x end-x]))
                         [0 (:duration (first layers))]
                         (rest layers)))))


(defn arcs-and-tangent
  "Given a pair of character positions that are not :level, generates plotting
  information for the arcs and the line tangent to them required to draw the
  transition"
  [{:keys [dir total-arc-radius],
    [{src-x :arc-x, src-y :arc-y, src-radius :arc-radius}
     {dest-x :arc-x, dest-y :arc-y, dest-radius :arc-radius
      dest-path-y :y}] :pair}]
  {:pre [(every? number? [total-arc-radius src-x src-y src-radius
                          dest-x dest-y dest-radius])
         (contains? #{:up :down} dir)]}
  (let [;; (intersect-x, intersect-y) is the point at which the line between
        ;; centers crosses the tangent
        intersect-x (/ (+ (* src-radius dest-x)
                          (* dest-radius src-x))
                       total-arc-radius)
        intersect-y (/ (+ (* src-radius dest-y)
                          (* dest-radius src-y))
                       total-arc-radius)
        src-radius2 (square src-radius)
        src-center-intersect-dx (- intersect-x src-x)
        src-center-intersect-dx2 (square src-center-intersect-dx)
        src-center-intersect-dy (- intersect-y src-y)
        src-center-intersect-dy2 (square src-center-intersect-dy)
        src-center-intersect-dx2-dy2 (+ src-center-intersect-dx2
                                        src-center-intersect-dy2)
        ;; In the triangle formed by arc center, tangent point, and intersect,
        ;; the src-altitude-foot is the foot that belongs to the tangent point
        src-altitude-foot-x (+ src-x
                               (/ (* src-radius2
                                     src-center-intersect-dx)
                                  src-center-intersect-dx2-dy2))
        src-altitude-foot-y (+ src-y
                               (/ (* src-radius2
                                     src-center-intersect-dy)
                                  src-center-intersect-dx2-dy2))
        src-radius-ratio (/ (.sqrt js/Math (- src-center-intersect-dx2-dy2
                                              src-radius2))
                            src-radius)
        ;; The case where (= src-y dest-y) can be treated as either
        ;; :up or :down
        center-line-dir (if (> src-y dest-y) :up :down)
        x-op (if (= dir center-line-dir) + -)
        [src-y-op dest-y-op] (if (= dir :up) [+ -] [- +])
        src-tangent-x (x-op src-altitude-foot-x
                            (* src-radius-ratio
                               (.abs js/Math (- src-altitude-foot-y src-y))))
        src-tangent-y (src-y-op src-altitude-foot-y
                                (* src-radius-ratio
                                   (.abs js/Math (- src-x src-altitude-foot-x))))
        radius-slope (/ (- src-tangent-y src-y)
                        (- src-tangent-x src-x))
        dest-tangent-x (- dest-x
                          (arc-x-distance dest-radius radius-slope))
        dest-tangent-y (dest-y-op dest-y
                                  (arc-y-distance dest-radius radius-slope))
        [src-sweep dest-sweep] (if (= dir :up) [0 1] [1 0])]
    [{:type :a, :radius src-radius, :sweep src-sweep
      :x src-tangent-x, :y src-tangent-y :arc-x src-x :arc-y src-y}
     {:type :l, :x dest-tangent-x, :y dest-tangent-y}
     {:type :a, :radius dest-radius, :sweep dest-sweep
      :x dest-x, :y dest-path-y :arc-x dest-x :arc-y dest-y}]))


(defn extend-h
  "Extends a terminal :h plot to a new x value"
  [plots x]
  {:pre [(= (:type (peek plots)) :h) (number? x)]}
  (assoc-in plots [(dec (count plots)) :x] x))


(defn h-to
  "Draw a horizontal line to x"
  [x]
  {:pre [(number? x)]}
  {:type :h, :x x})


(defn plot-duration
  "Plots a layer duration after a non-:level transition"
  [plots start-x end-x]
  {:pre [(number? start-x) (number? end-x)]}
  (if (= start-x end-x)
    plots
    (conj plots (h-to end-x))))


(defn move-to
  "Starts a path by moving to its beginning and drawing the initial layer's
  duration, if applicable"
  [{:keys [y], [start-x end-x] :xs}]
  {:pre [(every? number? [y start-x end-x])]}
  (let [plots [{:type :m
                :x start-x
                :y y}]]
    (plot-duration plots start-x end-x)))


(defn char-plots
  "Generates coordinates a single character's path"
  [plots {:keys [dir pair], [src dest] :pair, :as pair-map}]
  ;; TODO checking empty? on every step seems silly; once we have
  ;; multipart paths, potentially with mid-path M's, we should just
  ;; use (move-to src) as the seed value in the reduce
  (let [plots' (if (empty? plots)
                 (into plots (move-to src))
                 plots)
        last-type (:type (peek plots'))
        [dest-start-x dest-end-x] (:xs dest)]
    (match [last-type dir]
           [:h :level] (extend-h plots' dest-end-x)
           [_ :level] (conj plots' (h-to dest-end-x))
           :else (plot-duration (into plots' (arcs-and-tangent pair-map))
                                dest-start-x dest-end-x))))


(defn plot-xs
  "Given y-values and layer-spacing paramters, calculate an x-value for
  characters"
  [paths-y layers max-slope layer-sep]
  (let [layer-xs (absolute-layer-xs paths-y layers max-slope layer-sep)]
    (->> paths-y
         (map (fn [[character pair-maps]]
                [character (add-x-info pair-maps layer-xs)]))
         (map (fn [[character pair-maps]]
                {:character character
                 :plots (reduce char-plots [] pair-maps)})))))


(defn pathify
  "Given node-level y information, generate paths with character-level y info
  for each character"
  [node-ys nodes characters min-arc-radius char-sep]
  (->> nodes
       (mapcat (fn [node]
                 (map-indexed (fn [i c]
                                {:node node
                                 :character c
                                 :order i})
                              (get characters node))))
       (map (fn [info]
              (let [node (:node info)
                    node-top-y (get node-ys node)]
                (assoc info
                       :node-y [node-top-y
                                (+ node-top-y
                                   (* char-sep
                                      (dec (:weight node))))]
                       :y (+ node-top-y (* (:order info)
                                           char-sep))))))
       (group-by :character)
       (map (fn [[character infos]]
              [character (pair-up infos)]))
       (map (fn [[character pair-maps]]
              [character (add-y-info pair-maps min-arc-radius
                                     char-sep)]))))


(defn plot
  "Generates coordinates for all paths in a FlatGraph"
  [flat-graph max-slope min-arc-radius layer-sep node-sep char-sep]
  (let [{:keys [layers characters]} flat-graph]
    (-> flat-graph
        (plot-ys node-sep char-sep)
        (pathify (nodes flat-graph) characters min-arc-radius char-sep)
        (plot-xs layers max-slope layer-sep))))

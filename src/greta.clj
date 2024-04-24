(ns greta
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [ont-app.vocabulary.lstr :as lstr]
            [arachne.aristotle.registry :as reg]
            [arachne.aristotle :as ar]))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))

(defn parse-csv [file-path]
  (try
    (with-open [reader (io/reader file-path)]
      (doall (csv-data->maps (csv/read-csv reader :separator \;))))
    (catch Exception e
      (println "An error occurred: " e))))

(defn csv-data-from-file
  [csv-file]
  (parse-csv csv-file))

(def greta "https://w3id.org/kim/greta/")

(defn build-greta-id
  [num]
  (str "g_" (inc num)))

(defn parse-csv-data
  "prepares the csv file for easier parsing"
  [csv-file]
  (reduce (fn [acc val]
            (let [node {:id (build-greta-id (count acc))
                        :prefLabel (str (:Name val))
                        :definition (:Description val)
                        :BroaderConcept (:BroaderConcept val)
                        :broader (:id (first (filter
                                              (fn [x] (= (:prefLabel x) (:BroaderConcept val)))
                                              acc)))}]
              (conj acc node)))
          []
          (csv-data-from-file csv-file)))

(defn rdf-triples-from-node
  [kw node]
  (let [rdf-triples {:rdf/about (keyword kw (:id node)) ;; to follow aritotles way to build iris
                     :rdf/type :skos/Concept
                     :skos/prefLabel (lstr/->LangStr (:prefLabel node) "de")
                     :skos/definition (lstr/->LangStr (:definition node) "de")}]
    (if (:broader node)
      (assoc rdf-triples
             :skos/broader (keyword kw (:broader node))
             :skos/inScheme :greta/greta)
      (assoc rdf-triples :skos/topConceptOf :greta/greta))))

(defn csv->ttl
  [csv-file out-file]
  (let [g (ar/graph :simple)
        parsed-csv-data (parse-csv-data csv-file)
        topConcepts (filter (fn [x] (nil? (:broader x))) parsed-csv-data)]
    (reg/prefix :greta greta)
    (reg/prefix :skos "http://www.w3.org/2004/02/skos/core#")
    (reg/prefix :dct "http://purl.org/dc/terms/")
    (ar/read g "template.n3")
    ;; Add Concept Scheme
    (ar/add g {:rdf/about :greta/greta
               :rdf/type :skos/ConceptScheme
               :dct/title (lstr/->LangStr "Greta Kompetenzen" "de")
               :dct/description (lstr/->LangStr "Auflistung der GRETA Kompetenzen" "de")
               :skos/hasTopConcept (map #(keyword "greta" (:id %)) topConcepts)})
    ;; Add Concepts
    (doseq [node parsed-csv-data]
      (ar/add g (rdf-triples-from-node "greta" node))
      (ar/write g out-file :ttl))))

(defn csv-to-ttl
  [{:keys [csv out]}]
  (println "Converting" csv "and writing to" out)
  (csv->ttl (str csv) (str out)))

(comment
  (csv->ttl "GRETA.csv" "greta.ttl"))

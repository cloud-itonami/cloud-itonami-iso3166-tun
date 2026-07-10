(ns marketentry.facts "Tunisia market-entry catalog.")
(def catalog
  {"TUN" {:name "Tunisia"
          :owner-authority "HAICOP / TUNEPS"
          :legal-basis "Public Procurement Decree"
          :national-spec "TUNEPS supplier registration + RNE"
          :provenance "https://www.tuneps.tn/"
          :required-evidence ["RNE record" "TUNEPS registration record" "Tax ID record" "Authorized-representative record"]
          :rep-owner-authority "contracting authorities / HAICOP"
          :rep-legal-basis "Tunisian legal entity registration typically required for public awards"
          :rep-provenance "https://www.tuneps.tn/"
          :corporate-number-owner-authority "RNE / DGI"
          :corporate-number-legal-basis "RNE / tax ID"
          :corporate-number-provenance "https://www.registre-entreprises.tn/"}
   "USA" {:name "United States" :owner-authority "GSA/SAM.gov" :legal-basis "FAR" :national-spec "SAM.gov" :provenance "https://sam.gov/"
          :required-evidence ["EIN record" "SAM.gov registration record" "State business registration record" "SAM UEI verification record"]}
   "FRA" {:name "France" :owner-authority "PLACE" :legal-basis "Code de la commande publique" :national-spec "PLACE" :provenance "https://www.marches-publics.gouv.fr/"
          :required-evidence ["SIRET record" "PLACE registration" "RCS extract" "Authorized-representative record"]}
   "MAR" {:name "Morocco" :owner-authority "marchés publics" :legal-basis "Décret marchés publics" :national-spec "portail marchés publics" :provenance "https://www.marchespublics.gov.ma/"
          :required-evidence ["ICE/RC record" "e-procurement registration" "Commercial registry extract" "Authorized-representative record"]}})

(defn spec-basis [iso3] (get catalog iso3))
(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s) missing (remove catalog iso3s)]
     {:requested (count iso3s) :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note "R0 catalog seed"})))
(defn required-evidence-satisfied? [iso3 submitted]
  (when-let [{:keys [required-evidence]} (spec-basis iso3)]
    (= (count required-evidence) (count (filter (set submitted) required-evidence)))))
(defn evidence-checklist [iso3] (:required-evidence (spec-basis iso3) []))
(defn rep-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:rep-owner-authority sb)
      (select-keys sb [:rep-owner-authority :rep-legal-basis :rep-provenance]))))
(defn corporate-number-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:corporate-number-owner-authority sb)
      (select-keys sb [:corporate-number-owner-authority :corporate-number-legal-basis :corporate-number-provenance]))))

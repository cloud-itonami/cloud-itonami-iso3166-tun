(ns culture.facts
  "Country-level regional-culture catalog for Tunisia (TUN) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"TUN"
   [{:culture/id "tun.dish.brik"
     :culture/name "Brik"
     :culture/country "TUN"
     :culture/kind :dish
     :culture/summary "Stuffed malsouka pastry, commonly deep fried, originating from North Africa; the most recognized Tunisian version features a whole egg in a triangular pastry pocket with chopped onion, tuna, harissa, and parsley."
     :culture/url "https://en.wikipedia.org/wiki/Brik"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "tun.dish.couscous"
     :culture/name "Couscous"
     :culture/country "TUN"
     :culture/kind :dish
     :culture/summary "Tunisian couscous preparations typically feature spicy sauces with harissa and seafood specialties; in December 2020 Algeria, Mauritania, Morocco and Tunisia jointly obtained UNESCO Intangible Cultural Heritage recognition for the knowledge, know-how, and practices of couscous."
     :culture/url "https://en.wikipedia.org/wiki/Couscous"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "tun.dish.lablabi"
     :culture/name "Lablabi"
     :culture/country "TUN"
     :culture/kind :dish
     :culture/summary "Traditional Tunisian dish of chickpeas in a thin garlic- and cumin-flavored broth, served over small pieces of stale crusty bread; a staple comfort food in Tunisian culinary culture."
     :culture/url "https://en.wikipedia.org/wiki/Lablabi"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "tun.product.harissa"
     :culture/name "Harissa"
     :culture/country "TUN"
     :culture/kind :product
     :culture/summary "Tunisian hot chili pepper paste made from roasted red peppers, spices, and herbs, primarily produced in Nabeul on the Cape Bon Peninsula; UNESCO designated its knowledge, skills and culinary/social practices as Tunisian Intangible Cultural Heritage in 2022."
     :culture/url "https://en.wikipedia.org/wiki/Harissa"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "tun.beverage.boukha"
     :culture/name "Boukha"
     :culture/country "TUN"
     :culture/kind :beverage
     :culture/summary "Distilled beverage produced from figs, 36-40% alcohol, that originated within Tunisia's Jewish community in the 1880s and remains culturally significant in Tunisian culinary tradition."
     :culture/url "https://en.wikipedia.org/wiki/Boukha"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "tun.craft.chechia"
     :culture/name "Chechia"
     :culture/country "TUN"
     :culture/kind :craft
     :culture/summary "Traditional flexible, vermillion-red cap-shaped headgear worn in Tunisia; production has declined since independence in 1956 due to industrialization and changing fashion preferences."
     :culture/url "https://en.wikipedia.org/wiki/Chechia"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "tun.festival.international-festival-of-carthage"
     :culture/name "International Festival of Carthage"
     :culture/country "TUN"
     :culture/kind :festival
     :culture/summary "Annual music festival held each July and August since 1964 in the coastal city of Carthage, Tunisia, expanded from a two-day jazz event into a showcase of Arab-Mediterranean music, dance, theater, and ballet."
     :culture/url "https://en.wikipedia.org/wiki/International_Festival_of_Carthage"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "tun.heritage.medina-of-tunis"
     :culture/name "Medina of Tunis"
     :culture/country "TUN"
     :culture/kind :heritage
     :culture/summary "Historic old city quarter of Tunis, a UNESCO World Heritage Site since 1979, containing roughly 700 monuments blending Islamic, Andalusian, Roman, and Byzantine architectural influences."
     :culture/url "https://en.wikipedia.org/wiki/Medina_of_Tunis"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-tun culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "TUN"))
                 " TUN entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))

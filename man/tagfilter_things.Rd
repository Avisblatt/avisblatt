% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tagfilters_things.R
\name{tagfilter_things}
\alias{tagfilter_things}
\alias{tagfilter_mercery}
\alias{tagfilter_bag}
\alias{tagfilter_animalraw}
\alias{tagfilter_plantraw}
\alias{tagfilter_umbrella}
\alias{tagfilter_carriage}
\alias{tagfilter_pushchair}
\alias{tagfilter_storage}
\alias{tagfilter_buildingcomponents}
\alias{tagfilter_suitcase}
\alias{tagfilter_measure}
\alias{tagfilter_trolley}
\alias{tagfilter_health}
\alias{tagfilter_weapon}
\alias{tagfilter_shopequip}
\alias{tagfilter_tool}
\alias{tagfilter_stationary}
\alias{tagfilter_jewellery}
\alias{tagfilter_wood}
\alias{tagfilter_barrel}
\alias{tagfilter_tobaccoobjects}
\alias{tagfilter_hay}
\alias{tagfilter_woodobject}
\alias{tagfilter_dung}
\alias{tagfilter_plant}
\alias{tagfilter_glasses}
\alias{tagfilter_soil}
\alias{tagfilter_agriculturalobjects}
\alias{tagfilter_riding}
\alias{tagfilter_well}
\alias{tagfilter_naturalia}
\alias{tagfilter_container}
\alias{tagfilter_firestart}
\alias{tagfilter_extinguisher}
\alias{tagfilter_firework}
\alias{tagfilter_antique}
\alias{tagfilter_key}
\alias{tagfilter_cane}
\alias{tagfilter_wineobject}
\alias{tagfilter_rope}
\alias{tagfilter_tavernobject}
\alias{tagfilter_feed}
\alias{tagfilter_miscobject}
\title{Tagfilter Property}
\usage{
tagfilter_mercery()

tagfilter_bag()

tagfilter_animalraw()

tagfilter_plantraw()

tagfilter_umbrella()

tagfilter_carriage()

tagfilter_pushchair()

tagfilter_storage()

tagfilter_buildingcomponents()

tagfilter_suitcase()

tagfilter_measure()

tagfilter_trolley()

tagfilter_health()

tagfilter_weapon()

tagfilter_shopequip()

tagfilter_tool()

tagfilter_stationary()

tagfilter_jewellery()

tagfilter_wood()

tagfilter_barrel()

tagfilter_tobaccoobjects()

tagfilter_hay()

tagfilter_woodobject()

tagfilter_dung()

tagfilter_plant()

tagfilter_glasses()

tagfilter_soil()

tagfilter_agriculturalobjects()

tagfilter_riding()

tagfilter_well()

tagfilter_naturalia()

tagfilter_container()

tagfilter_firestart()

tagfilter_extinguisher()

tagfilter_firework()

tagfilter_antique()

tagfilter_key()

tagfilter_cane()

tagfilter_wineobject()

tagfilter_rope()

tagfilter_tavernobject()

tagfilter_feed()

tagfilter_miscobject()
}
\description{
Tagfilters are regular expression based filters designed to tag ads in order
to classify ads based on their content. The avisblatt R package comes with
curated filters to search for ads mentioning things not covered by the 
tagfilter families textiles, foodstuff and household. These include everything from
different accessories like mercery, umbrellas or purses to storage objects, carriages 
and dung.
}
\details{
Tagfilters can only predict if an ad is pertinent to a given topic. 
Depending on the complexity of the topic and the development stage of a 
tagfilter, there can be a considerable number of false positives and false 
negatives. 

The tagfilters help site provides you with a list of available tagfilters
families.
}
\seealso{
tagfilters
}

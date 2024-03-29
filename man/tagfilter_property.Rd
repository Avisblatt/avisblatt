% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tagfilters_property.R
\name{tagfilter_property}
\alias{tagfilter_property}
\alias{tagfilter_housing_rent1}
\alias{tagfilter_housing_rent2}
\alias{tagfilter_housing_sale1}
\alias{tagfilter_housing_sale2}
\alias{tagfilter_housing_sale3}
\alias{tagfilter_housing_sale4}
\alias{tagfilter_housing_sale5}
\alias{tagfilter_housing_other1}
\alias{tagfilter_housing_other2}
\alias{tagfilter_churchseat}
\title{Tagfilter Property}
\usage{
tagfilter_housing_rent1()

tagfilter_housing_rent2()

tagfilter_housing_sale1()

tagfilter_housing_sale2()

tagfilter_housing_sale3()

tagfilter_housing_sale4()

tagfilter_housing_sale5()

tagfilter_housing_other1()

tagfilter_housing_other2()

tagfilter_churchseat()
}
\description{
Tagfilters are regular expression based filters designed to tag ads in order
to classify ads based on their content. The avisblatt R package comes with
curated filters to search for ads concerning the housing and property market,
and the (tradable) entitlement to fixed seats in churches.

Tagfilters are used internally in the creation of yearly collections
of Avisblatt ads, to attribute tags to different ads.
}
\details{
Tagfilters can only predict if an ad is pertinent to a given topic.
Depending on the complexity of the topic and the development stage of a
tagfilter, there can be a considerable number of false positives and false
negatives.

The precision and sensitivity of some (families of) tagfilters can be
measured by comparison to a manual classification for four sample years
(1734, 1754, 1774 and 1834) from an early stage of the Avisblatt project.
Since the manual classification does often only roughly match the scope of
the tagfilters, their true precision and sensitivity are underestimated.

Calculated that way, the family of tagfilters concerning housing and property
shows a precision >94% and a sensitivity >90%. The churchseat tagfilter shows
a precision >96% and a sensitivity >96%.

The tagfilters help site provides you with a list of available tagfilters
families.
}
\seealso{
tagfilters
}

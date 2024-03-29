% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tagfilters_attributes.R
\name{tagfilter_attributes}
\alias{tagfilter_attributes}
\alias{tagfilter_secondhand}
\alias{tagfilter_attributes_female}
\title{Tagfilter Attributes}
\usage{
tagfilter_secondhand()

tagfilter_attributes_female()
}
\description{
#' Tagfilters are regular expression based filters designed to tag ads in order
to classify ads based on their content. The avisblatt R package comes with
curated filters to search for ads related to specific attributes or 
qualities, like female persons or second-hand goods.
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

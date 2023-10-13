#' Data on party policy ambiguity
#'
#' Data from Eurobarometer surveys and political texts on voter 
#' and party positions in 14 Western European Democracies.
#'
#' @docType data
#'
#'
#' A data frame with columns:
#' \describe{
#'  \item{lnsd1}{Ambiguity of party platform.}
#'  \item{y1}{Party platform mean.}
#'  \item{y1sq}{Party platform mean squared.}
#'  \item{q}{Wordcount in party manifesto.}
#' }
#'
#' @usage data(ambiguity)
#' @keywords datasets
#'
#' @references Braeuninger, Thomas; Giger, Nathalie, 2016, "Replication Data for: 
#' Strategic ambiguity of party positions in multiparty competition", 
#' \doi{https://doi.org/10.7910/DVN/GK9VCH}, Harvard Dataverse, V1.
#'
#' @source \doi{https://doi.org/10.7910/DVN/GK9VCH}
#'
#' @examples
#' data(ambiguity)
"ambiguity"

#' Flood data
#'
#' Recorded individual flood events from 1980 to 2008 in the world.
#'
#' @docType data
#'
#'
#' A data frame with columns:
#' \describe{
#'  \item{lndis_loss_overall_usd1995}{Logged economic loss due to floods.}
#'  \item{lnsum_precip_abs_pos}{Flood hazard magnitude.}
#'  \item{lnprop_sum_precip_abs_pos}{Flood propensity.}
#'  \item{lngdppc}{Per capita income of the country.}
#'  \item{lngdp}{Gross domestic product of the country.}
#' }
#'
#' @usage data(flood)
#' @keywords datasets
#'
#' @references Neumayer, Eric, Pluemper, Thomas, & Barthel, Fabian. 2014. 
#' The Political Economy of Natural Disaster Damage. Global Environmental Change, 24, 8-19.
#'
#' @source \doi{https://doi.org/10.7910/DVN/W8US2Q}
#'
#' @examples
#' data(flood)
"flood"
#' Title
#'
#' @param data_aggregated
#' @param variable
#' @param group
#' @param col_enrichment
#' @param group_enrichment
#'
#' @return
#' @export
#'
#' @examples
relatif_risk <- function(data_aggregated, variable ,group, col_enrichment,group_enrichment) {

  pourcentage_category_unwanted <- data_aggregated  %>%
    filter(!!sym(col_enrichment) != group_enrichment)

  pourcentage_category_wanted <- data_aggregated%>%
    filter(!!sym(col_enrichment) == group_enrichment)
  pourcentage_other_wanted <- data.frame()
  pourcentage_other_unwanted <- data.frame()
  for (i in 1:length(group)) {
    pourcentage_other_wanted_current = pourcentage_category_wanted%>%filter(!!sym(variable[i]) != group[i])
    pourcentage_category_wanted= pourcentage_category_wanted%>% filter(!!sym(variable[i]) == group[i])
    pourcentage_other_wanted= union_all(pourcentage_other_wanted,pourcentage_other_wanted_current)
    pourcentage_other_unwanted_current = pourcentage_category_unwanted%>%filter(!!sym(variable[i]) != group[i])
    pourcentage_category_unwanted= pourcentage_category_unwanted%>% filter(!!sym(variable[i]) == group[i])
    pourcentage_other_unwanted= union_all(pourcentage_other_unwanted,pourcentage_other_unwanted_current)
  }
  X = sum(pourcentage_category_wanted$cases)
  Y = sum(pourcentage_category_unwanted$cases)
  W = sum(pourcentage_other_wanted$cases)
  Z = sum(pourcentage_other_unwanted$cases)
  RR = X*(Z+W)/((Y+X)*W)
  return(RR)
}


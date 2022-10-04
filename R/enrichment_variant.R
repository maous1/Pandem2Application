#' Title
#'
#' @param data_aggregated
#' @param variable
#' @param group
#' @param multiplicateur
#' @param time
#' @param col_enrichment
#' @param group_enrichment
#'
#' @return
#' @export
#'
#' @examples
enrichment_variant <- function(data_aggregated, variable ,group, col_enrichment,group_enrichment, multiplicateur,time) {


  semaine <- c(unique(data_aggregated[time]))
  full_aggregated <- tibble()
  for (week in semaine) {
    data_aggregated_week <- data_aggregated %>% filter({{time}} == week)

    pourcentage_category_unwanted <- data_aggregated_week  %>%
      filter(!!sym(col_enrichment) != group_enrichment)

    pourcentage_category_wanted <- data_aggregated_week%>%
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
    m = multiplicateur
    X = pourcentage_category_wanted$cases
    Y = sum(pourcentage_category_unwanted$cases)
    W = sum(pourcentage_other_wanted$cases)
    Z = sum(pourcentage_other_unwanted$cases)
    RR = X*(Z+W)/((Y+X)*W)


    I = (-X*Z-W*X+RR*m*(Y*W+X*W))/(Y*RR*m+RR*m*X+W+Z)

    if(I>Y){I=Y}
    if(I>W){I=W}
    I = floor(I)

    new_RR = ((X+I)/(X+Y))/((W-I)/(Z+W))
    print(paste("The relatif risk in", week, "=", RR))
    print(paste("The new relatif risk in ", week, "=", new_RR))


    #######
    pourcentage_category_wanted$cases = pourcentage_category_wanted$cases+I
    full_aggregated = union_all(full_aggregated,pourcentage_category_wanted)
    #######
    pourcentage_category_unwanted = pourcentage_category_unwanted %>%
      ungroup() %>%
      mutate(pourcentage = cases / sum(cases))%>%
      mutate(remove = (pourcentage * I))%>%
      mutate(floor = floor(remove))%>%
      mutate(remove = remove-floor)%>%
      arrange(desc(remove))

    miss = I - sum(pourcentage_category_unwanted$floor)
    pourcentage_category_unwanted$floor[1:miss] = pourcentage_category_unwanted$floor[1:miss]+1

    variant = pourcentage_category_unwanted %>% ungroup() %>%select({{col_enrichment}},floor)%>%filter(floor!=0)
    pourcentage_category_unwanted = pourcentage_category_unwanted %>% mutate(cases = cases-floor) %>% select(-c("pourcentage","remove","floor"))
    full_aggregated = union_all(full_aggregated,pourcentage_category_unwanted)
    #######
    pourcentage_other_wanted = pourcentage_other_wanted %>%
      ungroup() %>%
      mutate(pourcentage = cases / sum(cases))%>%
      mutate(remove = (pourcentage * I))%>%
      mutate(floor = floor(remove))%>%
      mutate(remove = remove-floor)%>%
      arrange(desc(remove))

    miss = I - sum(pourcentage_other_wanted$floor)
    pourcentage_other_wanted$floor[1:miss] = pourcentage_other_wanted$floor[1:miss]+1
    category = pourcentage_other_wanted %>% ungroup() %>%select(-c({{col_enrichment}},"cases","pourcentage","remove"))%>%filter(floor!=0)
    pourcentage_other_wanted = pourcentage_other_wanted %>% mutate(cases = cases-floor) %>% select(-c("pourcentage","remove","floor"))
    full_aggregated = union_all(full_aggregated,pourcentage_other_wanted)

    #######
    category_desagregated = category %>% expandRows(count = "floor")
    variant_desagregated = expandRows(dataset = variant,count = "floor")
    colnames(variant_desagregated) = "sample__"
    variant_desagregated = sample(variant_desagregated$sample__,replace = F)
    category_desagregated [col_enrichment] = variant_desagregated

    pourcentage_other_unwanted_current = category_desagregated%>% group_by_all()%>% summarise(cases =n())
    pourcentage_other_unwanted = union_all(pourcentage_other_unwanted_current,pourcentage_other_unwanted)

    column = colnames(pourcentage_other_unwanted)
    column = column[column!="cases"]
    pourcentage_other_unwanted = pourcentage_other_unwanted%>%
      group_by(across({{column}}))%>%
      summarise(cases = sum(cases))

    full_aggregated = union_all(full_aggregated,pourcentage_other_unwanted)
  }

  return(full_aggregated)
}

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


  names(data_aggregated)[names(data_aggregated) %in% time] <- "time"
  semaine <- unique(data_aggregated$time)
  full_aggregated <- data.frame()
  for (week in semaine) {
    data_aggregated_week <- data_aggregated %>% filter(time == week)

    full_desaggregated_week <- expandRows(data_aggregated_week, count = "cases", drop = T)

    pourcentage_wanted <- full_desaggregated_week %>%
      group_by_all() %>%
      summarise(nb = n()) %>%
      group_by(across(all_of(variable))) %>%
      mutate(pourcentage = nb / sum(nb))%>%
      filter(!!sym(col_enrichment) == group_enrichment)
    for (i in 1:length(group)) {
      pourcentage_wanted= pourcentage_wanted%>% filter(!!sym(variable[i]) == group[i])
    }


    targetted_category <- full_desaggregated_week
    other_category = data.frame()
    for (i in 1:length(group)) {
      other_category_current = targetted_category%>%filter(!!sym(variable[i]) != group[i])
      targetted_category= targetted_category%>% filter(!!sym(variable[i]) == group[i])
      other_category= union_all(other_category,other_category_current)
    }


    targetted_category_unwanted <- targetted_category %>% filter(!!sym(col_enrichment) != group_enrichment)

    other_category_wanted <- other_category %>% filter(!!sym(col_enrichment) == group_enrichment)
    other_category_unwanted <- other_category %>% filter(!!sym(col_enrichment) != group_enrichment)

    fullother <- rbind(other_category_wanted, other_category_unwanted)


    pourcentage_other <- fullother %>%
      group_by_all() %>%
      summarise(nb = n()) %>%
      group_by(across(all_of(variable))) %>%
      mutate(pourcentage = nb / sum(nb)) %>%
      filter(!!sym(col_enrichment) == group_enrichment)


    while (ifelse(test = is_empty(pourcentage_wanted$pourcentage),
                  yes = ifelse(test = is_empty(pourcentage_other$pourcentage),
                               yes = FALSE,
                               no = TRUE),
                  no = ifelse(test = is_empty(pourcentage_other$pourcentage),
                              yes = FALSE,
                              no = sum(pourcentage_wanted$pourcentage) < sum(pourcentage_other$pourcentage) * multiplicateur & sum(pourcentage_wanted$pourcentage) != 1))  ) {

      targetted_category <- full_desaggregated_week
      other_category = data.frame()
      for (i in 1:length(group)) {
        other_category_current = targetted_category%>%filter(!!sym(variable[i]) != group[i])
        targetted_category= targetted_category%>% filter(!!sym(variable[i]) == group[i])
        other_category= union_all(other_category,other_category_current)
      }
      targetted_category_wanted <- targetted_category %>% filter(!!sym(col_enrichment) == group_enrichment)
      targetted_category_unwanted <- targetted_category %>% filter(!!sym(col_enrichment) != group_enrichment)

      other_category_wanted <- other_category %>% filter(!!sym(col_enrichment) == group_enrichment)
      other_category_unwanted <- other_category %>% filter(!!sym(col_enrichment) != group_enrichment)


      random <- sample(1:length(targetted_category_unwanted$country_code), 1)
      row_wanted <- targetted_category_unwanted [random,]
      targetted_category_unwanted <- targetted_category_unwanted[-random,]

      random <- sample(1:length(other_category_wanted$country_code), 1)
      row_unwanted <- other_category_wanted[random,]
      other_category_wanted <- other_category_wanted[-random,]
      row_unwanted[col_enrichment] <- row_wanted[col_enrichment]
      row_wanted[col_enrichment] = group_enrichment
      full_desaggregated_week <- rbind(targetted_category_unwanted, targetted_category_wanted, row_wanted, other_category_wanted, other_category_unwanted, row_unwanted)
      fullother <- rbind(other_category_wanted, other_category_unwanted, row_unwanted)

      pourcentage_wanted <- full_desaggregated_week %>%
        group_by_all() %>%
        summarise(nb = n()) %>%
        group_by(across(all_of(variable))) %>%
        mutate(pourcentage = nb / sum(nb))%>% filter(!!sym(col_enrichment) == group_enrichment)
      for (i in 1:length(group)) {
        pourcentage_wanted= pourcentage_wanted%>% filter(!!sym(variable[i]) == group[i])
      }

      pourcentage_other <- fullother %>%
        group_by_all() %>%
        summarise(nb = n()) %>%
        group_by(across(all_of(variable))) %>%
        mutate(pourcentage = nb / sum(nb)) %>%
        filter(!!sym(col_enrichment) == group_enrichment)
    }

    full_aggregated_week <- full_desaggregated_week %>%
      group_by_all() %>%
      summarise(cases = n())
    full_aggregated <- union_all(full_aggregated, full_aggregated_week)
    print(paste("pourcentage in week ", week, "=", sum(pourcentage_wanted$pourcentage),"for the category"))
    print(paste("pourcentage in week ", week, "=", sum(pourcentage_other$pourcentage),"for other category"))
  }

  return(full_aggregated)
}

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
#' @import dplyr
#' @import lubridate
#' @importFrom splitstackshape expandRows
#' @return
#' @export
#'
#' @examples
enrichment_variant <- function(data_aggregated, variable ,group, col_enrichment,group_enrichment, multiplicateur,time) {

  semaine <- c(unique(data_aggregated[time]))
  full_aggregated <- tibble()
  for (week in unlist(semaine)) {
    data_aggregated_week <- data_aggregated %>% filter(!!sym(time) == week)

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
    X = sum(pourcentage_category_wanted$cases)
    Y = sum(pourcentage_category_unwanted$cases)
    W = sum(pourcentage_other_wanted$cases)
    Z = sum(pourcentage_other_unwanted$cases)
    RR = X*(Z+W)/((Y+X)*W)


    I = (-X*Z-W*X+RR*m*(Y*W+X*W))/(Y*RR*m+RR*m*X+W+Z)

    if(I!="NaN"){
      if(I>Y){I=Y}
      if(I>W){I=W}
      if(I< -X){I=-X}
      if(I< -Z){I=-Z}

      if(I>0){I = floor(I)}

      if(I<0){I = -floor(-I)}

      new_RR = ((X+I)/(X+Y))/((W-I)/(Z+W))
      print(paste("The relatif risk in", week, "=", RR))
      print(paste("The new relatif risk in ", week, "=", new_RR))

      if(I ==0){
        full_aggregated = union_all(full_aggregated,data_aggregated_week)
      }else if (I>0){

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
        if(miss>0){
          pourcentage_category_unwanted$floor[1:miss] = pourcentage_category_unwanted$floor[1:miss]+1
        }
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
        if(miss>0){
          pourcentage_other_wanted$floor[1:miss] = pourcentage_other_wanted$floor[1:miss]+1
        }

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
      } else{
        #######
        pourcentage_category_wanted$cases = pourcentage_category_wanted$cases+I
        full_aggregated = union_all(full_aggregated,pourcentage_category_wanted)
        #######

        pourcentage_other_unwanted = pourcentage_other_unwanted %>%
          ungroup() %>%
          mutate(pourcentage = cases / sum(cases))%>%
          mutate(remove = (pourcentage * I))%>%
          mutate(floor = -floor(-remove))%>%
          mutate(remove = remove-floor)%>%
          arrange((remove))

        miss = -I + sum(pourcentage_other_unwanted$floor)
        if(miss>0){
          pourcentage_other_unwanted$floor[1:miss] = pourcentage_other_unwanted$floor[1:miss]-1
        }

        variant = pourcentage_other_unwanted %>% ungroup() %>%select(c({{col_enrichment}},"floor"))%>%filter(floor!=0)%>% mutate(floor = -floor)

        category = pourcentage_other_unwanted %>% ungroup() %>%select(-c({{col_enrichment}},"pourcentage","remove","cases")) %>%filter(floor!=0)%>% mutate(floor = -floor)
        pourcentage_other_unwanted = pourcentage_other_unwanted %>% mutate(cases = cases+floor) %>% select(-c("pourcentage","remove","floor"))


        full_aggregated = union_all(full_aggregated,pourcentage_other_unwanted)

        ########
        category = category %>% mutate({{col_enrichment}} := group_enrichment)%>% mutate(cases = floor)%>% select(-"floor")
        pourcentage_other_wanted = union_all(pourcentage_other_wanted,category)
        full_aggregated = union_all(full_aggregated,pourcentage_other_wanted)

        ########
        category = pourcentage_category_unwanted %>% select(-{{col_enrichment}},-"cases") %>% distinct()

        variant = variant %>% group_by(across({{col_enrichment}})) %>% summarise(cases = sum(floor))
        pourcentage_category_unwanted = union_all(pourcentage_category_unwanted,bind_cols(category,variant))
        full_aggregated = union_all(full_aggregated,pourcentage_category_unwanted)
      }
    }else{
      full_aggregated = union_all(full_aggregated,data_aggregated_week)
      warning(paste0("No cases in week ", week))}
  }
  column = colnames(full_aggregated)
  column = column[column!="cases"]
  full_aggregated = full_aggregated%>%
    group_by(across({{column}}))%>%
    summarise(cases = sum(cases))
  return(full_aggregated)
}

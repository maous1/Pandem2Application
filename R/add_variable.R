#' Title
#'
#' @param data the dataset
#' @param nomVariable the name of the variable that will be added
#' @param pourcentage the percentage of the group in the dataset
#' @param group The name of the groups in the variable
#'
#' @return data
#' @export add_variable
#' @import dplyr
#' @import tidyr
#' @examples
add_variable <- function(data, nomVariable, pourcentage, group) {
  for (i in 1:length(group)) {
    data <- data %>% mutate(!!sym(group[i]) := round(cases * pourcentage[i]))
  }
  data <- data %>%
    select(-cases) %>%
    pivot_longer(group, names_to = nomVariable, values_to = "cases")

  return(data)
}

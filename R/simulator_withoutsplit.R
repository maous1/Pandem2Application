#' Predict the variant from a training set
#'
#' Predict the variant from a training set with a column time and a column variant to a testset with a column time
#' The trainset and the testset must have the columns geolocation, time, count. You define the name of these columns in input and the columns must have the same name in the 2 data sets.
#' The trainset must have in addition an outcome column that you also define in input.
#'
#' @param trainset the dataset used to train the classifier
#' @param testset the dataset to which we will add a metadata
#' @param time the name of the column where the dates are found format = "%Y-%m-%d"
#' @param geolocalisation the name of the column where the different regions are located
#' @param count the name of the column used to desaggregate the data
#' @param outcome the name of the trainset column where the metadata to be added to the testset is located
#' @param factor The number of sequence used by time for the trainset to reduce the execution time
#'
#' @return The function returns the testset dataset with an outcome column based on the trainset. The output dataset is well aggregated.
#' @export simulator_withoutsplit
#' @import class
#' @import dplyr
#' @import purrr
#' @import lubridate
#' @importFrom splitstackshape expandRows
simulator_withoutsplit <- function(trainset, testset, time, outcome, count, factor) {

  if (!any(names(trainset) %in% time)) {
    if (!any(names(testset) %in% time)) {
      stop("error : wrong time in testset and trainset")
    }
    stop("error : wrong time in  trainset")
  }
  if (!any(names(testset) %in% time)) {
    stop("error : wrong time in testset")
  }

  if (!any(names(trainset) %in% outcome)) {
    stop("error : wrong outcome in trainset")
  }

  if (!any(names(trainset) %in% count)) {
    if (!any(names(testset) %in% count)) {
      stop("error : wrong count in testset and trainset")
    }
    stop("error : wrong count in  trainset")
  }
  if (!any(names(testset) %in% count)) {
    stop("error : wrong count in testset")
  }


  names(trainset)[names(trainset) %in% count] <- "new_cases"
  names(trainset)[names(trainset) %in% time] <- "time"
  names(testset)[names(testset) %in% count] <- "new_cases"
  names(testset)[names(testset) %in% time] <- "time"

  ####### split the dataframe in a list of dataframe (one per geolocalisation) and keep common geolocalisation

  trainset_list <- na.omit(trainset)
  testset_list <- na.omit(testset) %>% filter(new_cases > 0)

  ###### reduce the training dataset

  trainset_list <- trainset_list %>%
    group_by(time) %>%
    mutate(sum = sum(new_cases)) %>%
    rowwise() %>%
    mutate(new_cases = ifelse(sum < factor, new_cases, as.integer(round(factor * new_cases / sum)))) %>%
    filter(new_cases > 0) %>%
    select(-sum)

  ######### definition of the function to simulate on 1 geolocalisation

  simulator_1geo <- function(trainset_1geo, testset_1geo, time, outcome, count = NULL) {
    set.seed(2)

    trainset_1geo <- trainset_1geo %>%
      expandRows(count = count, drop = T) %>%
      rowwise() %>%
      mutate(time_num = as.numeric(as.Date(time))) %>%
      mutate(time_jitter = time_num + rnorm(length(time_num), 0, 3))


    testset_1geo <- testset_1geo %>%
      expandRows(count = count, drop = T) %>%
      mutate(time_num = as.numeric(as.Date(time))) %>%
      mutate(time_jitter = time_num + rnorm(length(time_num), 0, 3))

    ######## knn prediction

    trainset_predictor <- trainset_1geo["time_jitter"]
    testset_predictor <- testset_1geo["time_jitter"]
    trainset_class <- trainset_1geo[outcome]


    pr <- knn(
      train = data.frame(trainset_predictor),
      test = testset_predictor,
      cl = unlist(trainset_class),
      k = 1
    )

    # Create variant variable and aggregation
    testset_1geo[outcome] <- as.character(pr)

    # Concatenate prediction file for all countries

    testset_1geo <- testset_1geo %>%
      select(-c(time_jitter, time_num)) %>%
      group_by_all() %>%
      summarise(new_cases = n(), .groups = "drop")
    return(testset_1geo)
  }

  ###### apply the knn prediction on each compoent of the lists
  testset_predicted <-  simulator_1geo(trainset_1geo = trainset_list, testset_1geo = testset_list, time = "time", outcome = outcome, count = "new_cases")

  names(testset_predicted)[names(testset_predicted) %in% "time"] <- time
  names(testset_predicted)[names(testset_predicted) %in% "new_cases"] <- count
  return(testset_predicted)
}

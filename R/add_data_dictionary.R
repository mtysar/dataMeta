#' Add a data dictionary to a data frame in a single step.
#' 
#' \code{add_data_dictionary} wraps the three functions of the dataMeta package
#' into a single step. This is a tidy function that takes a data frame (or tibble) and returns
#' a modified data frame. For the function to run, the following parameters are needed.
#'
#' @param data Data.frame. The data set for which the user is creating the 
#' dictionary for.
#' @param main_string A character string describing the original dataset.
#' @param variable_description A string vector representing the different descriptions 
#' that the user will give to each variable name from the original dataset. These 
#' need to be in the same order as the original dataset's variable names.
#' @param variable_type A vector of integers with values 0 or 1, only. Use 0 for
#' variable names for which a range of values will be presented and 1 to show unique 
#' cases of each variable name option. See examples, below.
#' @param variable_sensitive An optional vector of integers with values 0 or 1, 
#' only. Use 1 for variables that contain sensitive information (e.g., personal 
#' health information).
#' @param na.rm Logical. Whether to remove \code{NA} when determining the range 
#' for variables with \code{variable_type == 0} in \code{linker}
#' @return This function will return an R dataset containing metadata stored in 
#' its attributes. Attributes added will include: a data dictionary, number of columns, 
#' number of rows, the name of the author or user who created the dictionary and added it,
#' the time when it was last edited and a brief description of the original dataset.
#' @examples
#' # example original data set for which a dictionary will be made
#' data("esoph")
#' my.data <- esoph
#' 
#' my.data <- add_data_dictionary(
#'   data = my.data,
#'   main_string = "This dataset describes tobacco and alcohol consumption at different age groups.",
#'   variable_description = c(
#'     "age group in years", 
#'     "alcohol consumption in gm/day", 
#'     "tobacco consumption in gm/day", 
#'     "number of cases (showing a range)", 
#'     "number of controls (showing range)"
#'   ),
#'   variable_type = c(0, 0, 0, 0, 0),
#'   na.rm = FALSE
#' )
#' 
#' attributes(my.data.2)
#' 
#' @export

add_data_dictionary <- function(
  data,
  main_string,
  variable_description,
  variable_type,
  variable_sensitive = NULL,
  na.rm = TRUE
) {

  linker <- build_linker(
    my.data = data,
    variable_description = variable_description,
    variable_type = variable_type,
    variable_sensitive = variable_sensitive
  )
  
  dict <- build_dict(
    my.data = as.data.frame(data),  # play well with tibbles
    linker = linker,
    option_description = NULL,
    prompt_varopts = FALSE,
    hide_sensitive = TRUE,  # if we set variables as sensitive, hide them
    na.rm = na.rm
  )
  
  # Return modified data frame
  incorporate_attr(
    data,
    data.dictionary = dict,
    main_string = main
  )
}
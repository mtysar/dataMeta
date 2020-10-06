#' Print data dictionary as a nicely formatted \code{gt} table.
#' 
#' \code{print_data_dictionary} takes a data frame to which a data dictionary has
#' been added using the \code{gt} package. For the function to run, the following parameters are needed.
#'
#' @param data Data.frame. Dataset that has attributes added, including a data dictionary.
#' @return This function will return a \code{gt} object.
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
#' print_data_dictionary(my.data)
#' 
#' @export

print_data_dictionary <- function(data) {
  
  nm <- deparse(substitute(data))
  
  attr(data, "dictionary") %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md(paste0("`", nm, "`")),
      subtitle = attr(data, "main")
    ) %>%
    gt::cols_label(
      variable_name = gt::md("**Variable Name**"),
      variable_description = gt::md("**Description**"),
      variable_options = gt::md("**Variable options**")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        font = "monospace"
      ),
      locations = gt::cells_body(
        columns = gt::vars(variable_name)
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        style = "italic"
      ),
      locations = gt::cells_body(
        columns = gt::vars(variable_options),
        rows = is.na(variable_options) | variable_options == "hidden"
      )
    )
}
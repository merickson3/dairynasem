#' Extract some part of the simulation results
#'
#' This extracts an element of a dn_out object returned by dn_run, for all simulations.
#'
#' @param dn_out the output of a Dairy NASEM simulation given by dn_run.
#' @param which_output which_output specifies which part of the NASEM results should be returned as a dataframe.
#' @details which_output specifies which part of the NASEM results should be returned as a dataframe.
#' \itemize{
#'  \item{Tbl1_1}{Physiological State/Management}
#'  \item{Tbl1_2}{Entered Performance}
#'  \item{Tbl1_3}{Predicted Production Variables}
#'  \item{Tbl2_1}{Macronutrients}
#'  \item{Tbl2_2}{Diet Ingredients}
#'  \item{Tbl3_1}{Ingredient Macronutrient Contributions}
#'  \item{Tbl4_1}{Energy Supply}
#'  \item{Tbl4_2}{NEL and ME Requirements}
#'  \item{Tbl4_3}{Nutrient Contributions to DE}
#'  \item{Tbl5_1}{Fatty Acid Supply}
#'  \item{Tbl6_1}{Animal Inputs}
#'  \item{Tbl6_2}{Net Protein and Metabolizable Protein}
#'  \item{Tbl6_3}{Predicted and Target Supply of Metabolizable Protein and Amino Acids}
#'  \item{Tbl6_4}{EAA Partitioning using Predicted Milk NP, g/d}
#'  \item{Tbl7_1}{Minerals}
#'  \item{Tbl7_2}{Vitamin Supply and Requirements}
#'  \item{Tbl7_3}{Water, Volatile Solids, and Methane}
#'  \item{Tbl8_1}{Nitrogen and Mineral Excretion}
#'  \item{Tbl8_2}{Ingredient Mineral Contributions}
#'  }
#' @md
#' @return A dataframe with selected results extracted from an object produced by dn_run.
#' @importFrom magrittr %>%
#' @export
#'
#'
#' @examples
#'
#' # Run simulations
#'
#' data(diet_comp_ex_1)
#' data(scenario_ex_1)
#' dn_out = dn_run(diet_comp = diet_comp_ex_1, scenario = scenario_ex_1)
#'
#'
#' # Extract results related to dry matter intake
#'
#' dmi_results <- dn_extract(dn_out, "dmi")
#'
#'
#' # Extract results for Table 4-1
#'
#' table_4_1_results <- dn_extract(dn_out, "Tbl4_1")


dn_extract <- function(dn_out, which_output) {
  if (is.symbol(which_output)) stop("Please put the value for which_output in quotes.")
  if(!is.character(which_output)){stop("which_output must be a string enclosed in quotes")}
  if(!is.list(dn_out)){stop("dn_out must be a list of simulations returned by dn_run")}

  if (grepl("Tbl", which_output)) {
    # Table out method (single column table)
    # could have arguments for wide vs. long
    # Maybe it would be nice to have tidy output with table headers as "Category" and subheaders as "Variable"
    # need a separate method per table to do this.

    dn_out %>%
      purrr::map(., ~ .[[which_output]]) %>%
      purrr::map(., ~ data.frame(.)) %>%
      purrr::map(., ~ tibble::rownames_to_column(data.frame(.))) %>%
      dplyr::bind_rows(., .id = "SimID") %>%
      dplyr::mutate(rowname = janitor::make_clean_names(rowname, allow_dupes = TRUE)) %>%
      #rename(AA_Name = rowname) %>% only works for tables with AA
      # tidyr::pivot_wider(id_cols = SimID, names_from = "rowname", values_from = "value") %>%
      janitor::clean_names(., "none")

  } else {
    # non-table out method
    dn_out %>%
      purrr::map(., ~ .[[which_output]]) %>%
      dplyr::bind_rows(.id = "SimID") %>%
      tidyr::separate(col = SimID, sep = "__", into = c("DietID", "ScenarioID"))
  }
}


#' Export text or spreadsheet results
#'
#' This exports a text or xlsx file from a dn_out object produced by dn_run.
#'
#' @param filetype A character string specifying the type of file to export. Valid options are "txt" (default) and "xlsx".
#' @param dn_out The output of a Dairy NASEM simulation given by dn_run.
#'
#' @details Writes a .txt or .xlsx file to the working directory with simulation results for each element in a dn_out nested list (produced by \code{\link[dairynasem]{dn_run}}). Output is named with the DietID and ScenarioID.
#' @return This function does not have a direct return value. It creates and exports .txt or .xlsx files with the simulation results.
#' @export
#'
#' @examples
#'
#' # Run simulations
#'
#' data(diet_comp_ex_1)
#' data(scenario_ex_1)
#' dn_out = dn_run(diet_comp = diet_comp_ex_1, scenario = scenario_ex_1)
#'
#'
#' # Export the results to .txt files (default):
#'
#' dn_export(dn_out)
#'
#' # Export the results to .xlsx files:
#'
#' dn_export(dn_out, filetype = "xlsx")
#'

dn_export <- function(dn_out, filetype = "txt") {
  dn_out_2 <- dn_out %>%
    purrr::map(., ~ purrr::keep(., grepl("Tbl", names(.))))

  if(!is.list(dn_out)){stop("dn_out must be a list of simulations returned by dn_run")} # (protein_sims) %>% map(length)
  if(!is.character(filetype)){stop("filetype must be a string, either 'txt' or xlsx'")}else if (filetype != "txt" && filetype != "xlsx") {
    warning("Unrecognized filetype. Please use 'txt' or 'xlsx'. Exporting as 'txt' by default.")
    filetype <- "txt"}


  if (filetype == "txt") {
    dn_out_2 %>%
      names(.) %>%
      purrr::walk(~ capture.output(dn_out_2[[.]], file = paste0(., "_NASEM_results.txt")))

  } else if (filetype == "xlsx") {
    dn_out_2 %>%
      names(.) %>%
      purrr::map(~ openxlsx::write.xlsx(dn_out_2[[.]], file = paste0(., "_NASEM_results.xlsx"), rowNames = TRUE))
  }
  message("Exported files were written to the working directory.")
  # walk along names of dn_out_2 (for each simulation) and extract each element ([[.]]) of dn_out_2 with capture_output
}

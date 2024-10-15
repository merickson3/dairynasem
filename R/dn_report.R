

#' Generate an R Markdown report of results.
#'
#' This function creates an R Markdown report from the results of a dairynasem simulation. Only one element (i.e., one diet and scenario combination) of a dn_run object can be viewed in a report.
#' @param dn_simulation A single simulation, i.e., one element of a dn_run list object.
#' @param filename A character string specifying the filename.
#' @param author The name to add to the top of the R Markdown report.
#'
#' @return an Rmd file
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
#' # Generate a report
#'
#' dn_report(dn_out[[1]], "file2", "My Name")
#'
dn_report <- function(dn_simulation, filename, author) {
  ## %######################################################%##
  #                                                          #
  ####     Set global options with internal functions     ####
  #                                                          #
  ## %######################################################%##

  format_for_gt <- function(mat) {
    mat %>%
      data.frame() %>%
      tibble::rownames_to_column()
  }

  my_gt <- function(df) {
    df %>%
      gt::gt() %>%
      gt::fmt_scientific(
        columns = where(~ is.numeric(.x) && max(.x, na.rm = TRUE) > 1E4)
      )
  }

  ## %######################################################%##
  #                                                          #
  ####                   create tables                    ####
  #                                                          #
  ## %######################################################%##
  # Functions for gt and formatting need to be separate to apply custom formatting in certain cases.
  Tbl1_1 <- dn_simulation$Tbl1_1 %>%
    format_for_gt() %>%
    my_gt()

  Tbl1_2 <- dn_simulation$Tbl1_2 %>%
    format_for_gt() %>%
    dplyr::mutate(`Target.Performance.` = as.numeric(`Target.Performance.`)) %>%
    my_gt() %>%
    gt::fmt_number()

  Tbl1_3 <- dn_simulation$Tbl1_3 %>%
    format_for_gt() %>%
    dplyr::mutate(Values = as.numeric(Values)) %>%
    my_gt() %>%
    gt::fmt_number()

  Tbl2_1 <- dn_simulation$Tbl2_1 %>%
    format_for_gt() %>%
    my_gt() %>%
    gt::fmt_number()

  Tbl2_2 <- dn_simulation$Tbl2_2 %>%
    format_for_gt() %>%
    dplyr::mutate(dplyr::across(-c(1:2), \(x) as.numeric(x))) %>%
    dplyr::mutate(Fd_DMInp = as.numeric(Fd_DMInp)) %>%
    my_gt() %>%
    gtExtras::gt_plt_bar(column = Fd_DMInp, keep_column = TRUE)

  # find the last barplot row and set it to NA to omit this barplot
  Tbl2_2[["_data"]]$DUPE_COLUMN_PLT[length(Tbl2_2[["_data"]]$DUPE_COLUMN_PLT)] <- NA

  Tbl3_1 <- dn_simulation$Tbl3_1 %>%
    format_for_gt() %>%
    dplyr::mutate(dplyr::across(-Fd_Name, \(x)  as.numeric(x))) %>%
    my_gt() %>%
    gt::fmt_number()

  Tbl4_1 <- dn_simulation$Tbl4_1 %>%
    format_for_gt() %>%
    dplyr::mutate(dplyr::across(-rowname, \(x)  as.numeric(x))) %>%
    my_gt() %>%
    gt::fmt_number()

  Tbl4_2 <- dn_simulation$Tbl4_2 %>%
    format_for_gt() %>%
    my_gt()

  Tbl4_3 <- dn_simulation$Tbl4_3 %>%
    format_for_gt() %>%
    my_gt()

  Tbl5_1 <- dn_simulation$Tbl5_1 %>%
    format_for_gt() %>%
    my_gt()

  Tbl6_1 <- dn_simulation$Tbl6_1 %>%
    format_for_gt() %>%
    my_gt()

  Tbl6_2 <- dn_simulation$Tbl6_2 %>%
    format_for_gt() %>%
    my_gt()

  Tbl6_3 <- dn_simulation$Tbl6_3 %>%
    format_for_gt() %>%
    my_gt()

  Tbl6_4 <- dn_simulation$Tbl6_4 %>%
    format_for_gt() %>%
    my_gt()

  Tbl6_5 <- dn_simulation$Tbl6_5 %>%
    format_for_gt() %>%
    my_gt()

  Tbl7_1 <- dn_simulation$Tbl7_1 %>%
    format_for_gt() %>%
    my_gt()

  Tbl7_2 <- dn_simulation$Tbl7_2 %>%
    format_for_gt() %>%
    my_gt()

  Tbl7_3 <- dn_simulation$Tbl7_3 %>%
    format_for_gt() %>%
    my_gt()

  Tbl8_1 <- dn_simulation$Tbl8_1 %>%
    format_for_gt() %>%
    my_gt()

  Tbl8_2 <- dn_simulation$Tbl8_2 %>%
    format_for_gt() %>%
    my_gt()

  ## %######################################################%##
  #                                                          #
  ####                   Create report                    ####
  #                                                          #
  ## %######################################################%##
  # dput(names(dn_result_single))

  dnt <- list(
    Tbl1_1 = Tbl1_1,
    Tbl1_2 = Tbl1_2,
    Tbl1_3 = Tbl1_3,
    Tbl2_1 = Tbl2_1,
    Tbl2_2 = Tbl2_2,
    Tbl3_1 = Tbl3_1,
    Tbl4_1 = Tbl4_1,
    Tbl4_2 = Tbl4_2,
    Tbl4_3 = Tbl4_3,
    Tbl5_1 = Tbl5_1,
    Tbl6_1 = Tbl6_1,
    Tbl6_2 = Tbl6_2,
    Tbl6_3 = Tbl6_3,
    Tbl6_4 = Tbl6_4,
    Tbl6_5 = Tbl6_5,
    Tbl7_1 = Tbl7_1,
    Tbl7_2 = Tbl7_2,
    Tbl7_3 = Tbl7_3,
    Tbl8_1 = Tbl8_1,
    Tbl8_2 = Tbl8_2
  )
  dnt

  ## %######################################################%##
  #                                                          #
  ####                 write Rmd to html                  ####
  #                                                          #
  ## %######################################################%##


  cat("---
title: \"", filename, "\"
author: \"", author, "\"
date: \"\`r Sys.Date()\`\"
output: html_document
---

\`\`\`{r setup, include=FALSE}
knitr::opts_chunk$set(echo = F)
\`\`\`
## Table 1_1
\`\`\`{r, echo=F}
dnt$Tbl1_1
\`\`\`

## Table 1_2
\`\`\`{r, echo=F}
dnt$Tbl1_2
\`\`\`

## Table 1_3
\`\`\`{r, echo=F}
dnt$Tbl1_3
\`\`\`

## Table 2_1
\`\`\`{r, echo=F}
dnt$Tbl2_1
\`\`\`

## Table 2_2
\`\`\`{r, echo=F}
dnt$Tbl2_2
\`\`\`

## Table 3_1
\`\`\`{r, echo=F}
dnt$Tbl3_1
\`\`\`

## Table 4_1
\`\`\`{r, echo=F}
dnt$Tbl4_1
\`\`\`

## Table 4_2
\`\`\`{r, echo=F}
dnt$Tbl4_2
\`\`\`

## Table 4_3
\`\`\`{r, echo=F}
dnt$Tbl4_3
\`\`\`

## Table 5_1
\`\`\`{r, echo=F}
dnt$Tbl5_1
\`\`\`

## Table 6_1
\`\`\`{r, echo=F}
dnt$Tbl6_1
\`\`\`

## Table 6_2
\`\`\`{r, echo=F}
dnt$Tbl6_2
\`\`\`

## Table 6_3
\`\`\`{r, echo=F}
dnt$Tbl6_3
\`\`\`

## Table 6_4
\`\`\`{r, echo=F}
dnt$Tbl6_4
\`\`\`

## Table 6_5
\`\`\`{r, echo=F}
dnt$Tbl6_5
\`\`\`

## Table 7_1
\`\`\`{r, echo=F}
dnt$Tbl7_1
\`\`\`

## Table 7_2
\`\`\`{r, echo=F}
dnt$Tbl7_2
\`\`\`

## Table 7_3
\`\`\`{r, echo=F}
dnt$Tbl7_3
\`\`\`

## Table 8_1
\`\`\`{r, echo=F}
dnt$Tbl8_1
\`\`\`

## Table 8_2
\`\`\`{r, echo=F}
dnt$Tbl8_2
\`\`\`

",
    file = "tmp.Rmd"
  )

  rmarkdown::render("tmp.Rmd", output_file = paste0(filename))
  file.remove("tmp.Rmd") # the Rmd does not work if opened independently, so delete it.
}


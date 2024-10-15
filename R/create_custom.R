#' Create a custom feed ingredient
#'
#' Some users may wish to modify the feed library by adding custom ingredients. This function assumes that new feeds are modified from existing feeds in the default library. If preferred, it is also possible to modify a feed library in Excel or a csv editor rather than in R.
#'
#' @param template_fd_name  The Fd_Name of the template feed to use as a base for modifications.
#' @param new_fd_name The name of the new custom feed.
#' @param modifications A list of modifications to apply to the template feed.
#'
#' @return A dataframe representing the new custom feed with applied modifications.
#' @export
#'
#' @examples
#'
#' create_custom_fd("Corn silage, typical", "My_Custom_CornSilage",
#'  list("Fd_DM" = 36.0, "Fd_CP" = 7.0))
#'
# new_fd_name is outside of modifications because it is REQUIRED.
create_custom_fd <- function(template_fd_name, new_fd_name, modifications) {
  if (missing(new_fd_name)) {
    stop("Must specify a new_fd_name")
  }

  template_fd <- default_library %>%
    dplyr::filter(Fd_Name == template_fd_name)

  modifications <- append(modifications, list("Fd_Name" = new_fd_name))

  new_fd <- template_fd %>%
    replace(., names(modifications), modifications)

  diffs <- dplyr::bind_rows(new_fd, template_fd) %>%
    dplyr::select(.,  dplyr::any_of(names(modifications)), -Fd_Name) %>%
    t() %>%
    data.frame() %>%
    dplyr::rename("Template" = 1, "New" = 2) %>%
    dplyr::mutate(Relative_Diff = ifelse(is.numeric(New), 100*abs(Template - New) / (Template + New / 2), ""))

  message(paste0(capture.output(diffs), collapse = "\n"))

  return(new_fd)
}


#
#' Create a custom feed library
#'
#' Create a custom library by appending custom feeds to the default library. The user can specify whether to use the default_library as the base library, or to specify a different library. To use only custom feeds, the user can also specify "none" as the base library.
#'
#' @param base_library The base library to use as a starting point for the custom library. Default is "default_library". If base_library = "none", the function starts with an empty library.
#' @param custom_fd_list A list of custom feeds created using \code{\link[dairynasem]{create_custom_fd}}.
#'
#' @return A dataframe representing the custom library with appended custom feeds.
#' @export
#'
#' @examples
#'
#' # Create a custom library using the default library as a base and appending custom feeds.
#'
#' Custom_alfalfa_silage <- create_custom_fd("Legume silage, mid-maturity",
#' "Custom_alfalfa_silage",
#' list("Fd_DM" = 45, "Fd_CP" = 28))
#'
#' Custom_corn_silage <- create_custom_fd("Corn silage, typical",
#' "Custom_corn_silage",
#' list("Fd_DM" = 38, "Fd_CP" = 8))
#'
#' my_custom_library <- create_custom_library(base_library = "default_library",
#' list(Custom_alfalfa_silage, Custom_corn_silage))
#'
create_custom_library <- function(base_library = "default_library", custom_fd_list) {
  custom_fd_list <- dplyr::bind_rows(custom_fd_list)
  if (is.data.frame(base_library)) {
    custom_library <- dplyr::bind_rows(base_library, custom_fd_list)
  } else if (base_library == "default_library") {
    custom_library <- dplyr::bind_rows(default_library, custom_fd_list)
  } else if (base_library == "none") {
    custom_library <- custom_fd_list
  }
  else {
    (warning("base_library argument must be a dataframe or the strings 'default_library' or 'none'"))
  }
  if (!is.list(custom_fd_list) | length(custom_fd_list) == 0) {
    warning("custom_fd_list must be a list of 1 or more feeds created with create_custom_fd")
  }
  if (missing(custom_fd_list)) {
    stop("custom_fd_list was empty, so no custom library could be created.")
  }
  return(custom_library)
}


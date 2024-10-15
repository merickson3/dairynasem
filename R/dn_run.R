#' Run the NASEM (2021) model
#'
#' This function runs the NASEM (2021) model. It produces all possible combinations of diets and scenarios to generate a set of simulations, unless a subset of diet_scenario_combos is specified to limit the results.
#'
#' @param diet_comp a dataframe with DietID, Fd_Name, and Fd_Amount_DM
#' @param scenario a dataframe with ScenarioID, and details of the scenario, e.g., An_Breed
#' @param feed_library a dataframe, default is default_library which includes NASEM (2021) values
#' @param eff either the character string "default_eff" or a vector of efficiencies. The default sets efficiencies to the NASEM (2021) values.
#' @param infus either the character string "no_infusions" or a vector of infusion data. The default is no_infusions.
#' @param diet_scenario_combos a character vector specifying which combinations of diet and scenario desired. If none are specified, the function returns all possible combinations of diets and scenarios.
#'
#' @return A list containing the results of Dairy NASEM simulations for each combination of diet and scenario.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'
#' data(diet_comp_ex_1)
#' data(scenario_ex_1)
#' dn_out = dn_run(diet_comp = diet_comp_ex_1, scenario = scenario_ex_1)
#'
dn_run <- function(diet_comp, scenario, feed_library = "default_library",
                   eff = "default_eff", infus = "no_infusions", diet_scenario_combos = NA) {

 # library(plyr)
 # library(dairynasem)
 # library(purrr)
 # inff = no_infusions
 # diet_comp = diet_comp_ex_1
  #scenario = scenario_ex_1
  #eff = "default_eff"
  #infus = inff
# feed_library = "default_library"
 # diet_scenario_combos = c("d1__s1", "d2__s4")

##%######################################################%##
#                                                          #
####                    Check inputs                    ####
#                                                          #
##%######################################################%##
# Check inputs match expected class
checkmate::assert_data_frame(diet_comp,
  any.missing = FALSE,
  min.rows = 1,
  ncols = 3,
  types = c("character", "character", "double"),
  col.names = "named"
)

# Names must match exactly
checkmate::assert_names(colnames(diet_comp), identical.to = c("DietID", "Fd_Name", "Fd_DMInp"))

checkmate::assert_data_frame(scenario,
  any.missing = TRUE,
  min.rows = 1,
  ncols = 38,
  col.names = "named"
)
checkmate::assert_names(colnames(diet_comp), identical.to = c("DietID", "Fd_Name", "Fd_DMInp"))

checkmate::assert(
  checkmate::check_class(feed_library, "data_frame"),
  checkmate::check_class(feed_library, "data.frame"),
  checkmate::check_class(feed_library, "character"),
  combine = "or"
)
checkmate::assert_names(colnames(scenario), identical.to = c(
  "ScenarioID", "An_Breed", "An_AgeDay", "An_BW", "An_BW_mature",
  "Trg_FrmGain", "Trg_RsrvGain", "An_Parity_rl", "An_BCS", "An_GestLength",
  "An_AgeConcept1st", "An_DIMConcept", "Fet_BWbrth", "An_StatePhys",
  "An_LactDay", "An_GestDay", "Env_TempCurr", "Env_DistParlor",
  "Env_TripsParlor", "Env_Topo", "Trg_MilkProd", "Trg_MilkLacp",
  "Trg_MilkTPp", "Trg_MilkFatp", "Trg_Dt_DMIn", "An_AgeDryFdStart",
  "DMIn_eqn", "Use_DNDF_IV", "RUP_eqn", "MiN_eqn", "mProd_eqn",
  "mLac_eqn", "mPrt_eqn", "An_305RHA_MlkTP", "mFat_eqn", "FrmGain_eqn",
  "RsrvGain_eqn", "Monensin_eqn"
))
checkmate::assert(
  checkmate::check_class(eff, "data_frame"),
  checkmate::check_class(eff, "data.frame"),
  checkmate::check_class(eff, "character"),
  combine = "or"
)

if (is.data.frame(eff)) {
  checkmate::assert_names(colnames(eff), must.include = c(
    "Trg_AbsHis_NPHis", "Trg_AbsIle_NPIle", "Trg_AbsLeu_NPLeu",
    "Trg_AbsLys_NPLys", "Trg_AbsMet_NPMet", "Trg_AbsPhe_NPPhe", "Trg_AbsThr_NPThr",
    "Trg_AbsTrp_NPTrp", "Trg_AbsVal_NPVal", "Trg_MP_NP"
  ))
}

checkmate::assert(
  checkmate::check_class(infus, "data_frame"),
  checkmate::check_class(infus, "data.frame"),
  checkmate::check_class(infus, "character"),
  combine = "or"
)

if (is.data.frame(infus)) {
  checkmate::assert_names(colnames(infus), must.include = c(
    "Inf_Acet_g", "Inf_ADF_g", "Inf_Arg_g", "Inf_Ash_g",
    "Inf_Butr_g", "Inf_CP_g", "Inf_CPARum_CP", "Inf_CPBRum_CP", "Inf_CPCRum_CP",
    "Inf_dcFA", "Inf_dcRUP", "Inf_DM_g", "Inf_EE_g", "Inf_FA_g",
    "Inf_Glc_g", "Inf_His_g", "Inf_Ile_g", "Inf_KdCPB", "Inf_Leu_g",
    "Inf_Lys_g", "Inf_Met_g", "Inf_NDF_g", "Inf_NPNCP_g", "Inf_Phe_g",
    "Inf_Prop_g", "Inf_St_g", "Inf_Thr_g", "Inf_Trp_g", "Inf_ttdcSt",
    "Inf_Val_g", "Inf_VFA_g", "Inf_Location"
  ))
}

checkmate::assert(
  checkmate::check_class(diet_scenario_combos, "vector"),
  checkmate::check_class(diet_scenario_combos, "scalar"),
  null.ok = TRUE,
  combine = "or"
)

## %######################################################%##
  #                                                          #
  ####              Format inputs into list               ####
  #                                                          #
  ## %######################################################%##
  # load defaults

  if (feed_library[[1]][[1]] == "default_library") {
    feed_library <- default_library
  } else {
    (feed_library <- as.data.frame(feed_library))
  }
  if (!length(feed_library)==87) {
    stop("The columns in the feed library must match the NASEM (2021) library. Load default library to see an example.")
  }
  #—————————————————————

  if (infus[[1]][[1]] == "no_infusions") {
    infus <- no_infusions
  } else {
    (infus <- as.data.frame(infus))
  }
  if (!length(infus)==34) {
    stop("infus must either be a character vector 'no_infusions', or a numeric vector matching the NASEM (2021) format. See data(no_infusions) to see an example.")
  }
  if (eff[[1]][[1]] == "default_eff") {
    eff <- default_eff
  } else {
    (eff <- as.data.frame(eff))
  }
  if (!length(eff)==11) {
    stop("eff must either be a character vector 'default_eff', or a numeric vector matching the NASEM (2021) format. Run data(default_eff) to see an example.")
  }
  #—————————————————————

  # warning for invalid DMInp input, should sum to 1
  diet_comp_scaled <- diet_comp |>
    dplyr::group_by(DietID) |>
    dplyr::mutate(DMInp_sum = sum(Fd_DMInp)) |>
    dplyr::mutate(Fd_DMInp_scaled = Fd_DMInp / DMInp_sum)

  if (any(diet_comp_scaled$DMInp_sum > 1.001 | diet_comp_scaled$DMInp_sum < .999)) {
    diet_comp$Fd_DMInp <- diet_comp_scaled$Fd_DMInp_scaled
    message("Fd_DMInp was scaled to sum to 1.0 for each diet. Fd_DMInp is entered as the feed's proportional contribution to diet DM.")
  }

##%######################################################%##
#                                                          #
####     Convert to list and join with feed library     ####
#                                                          #
##%######################################################%##

  diet_comp_l <- diet_comp |>
    as.data.frame() |> # must be df to access atomic vectors with $
    split(~DietID) |>
    purrr::map(\(.) dplyr::left_join(., feed_library, by = "Fd_Name"))

  # removed ~ in purrr, then added it in split.
##%######################################################%##
#                                                          #
####             Warn for mismatched feeds              ####
#                                                          #
##%######################################################%##

  total_fds = length(rowSums(data.frame(diet_comp_l[[1]][["Fd_Name"]]  %in%  feed_library$Fd_Name ), na.rm = T))
  matched_fds = sum(rowSums(data.frame(diet_comp_l[[1]][["Fd_Name"]]  %in%  feed_library$Fd_Name ), na.rm = T))
  mismatch_fd <- total_fds-matched_fds
  mismatch_names = diet_comp_l[[1]][!(diet_comp_l[[1]][["Fd_Name"]]  %in%  feed_library$Fd_Name)] # subset where mismatched
  # We may use rowSums as TRUE -> 1 and FALSE -> 0, thus each TRUE element when summed gives the count

  if (mismatch_fd > 0) {
    stop(paste0("Invalid result. n = ", mismatch_fd, " feeds have not been matched between diet_comp and fd_library. Fd_Names in the diet but not the feed library include", (mismatch_names)))
  }

  scenario_l <- scenario |>
    as.data.frame() |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "NA")),
     # dplyr::across(dplyr::where(is.numeric), ~ dplyr::na_if(.x, NA)) unecessary
    ) |>
    suppressWarnings(dplyr::mutate(dplyr::across(-c("ScenarioID", "An_Breed", "An_StatePhys")), as.numeric)) |>
    split(~ScenarioID)

  infus_l <- list(infus)

  eff_l <- list(eff)

  super_l <- list(diet_comp = diet_comp_l, scenario = scenario_l, infus = infus_l, eff = eff_l)

  # Generate all possible combinations of diet and scenario (not allowing multiple combinations of infusion and efficicney. ??)
  # For example, 30 elements expected (10 scenario, 3 diet)


  # Source for cross, modified. Do not need outside of this function.
cr <- function(.l, .filter = NULL) {
  n <- length(.l) # length for main list
  lengths <- lapply(.l, length) # length for sublists
  names <- names(.l) #
  factors <- cumprod(lengths) # cumulative product = product of all lengths , eg try cumprod(1:3)
  total_length <- factors[n] # subset last element of factors, whcih has the total number of possible combos
  factors <- c(1, factors[-n]) # dont' need the last one due to modulo
  out <- replicate(total_length, vector("list", n), simplify = FALSE) # replicate is a wrapper for the common use of sapply for repeated evaluation of an expression (which will usually involve random number generation).
  # create empty lists which will be the same length as the main list
  # vector("list", n) creates an empty list with n elements (e.g., 4 in this case. ) Does this for all possible combos (total_length of 30)
  # start a for loop for each element in out. Inside it, start a for loop for each element in the list length n.
  # the first element of factors is 1. Therefore, the list will start at index = 0.   length(.l[[1]]) = 3 when j = 1. 0/1 %% 3 = 0, + 1, index = 1
  # out = the combination of list at 1, with the index. the out[i][j] is .l[1][1] = diet_comp for i and j = 1. This creates the sublists.
  # The for loop iterates over out, the empty list, and changes it. THe loop itself does not create an object.
  # .l[[j]] = diet comp LIST, scenario LIST, etc
  # First, go through all j within i = 1. THen go to i = 2, which is the second
  # seq_along(out) = 1:30
  for (i in seq_along(out)) {
    for (j in seq_len(n)) {
      index <- floor((i - 1) / factors[j]) %% length(.l[[j]]) + # modulo %% gives remainder to restart next, floor rounds down to integer.
        1
      out[[i]][[j]] <- .l[[j]][[index]]
    }
    names(out[[i]]) <- names
  }
  purrr::compact(out)
}


  dn_in_list = cr(  .l = super_l)

  # for each list element, pluck the first diet and scenario ID. no chance to misassign ..
  dietIDs = dn_in_list |> purrr::map(\(.) purrr::pluck(., 1, "DietID", 1))
  scenarioIDs = dn_in_list |> purrr::map(\(.) purrr::pluck(., 2, "ScenarioID", 1))
  nm = expand.grid(dietIDs, scenarioIDs) |>
    tidyr::unite("name", sep = "__") |>
    unique()

  names(dn_in_list) <- nm$name

  # cross rearranges output least to greatest by first then second column.
  # expand.grid leaves original order for first then second.
  # cannot use .filter argument in cross because trying to filter based on top level of list. ?
  dn_in_list = dn_in_list |>
    (\(.) .[order(names(.))])() # native pipe odd requirements for bracketing. anonymous function

  dn_in_list %>% purrr::map(., ~names(.))

  # Select only certain diet scenario combos as given in arguments
  # commented out 2023.02.19

  if (!is.na(diet_scenario_combos[[1]])) {
    dn_in_list <- dn_in_list |>
      purrr::keep_at(diet_scenario_combos)
  }


  ## %######################################################%##
  #                                                          #
  ####                Run dairynasem model                ####
  #                                                          #
  ## %######################################################%##

dn_in_list %>%
    purrr::map(., ~ dairy_nasem(
      DietID = NA,
      An_Breed = .[["scenario"]]$An_Breed,
      An_AgeDay = .[["scenario"]]$An_AgeDay,
      An_BW = .[["scenario"]]$An_BW,
      An_BW_mature = .[["scenario"]]$An_BW_mature,
      Trg_FrmGain = .[["scenario"]]$Trg_FrmGain,
      Trg_RsrvGain = .[["scenario"]]$Trg_RsrvGain,
      An_Parity_rl = .[["scenario"]]$An_Parity_rl,
      An_BCS = .[["scenario"]]$An_BCS,
      An_GestLength = .[["scenario"]]$An_GestLength,
      An_AgeConcept1st = .[["scenario"]]$An_AgeConcept1st,
      An_DIMConcept = .[["scenario"]]$An_DIMConcept,
      Fet_BWbrth = .[["scenario"]]$Fet_BWbrth,
      An_StatePhys = .[["scenario"]]$An_StatePhys,
      An_LactDay = .[["scenario"]]$An_LactDay,
      An_GestDay = .[["scenario"]]$An_GestDay,
      Env_TempCurr = .[["scenario"]]$Env_TempCurr,
      Env_DistParlor = .[["scenario"]]$Env_DistParlor,
      Env_TripsParlor = .[["scenario"]]$Env_TripsParlor,
      Env_Topo = .[["scenario"]]$Env_Topo,
      Trg_MilkProd = .[["scenario"]]$Trg_MilkProd,
      Trg_MilkLacp = .[["scenario"]]$Trg_MilkLacp,
      Trg_MilkTPp = .[["scenario"]]$Trg_MilkTPp,
      Trg_MilkFatp = .[["scenario"]]$Trg_MilkFatp,
      Trg_Dt_DMIn = .[["scenario"]]$Trg_Dt_DMIn,
      An_AgeDryFdStart = .[["scenario"]]$An_AgeDryFdStart,
      ####
      Fd_DMInp = .[["diet_comp"]]$Fd_DMInp,
      f = .[["diet_comp"]],
      i = .[["infus"]],
      ####
      DMIn_eqn = .[["scenario"]]$DMIn_eqn,
      Use_DNDF_IV = .[["scenario"]]$Use_DNDF_IV,
      RUP_eqn = .[["scenario"]]$RUP_eqn,
      MiN_eqn = .[["scenario"]]$MiN_eqn,
      mProd_eqn = .[["scenario"]]$mProd_eqn,
      mLac_eqn = .[["scenario"]]$mLac_eqn,
      mPrt_eqn = .[["scenario"]]$mPrt_eqn,
      An_305RHA_MlkTP = .[["scenario"]]$An_305RHA_MlkTP,
      mFat_eqn = .[["scenario"]]$mFat_eqn,
      FrmGain_eqn = .[["scenario"]]$FrmGain_eqn,
      RsrvGain_eqn = .[["scenario"]]$RsrvGain_eqn,
      Monensin_eqn = .[["scenario"]]$Monensin_eqn,
      ####
      Eff = .[["eff"]]
    ))
}


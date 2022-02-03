
#' Make an experiment object
#' @param ncage_treatment The number of cages per treatment, in default it equals to the control.
#' @param mice_per_cage_treatment number of mice in each cage in treatment group, in default it equals to the control.
#' @return An object of class "expt"
#' @export
make_expt <- function (ncage_treatment = 4,
                       ncage_control = ncage_treatment,
                       mice_per_cage_treatment = 1,
                       mice_per_cage_control = mice_per_cage_treatment) {
  expt <- list(
    number_of_cages_trt = ncage_treatment,
    number_of_cages_con = ncage_control,
    mice_per_cage_trt = mice_per_cage_treatment,
    mice_per_cage_con = mice_per_cage_control
  )
  expt
}

has_random_effects <- function (expt) {
   expt$mice_per_cage_trt > 1
}


get_samples <- function (expt) {
  tibble::tibble(
    group = c("control", "treatment"),
    ncage = c(expt$number_of_cages_con, expt$number_of_cages_trt)
  ) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(cage = paste(group, "cage", seq.int(ncage))) %>%
  dplyr::group_by(cage) %>%
    dplyr::summarise(subject_id = paste(cage, "subj", seq.int(expt$mice_per_cage_trt))) %>%
    dplyr::mutate(group = sub(" cage.*", "", cage))

}


get_effects <- function (d = 1.2) {
  tibble::tibble(
    group = c("control", "treatment"),
    group_effect = c(0, d))
}

experiment_dat <- function (expt, d = 1.2, p = 0) {
  s <- get_samples(expt)
  eff <- get_effects(d)

  if (has_random_effects(expt)) {
    random_dat <- tibble::tibble(
      cage = unique(s$cage),
      random_effect = rnorm(expt$number_of_cages_con + expt$number_of_cages_trt, 0, sqrt(p))
    )
  }

  else {
    random_dat <- tibble::tibble(cage = unique(s$cage),
                                 random_effect = 0)
  }

  s %>%
    dplyr::left_join(eff, by = "group") %>%
    dplyr::mutate(noise = rnorm(dplyr::n(), 0, sqrt(1-p))) %>%
    dplyr::left_join(random_dat, by = "cage") %>%
    dplyr::mutate(y = group_effect  + noise + random_effect )
}


fit_observation <- function (obs) {
  obs_count <- obs %>%
    dplyr::group_by(group) %>%
    dplyr::count(cage)

  if (max(obs_count$n) > 1) {
    lmer_test <- nlme::lme(y ~ group, random = ~1|cage, data = obs)
    mod <- summary(lmer_test)
    data.frame(term  = rownames(mod$tTable), mod$tTable, row.names = NULL) %>%
      dplyr::filter(!term %in% "(Intercept)")
  }
  else {
    broom::tidy(lm(y ~ group, data = obs)) %>%
      dplyr::filter(!term %in% "(Intercept)")
  }
}


#' Get result table of the fitted model
#' @param expt An object of class "expt"
#' @param d A vector of effect sizes (difference in mean / s.d.)
#' @param p Intraclass correlation coefficient(proportion of variance
#'  explained by cage), between 0 to 1
#' @param nsim Number of simulations per value of d
#' @param seed Random seed
#' @return a tibble with estimated power corresponding to d, p and standard t test power
#' calculated based on the minim number of mice per cage if you have unqual cages.
#' @export

get_power <-
  function (expt,
            d,
            p = 0,
            nsim = 100,
            seed = 2018) {
    if (expt$mice_per_cage_trt > 1 && p == 0) {
      stop(
        "Random effect (p) is expected to be greater than 0\n",
        "when number of mouse in each cage is greater than 1"
      )
    }

    if (expt$mice_per_cage_trt == 1 && p > 0) {
      stop("Random effect(p) should be 0 when\n",
           "there is only one mouse per cage")
    }
    if (expt$mice_per_cage_trt > 1 && p > 1){
      stop(
        "Intraclass correlation coefficient (proportion of\n","
        the variance explained by the grouping structure in the population)\n",
        "should be between 0 to 1"
      )
    }

    #future::plan(future::multicore)

    if (expt$mice_per_cage_trt > 1) {

      d_cross <- tidyr::crossing(d, p, num_sim = seq.int(nsim))
      sim_p <- furrr::future_map2_dfr(d_cross$d, d_cross$p, ~fit_observation(experiment_dat(expt, .x, .y)),
                                      .options = furrr::furrr_options(seed = seed))

      cbind(d_cross, sim_p) %>%
        dplyr::mutate(reject = p.value <= 0.05) %>%
        dplyr::group_by(d, p) %>%
        dplyr::summarise(power = sum(reject) / dplyr::n()) %>%
        dplyr::mutate(t_test_power = pwr::pwr.t.test(n = expt$number_of_cages_trt, d = d)$power) %>%
        dplyr::ungroup()

    }
    else {
      d_cross <- tidyr::crossing(d, num_sim = seq.int(nsim))
      sim_p <- furrr::future_map_dfr(d_cross$d,  ~fit_observation(experiment_dat(expt, .x, p = 0)),
                                     .options = furrr::furrr_options(seed = seed))

      cbind(d_cross, sim_p) %>%
        dplyr::mutate(reject = p.value <= 0.05) %>%
        dplyr::group_by(d) %>%
        dplyr::summarise(power = sum(reject) / dplyr::n()) %>%
        dplyr::mutate(t_test_power = pwr::pwr.t.test(n = expt$number_of_cages_trt, d = d)$power) %>%
        dplyr::ungroup()

    }
  }




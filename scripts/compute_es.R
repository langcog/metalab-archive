library(dplyr)
library(purrr)
library(assertthat)


complete <- function(...) {
  args = list(...)
  !any(unlist(map(args, ~(is.null(.x) || is.na(.x)))))
}



on_failure(`%in%`) <- function(call, env) {
  paste0(deparse(call$x), " is not in ", deparse(call$table))
}

compute_es <- function(participant_design, x_1 = NA, x_2 = NA, x_dif = NA,
                       SD_1 = NA, SD_2 = NA, SD_dif = NA, n_1 = NA, n_2 = NA,
                       t = NA, f = NA, d = NA, d_var = NA, corr = NA, corr_imputed = NA,
                       r = NA) {


  assert_that(participant_design %in% c("between", "within_two", "within_one"))

  d_calc <- NA
  d_var_calc <- NA

  if (participant_design == "between") {
    if (complete(n_1, n_2)) {
      if (complete(x_1, x_2, SD_1, SD_2)) {
        pooled_SD <- sqrt(((n_1 - 1) * SD_1 ^ 2 + (n_2 - 1) * SD_2 ^ 2) / (n_1 + n_2 - 2))
        d_calc <- (x_1 - x_2) / pooled_SD
      } else if (complete(t)) {
        d_calc <- t * sqrt((n_1 + n_2) / (n_1 * n_2))
      } else if (complete(f)) {
        d_calc <- sqrt(f * (n_1 + n_2) / (n_1 * n_2))
      }
      d_var_calc <- ((n_1 + n_2) / (n_1 * n_2)) + (d_calc ^ 2 / 2 * (n_1 + n_2))
    } else if (complete(r)) {
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
    } else if (complete(d, d_var)) {
      d_calc <- d
      d_var_calc <- d_var
    }

  } else if (participant_design == "within_two") {
    if (is.na(corr)) {
      corr <- corr_imputed
    }
    if (complete(n_1)) {
      if (complete(x_1, x_2, SD_1, SD_2)) {
        pooled_SD <- sqrt((SD_1 ^ 2 + SD_2 ^ 2) / 2)
        d_calc <- (x_1 - x_2) / pooled_SD
      } else if (complete(x_1, x_2, SD_dif)) {
        within_SD <- SD_dif / sqrt(2 * (1 - corr))
        d_calc <- (x_1 - x_2) / within_SD
      } else if (complete(x_dif, SD_1, SD_2)) {
        pooled_SD <- sqrt((SD_1 ^ 2 + SD_2 ^ 2) / 2)
        d_calc <- x_dif / pooled_SD
      } else if (complete(x_dif, SD_dif)) {
        wc <- sqrt(2 * (1 - corr))
        d_calc <- (x_dif / SD_dif) * wc
      } else if (complete(t)) {
        wc <- sqrt(2 * (1 - corr))
        d_calc <- (t / sqrt(n_1)) * wc
      } else if (complete(f)) {
        wc <- sqrt(2 * (1 - corr))
        d_calc <- sqrt(f / n_1) * wc
      }
      d_var_calc <- ((1 / n_1) + (d_calc ^ 2 / (2 * n_1))) * 2 * (1 - corr)
    } else  if (complete(r)) {
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
    } else if (complete(d, d_var)) {
      d_calc <- d
      d_var_calc <- d_var
    }

  } else if (participant_design == "within_one") {
    if (complete(n_1)) {
      if (complete(x_1, x_2, SD_1)) {
        d_calc <- (x_1 - x_2) / SD_1
      } else if (complete(t)) {
        d_calc <- t / sqrt(n_1)
      } else if (complete(f)) {
        d_calc <- sqrt(f / n_1)
      }
      d_var_calc <- (1 / n_1) + (d_calc ^ 2 / (2 * n_1)) #TODO: x2 factor
    } else if (complete(r)) {
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
    } else  if (complete(d, d_var)) {
      d_calc <- d
      d_var_calc <- d_var
    }
  }

  g_calc <- d_calc * (1 - 3 / (4 * sum(n_1, n_2, na.rm = TRUE) - 5))

  #TODO: should within have a = 4?
  a <- ifelse(participant_design == "between", ((n_1 + n_2) ^ 2) / (n_1 * n_2), 4)
  r_calc = d_calc / sqrt(d_calc ^ 2 + a)

  #TODO: g_var_calc and r_var_calc
  return(data.frame("d_calc" = d_calc, "d_var_calc" = d_var_calc,
                    "g_calc" = g_calc, "r_calc" = r_calc))

}

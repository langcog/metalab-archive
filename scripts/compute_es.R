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
                       t = NA, f = NA, d = NA, d_var = NA, corr = NA,
                       corr_imputed = NA, r = NA, unique_ID = NA, expt_num = NA,
                       special_cases_measures = NA, contrast_sampa = NA) {


  assert_that(participant_design %in% c("between", "within_two", "within_one"))

  d_calc <- NA
  d_var_calc <- NA

  if (participant_design == "between") {
    if (complete(x_1, x_2, SD_1, SD_2)) {
      pooled_SD <- sqrt(((n_1 - 1) * SD_1 ^ 2 + (n_2 - 1) * SD_2 ^ 2) / (n_1 + n_2 - 2))
      d_calc <- (x_1 - x_2) / pooled_SD
    } else if (complete(t)) {
      d_calc <- t * sqrt((n_1 + n_2) / (n_1 * n_2))
    } else if (complete(f)) {
      d_calc <- sqrt(f * (n_1 + n_2) / (n_1 * n_2))
    }
    if (complete(n_1, n_2, d_calc)) {
      # TODO: is this missing a term?
      d_var_calc <- ((n_1 + n_2) / (n_1 * n_2)) + (d_calc ^ 2 / (2 * (n_1 + n_2)))
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
    if (complete(n_1, d_calc)) {
      d_var_calc <- ((1 / n_1) + (d_calc ^ 2 / (2 * n_1))) * 2 * (1 - corr)
    } else  if (complete(r)) {
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
    } else if (complete(d, d_var)) {
      d_calc <- d
      d_var_calc <- d_var
    }

  } else if (participant_design == "within_one") {
    if (complete(x_1, x_2, SD_1)) {
      d_calc <- (x_1 - x_2) / SD_1
    } else if (complete(t)) {
      d_calc <- t / sqrt(n_1)
    } else if (complete(f)) {
      d_calc <- sqrt(f / n_1)
    }
    if (complete(n_1, d_calc)) {
      d_var_calc <- (1 / n_1) + (d_calc ^ 2 / (2 * n_1)) #TODO: x2 factor
    } else if (complete(r)) {
      d_calc <- 2 * r / sqrt(1 - r ^ 2)
      d_var_calc <- 4 * r_var / ((1 - r ^ 2) ^ 3)
    } else  if (complete(d, d_var)) {
      d_calc <- d
      d_var_calc <- d_var
    }
  }

  # SPECIAL CASES
  if (!complete(d_calc, d_var_calc)) {

    special <- special_cases_measures %>% as.character() %>% strsplit(";") %>%
      unlist() %>% as.numeric()
    pooled_SD <- NA
    if (unique_ID == "Kuhl1982") {
      # there are three groups, two experimental (vowel discrimination and pitch discrimination) and one control (which is actually two control groups collapsed together). Only one exp group (vowel discr) was relevant & entered
      # note that the 2nd row for this study is covered by a t score and does not need the special case calculation
      #pooled_SD <- sqrt((n_1[1] * x_1[1] ^ 2 + n_2[1] * x_2[1] ^ 2 + special[1] * special[2] ^ 2 - ((n_1[1] * x_1[1] + n_2[1] * x_2[1] + special[1] * special[2]) ^ 2 / (n_1[1] + n_2[1] + special[1]))) / (special[4] - 1) / special[5])
      pooled_SD <- sqrt((n_1 * x_1 ^ 2 + n_2 * x_2 ^ 2 + special[1] * special[2] ^ 2 - ((n_1 * x_1 + n_2 * x_2 + special[1] * special[2]) ^ 2 / (n_1 + n_2 + special[1]))) / (special[4] - 1) / special[5])
    }
    if (unique_ID == "Polka1996" && (contrast_sampa == "dy:t-du:t" || contrast_sampa == "du:t-dy:t")) {
      # we estimate the SD from an F that compares the 2 vowel orders within each contrast - it is probably pretty bad because the df are incorrect (there are 40 children in total)
      # for the German contrast, performance was significantly poorer for those infants tested with /u/ as the back- ground or reference vowel compared to infants tested with /y/ as the background vowel 􏰂F 􏰅1,32􏰀􏰋17.606, p 􏰈0.0001􏰁.
      # x_1 = mean(.524, .524) (average for u->y), x_2 = mean(0.824, 0.704) (average for y->u), F = 17.606
      pooled_SD <- sqrt((special[1] * special[2] ^ 2 + special[1] * special[3] ^ 2 - ((special[1] * special[2] + special[1] * special[3]) ^ 2 / (special[1] + special[1]))) / (special[4] - 1) / special[5])
    }
    if (unique_ID == "Polka1996" && (contrast_sampa == "d{t-dEt" || contrast_sampa == "dEt-d{t")) {
      # for the English contrast, performance was poorer for those infants tested with /,/ as the background vowel compared to infants tested with /􏰆/ as the background vowel 􏰂F􏰅(1,32)􏰀 = 19.941, p = 0.0001
      # x_1 = mean(0.492, .428) (average for ae->E), x_2 = mean(0.692, 0.672) (average for E->ae), F = 19.941
      pooled_SD <- sqrt((special[1] * special[2] ^ 2 + special[1] * special[3] ^ 2 - ((special[1] * special[2] + special[1] * special[3]) ^ 2 / (special[1] + special[1]))) / (special[4] - 1) / special[5])
    }
    if (unique_ID == "Swoboda1976") {
      # there are three groups, two experimental and one control
      # the two exp groups are entered as two subsequent rows, and control group is the same for both
      #pooled_SD <- sqrt((n_1[1] * x_1[1] ^ 2 + n_2[1] * x_2[1] ^ 2 + special[1] * special[2] ^ 2 - ((n_1[1] * x_1[1] + n_2[1] * x_2[1] + special[1] * special[2]) ^ 2 / (n_1[1] + n_2[1] + special[1]))) / (special[4] - 1) / special[5])
      pooled_SD <- sqrt((n_1 * x_1 ^ 2 + n_2 * x_2 ^ 2 + special[1] * special[2] ^ 2 - ((n_1 * x_1 + n_2 * x_2 + special[1] * special[2]) ^ 2 / (n_1 + n_2 + special[1]))) / (special[4] - 1) / special[5])
    }
    if (unique_ID == "Grieser1989" && expt_num != "1") {
      # we estimate the SD from the t that compares the 2 conditions in exp2, and then we attribute it to both conditions
      # n_1 = n_2 = 16, x_1 = 64.8, x_2 = 77.5, t = 9.4
      #pooled_SD <- abs(x_1[1] - special[2]) / (special[5] * sqrt((n_1[1] + special[1]) / (n_1[1] * special[1])))
      pooled_SD <- abs(x_1 - special[2]) / (special[5] * sqrt((n_1 + special[1]) / (n_1 * special[1])))
    }
    if (complete(x_1, x_2, pooled_SD)) {
      d_calc <- (x_1 - x_2) / pooled_SD
    }
    if (complete(n_1, n_2, d_calc)) {
      d_var_calc <- ((n_1 + n_2) / (n_1 * n_2)) + (d_calc ^ 2 / (2 * (n_1 + n_2)))
    } else if (complete(n_1, d_calc)) {
      d_var_calc <- (1 / n_1) + (d_calc ^ 2 / (2 * n_1)) #TODO: x2 factor
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

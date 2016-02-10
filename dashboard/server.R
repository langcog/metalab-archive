library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(pwr)
library(metafor)
library(langcog)
font <- "Ubuntu"

input <- list(dataset_name = "Infant directed speech preference",
              moderators = c("response_mode"),
              #mod_response_mode = FALSE, mod_procedure = FALSE, mod_mean_age = FALSE,
              response_mode = "behavior", procedure = NULL, mean_age = NULL,
              N = 16, meta_datasets = c("Phonemic discrimination"))

shinyServer(function(input, output, session) {

  data <- reactive({
    filter(all_data, dataset == input$dataset_name)
  })

  moderators <- reactive({
    input$moderators
    #mod_opts <- c("response_mode", "procedure", "mean_age")
    #mod_opts[c(input$mod_response_mode, input$mod_procedure, input$mod_mean_age)]
  })

#   output$include_procedure <- reactive({
#     length(unique(data()$procedure)) > 1
#   })
#
#   output$include_response_mode <- reactive({
#     length(unique(data()$response_mode)) > 1
#   })

  #outputOptions(output, "include_procedure", suspendWhenHidden = FALSE)
  #outputOptions(output, "include_response_mode", suspendWhenHidden = FALSE)

  model <- reactive({
    if (length(moderators()) == 0) {
      rma(d, vi = d_var, slab = as.character(unique_ID), data = data(), method = "REML")
    } else {
      rma(as.formula(paste("d ~", paste(moderators(), collapse = "+"))),
          vi = d_var, slab = as.character(unique_ID), data = data(), method = "REML")
    }
  })

  output$response_mode <- renderUI({
    selectInput("response_mode", label = h5("Response mode"),
                choices = as.character(unique(data()$response_mode)))
  })

  output$procedure <- renderUI({
    selectInput("procedure", label = h5("Procedure"),
                choices = as.character(unique(data()$procedure)))
  })

  output$mean_age <- renderUI({
    sliderInput("mean_age", label = h5("Mean Age (months)"),
                min = round(min(data()$mean_age) / avg_month),
                max = round(max(data()$mean_age) / avg_month),
                value = mean(round(min(data()$mean_age) / avg_month),
                             round(max(data()$mean_age)) / avg_month))
 })

#   effect_size <- reactive({
#     filtered_data <- data()
#     if ("response_mode" %in% moderators() && !is.null(input$response_mode) && input$response_mode %in% filtered_data$response_mode) {
#       filtered_data <- filter(filtered_data, response_mode == input$response_mode)
#     }
#     if ("procedure" %in% moderators() && !is.null(input$procedure) && input$procedure %in% filtered_data$procedure) {
#       filtered_data <- filter(filtered_data, procedure == input$procedure)
#     }
#     if ("mean_age" %in% moderators() && !is.null(input$mean_age) && findInterval(input$mean_age * avg_month,
#                                                                        range(filtered_data$mean_age))) {
#       model <- rma(d ~ mean_age, vi = d_var, slab = as.character(short_cite),
#                    data = filtered_data, method = "REML")
#       predict(model, newmods = input$mean_age*avg_month)
#     } else {
#       model <- rma(d, vi = d_var, slab = as.character(short_cite),
#                    data = filtered_data, method = "REML")
#       predict(model)
#     }
#   })

#   output$effect_size <- renderText({
#     sprintf("Estimated effect size is %.2f, 95%% confidence interval (%.2f, %.2f).",
#             effect_size()$pred, effect_size()$ci.lb, effect_size()$ci.ub)
#   })
#
#   safe.n <- function(es, sig, pow) {
#     n <- NULL
#     tryCatch({n <- ceiling(pwr.p.test(h = es, sig.level = sig, power = pow)$n)},
#              error = function(e) "effect size too large for reasonable n, setting n to 2")
#     if (is.null(n)) n <- 2
#     return(n)
#   }
#
#   output$power <- renderPlot({
#
#     es <- effect_size()$pred
#     n_target <- safe.n(es, 0.05, 0.9)
#     n_max <- max(n_target, input$N)
#     n_min <- if (n_target > 5) 5 else 1
#     n_step <- if (n_target > 5) 5 else 1
#     ns <- seq(n_min, n_max, n_step)
#
#     pwrs <- data.frame(
#       ns = ns,
#       Experimental = pwr.p.test(h = es, n = ns, sig.level = 0.05)$power,
#       Control = pwr.p.test(h = 0, n = ns, sig.level = 0.05)$power#,
#       #      Interaction = pwr.2p.test(h = es, n = ns, sig.level = .05)$power
#     ) %>%
#       gather(condition, ps, Experimental, Control) #Interaction,
#
#     this.pwr <- data.frame(
#       ns = rep(input$N, 2),
#       ps = c(pwr.p.test(h = es, n = input$N, sig.level = 0.05)$power,
#              pwr.p.test(h = 0, n = input$N, sig.level = 0.05)$power),
#       condition = c("Experimental", "Control")
#     )
#
#     pwrs %>%
#       mutate(condition = paste0(condition, "  ")) %>%
#       ggplot(aes(x = ns, y = ps, colour = condition)) +
#         geom_point() +
#         geom_line() +
#         geom_point(data = this.pwr, colour = "red", size = 6) +
#         geom_hline(yintercept = 0.8, linetype = "dashed") +
#         geom_vline(
#           xintercept = safe.n(es, 0.05, 0.8),#pwr.p.test(h = es, sig.level = 0.05, power = 0.8)$n,
#           linetype = "dashed"
#         ) +
#         #        geom_vline(xintercept=pwr.2p.test(h = es,
#         #                                          sig.level = .05,
#         #                                          power = .8)$n, lty=2) +
#         ylim(c(0,1)) +
#         ylab("Power to reject the null at p < .05\n") +
#         xlab("\nSample size") +
#         scale_colour_solarized(name = "") +
#         theme_bw(base_size = 14) +
#         theme(text = element_text(family = font),
#               legend.position = "top",
#               legend.direction = "horizontal",
#               legend.key = element_blank())
#   }, height = function() {
#     session$clientData$output_power_width * 0.7
#   })
#
  mod_group <- reactive({
    if (length(moderators()) == 0) {
      NULL
    } else if ("response_mode" %in% moderators() & "procedure" %in% moderators()) {
      "response_mode_procedure"
    } else if ("response_mode" %in% moderators()) {
      "response_mode"
    } else if ("procedure" %in% moderators()) {
      "procedure"
    }
  })

  output$studies_box <- renderValueBox({
    valueBox(
      nrow(data()), "Studies", icon = icon("list", lib = "glyphicon"),
      color = "red"
    )
  })

  output$effect_size_box <- renderValueBox({
    valueBox(
      sprintf("%.2f", model()$b[,1][["intrcpt"]]), "Effect Size", icon = icon("record", lib = "glyphicon"),
      color = "red"
    )
  })

  output$effect_size_var_box <- renderValueBox({
    valueBox(
      #     sprintf("Estimated effect size is %.2f, 95%% confidence interval (%.2f, %.2f).",
      #             effect_size()$pred, effect_size()$ci.lb, effect_size()$ci.ub)

      sprintf("%.2f", model()$se[1]), "Effect Size SE", icon = icon("resize-horizontal", lib = "glyphicon"),
      color = "red"
    )
  })

  output$scatter <- renderPlot({
    ggplot(data(), aes_string(x = "mean_age_months", y = "d", colour = mod_group())) +
      geom_point(aes(size = n)) +
      geom_smooth(method = "lm", formula = y ~ log(x)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_colour_solarized(name = "") +
      scale_size_continuous(guide = FALSE) +
      xlab("\nMean Subject Age (Months)") +
      ylab("Effect Size\n") +
      theme_bw(base_size = 14) +
      theme(text = element_text(family = font),
            legend.position = "top",
            legend.key = element_blank(),
            legend.background = element_rect(fill = "transparent"))
  }, height = function() {
    session$clientData$output_scatter_width * 0.7
  })

  output$violin <- renderPlot({
    grp <- mod_group()
    if (is.null(grp)) grp <- "all_mod"
    x_label <- switch(grp,
                      "all_mod" = "",
                      "procedure" = "Procedure",
                      "response_mode" = "Response Mode",
                      "response_mode_procedure" = "Response Mode and Procedure")
    ggplot(data(), aes_string(x = grp, y = "d", colour = grp)) +
      geom_jitter(height = 0) +
      geom_violin() +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey") +
      scale_colour_solarized(name = "") +
      xlab(paste0("\n", x_label)) +
      ylab("Effect Size\n") +
      theme_bw(base_size = 14) +
      theme(text = element_text(family = font),
            legend.position = "top",
            legend.key = element_blank(),
            legend.background = element_rect(fill = "transparent"))
  })

  output$forest <- renderPlot({
    f <- fitted(model())
    p <- predict(model())
    alpha <- .05
    forest_data <- data.frame(effects = as.numeric(model()$yi.f),
                              variances = model()$vi.f) %>%
      mutate(effects.cil = effects - qnorm(alpha/2, lower.tail = FALSE) * sqrt(variances),
             effects.cih = effects + qnorm(alpha/2, lower.tail = FALSE) * sqrt(variances),
             estimate = as.numeric(f),
             unique_ID = names(f),
             estimate.cil = p$ci.lb,
             estimate.cih = p$ci.ub,
             identity = 1) %>%
      left_join(mutate(data(), unique_ID = make.unique(unique_ID))) %>%
      arrange(desc(effects)) %>%
      mutate(unique_ID = factor(unique_ID, levels = unique_ID))

    qplot(unique_ID, effects, ymin = effects.cil, ymax = effects.cih,
          geom = "linerange",
          data = forest_data) +
      geom_point(aes(y = effects, size = n)) +
      geom_pointrange(aes(x = unique_ID,
                    y = estimate,
                    ymin = estimate.cil,
                    ymax = estimate.cih),
                col = "red") +
      coord_flip() +
      scale_size_continuous(guide = FALSE) + #name = "N") +
      scale_colour_manual(values = c("data" = "black", "model" = "red")) +
      xlab("") +
      ylab("Effect Size") +
      theme_bw() +
      theme(text = element_text(family = font))
  }, height = function() {
    session$clientData$output_forest_width * 2
  })


  output$funnel <- renderPlot({
    if (length(moderators()) == 0) {
      d = data.frame(se = sqrt(model()$vi), es = model()$yi)
      center = mean(d$es)
      xlabel = "\nEffect Size"
    } else {
      r =  rstandard(model())
      d = data.frame(se = r$se, es = r$resid)
      center = 0
      xlabel = "\nResidual"
    }

    lower_lim = max(d$se) + .05*max(d$se)
    left_lim = ifelse(center - lower_lim*1.96 < min(d$es), center - lower_lim * 1.96, min(d$es) )
    right_lim = ifelse(center + lower_lim*1.96 > max(d$es), center + lower_lim * 1.96, max(d$es) )
    funnel = data.frame(x = c(center - lower_lim * 1.96, center, center + lower_lim * 1.96),
                        y = c(-lower_lim, 0, -lower_lim))

    ggplot(d, aes(x = es, y = -se)) +
      scale_x_continuous(limits = c(left_lim,right_lim)) +
      scale_y_continuous(expand = c(0, 0), # gets rid of extra space ggplot adds to limits
                         breaks = round(seq(0, -max(d$se), length.out = 5), 2),
                         labels = round(seq(0, max(d$se), length.out = 5), 2)) +
      geom_polygon(data = funnel, aes(x = x, y = y), fill = "white") +
      geom_vline(xintercept = center, linetype = "dotted", color = "black", size = .5) +
      geom_point() +
      xlab(xlabel) +
      ylab("Standard error\n") +
      theme_bw(base_size = 14) +
      theme(text = element_text(family = font),
            panel.background = element_rect(fill = "grey"),
            panel.grid.major =  element_line(colour = "darkgrey", size = 0.2),
            panel.grid.minor =  element_line(colour = "darkgrey", size = 0.5))


  }, height = function() {
    session$clientData$output_funnel_width * 0.7
  })
#
#   output$meta_datasets <- renderUI({
#     selectInput("meta_datasets", label = h4("Datasets"), choices = datasets$name,
#                 selected = datasets$name, multiple = TRUE)
#   })
#
#   output$metameta <- renderPlot({
#     all_data %>%
#       filter(dataset %in% input$meta_datasets) %>%
#       mutate(dataset = paste0(dataset, "  ")) %>%
#       ggplot(aes(x = mean_age_months, y = d, colour = dataset)) +
#         #geom_point(aes(size = n)) +
#         geom_point(alpha = 0.5) +
#         geom_smooth(method = "lm", formula = y ~ log(x), size = 1) +
#         geom_hline(yintercept = 0, linetype = "dotted", color = "grey") +
#         scale_colour_solarized(name = "") +
#         #scale_size_continuous(guide = FALSE) +
#         xlab("\nMean Subject Age (Months)") +
#         ylab("Effect Size\n") +
#         theme_bw(base_size = 14) +
#         theme(text = element_text(family = font),
#               legend.position = "top",
#               legend.direction = "horizontal",
#               legend.key = element_blank(),
#               legend.background = element_rect(fill = "transparent"))
#     }, height = function() {
#       session$clientData$output_metameta_width * 0.8
#     })

})

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(pwr)
library(metafor)
library(langcog)
font <- "Ubuntu"

# input <- list(dataset_name = "Mutual exclusivity",
#               moderators = c("response_mode"))

shinyServer(function(input, output, session) {

  data <- reactive({
    filter(all_data, dataset == input$dataset_name)
  })

  # Meta-analysis model
  model <- reactive({
    if (length(input$moderators) == 0) {
      no_mod_model()
    } else {
      rma(as.formula(paste("d_calc ~", paste(input$moderators, collapse = "+"))),
          vi = d_var_calc, slab = as.character(unique_ID), data = data(), method = "REML")
    }
  })

  # Meta-analysis model without any moderators
  no_mod_model <- reactive({
      rma(d_calc, vi = d_var_calc, slab = as.character(unique_ID),
          data = data(), method = "REML")
  })

  mod_group <- reactive({
    if (length(input$moderators) == 0) {
      NULL
    } else if ("response_mode" %in% input$moderators & "exposure_phase" %in% input$moderators) {
      "response_mode_exposure_phase"
    } else if ("response_mode" %in% input$moderators) {
      "response_mode"
    } else if ("exposure_phase" %in% input$moderators) {
      "exposure_phase"
    }
  })

  output$moderator_input <- renderUI({
    mod_choices <- list("Age" = "mean_age",
                        "Response mode" = "response_mode",
                        "Exposure phase" = "exposure_phase")
    valid_mod_choices <- mod_choices %>% keep(~length(unique(data()[[.x]])) > 1)
    checkboxGroupInput("moderators", label = "Moderators", valid_mod_choices,
                       inline = TRUE)
  })

  output$studies_box <- renderValueBox({
    valueBox(
      nrow(data()), "Studies", icon = icon("list", lib = "glyphicon"),
      color = "red"
    )
  })

  output$effect_size_box <- renderValueBox({
    valueBox(
      sprintf("%.2f", no_mod_model()$b[,1][["intrcpt"]]), "Effect Size",
      icon = icon("record", lib = "glyphicon"),
      color = "red"
    )
  })

  output$effect_size_var_box <- renderValueBox({
    valueBox(
      sprintf("%.2f", no_mod_model()$se[1]), "Effect Size SE",
      icon = icon("resize-horizontal", lib = "glyphicon"),
      color = "red"
    )
  })

  output$scatter <- renderPlot({
    ggplot(data(), aes_string(x = "mean_age_months", y = "d_calc", colour = mod_group())) +
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
    ggplot(data(), aes_string(x = grp, y = "d_calc", colour = grp)) +
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
    if (length(input$moderators) == 0) {
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

})

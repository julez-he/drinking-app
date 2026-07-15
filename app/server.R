# server.R

# This is for defining actual functionality
# The simulation and estimation of IPREDS will be outsurced but called here
# Author: Muenchow

library(shiny)
library(mrgsolve)      # for popPK modeling
library(tidyverse)    # for plotting
library(shinyBS)

# Load the mrgsolve model from model.cpp
mod <- mrgsolve::mread("../model/model.cpp")

# --- Standard drink definitions (single source of truth) ---
STANDARD_DRINKS <- list(
  beer = list(vol_ml = 500, conc_pct = 5.0,  label = "Beer",       unit_note = "330 mL bottle, 5.0% vol."),
  wine = list(vol_ml = 200, conc_pct = 12.0, label = "White Wine", unit_note = "200 mL glass, 12.0% vol."),
  shot = list(vol_ml = 20,  conc_pct = 40.0, label = "Shot",       unit_note = "20 mL shot, 40.0% vol.")
)

# Helper: format summary stats
format_summary <- function(df) {
  if(nrow(df) == 0) return(NULL)
  # Numeric columns summary only (except ID, if present)
  numcols <- sapply(df, is.numeric)
  if(sum(numcols) == 0) return(NULL)
  if ("ID" %in% names(df)){
    numcols["ID"] = FALSE
  }

  if ("ID" %in% names(df)){
    numcols["ID"] = FALSE
  }

  stats <- df[, numcols, drop=FALSE]
  
  
  as_tibble(lapply(stats, function(x) round(mean(x, na.rm=TRUE),3)))
}

# Helper: nice HTML for summary
summary_text <- function(df) {
  s <- format_summary(df)
  if(is.null(s)) return(NULL)
  tags$div(
    style="font-size:1.2em; margin-bottom:8px;",
    tags$b("Key stats:"),
    tags$ul(
      lapply(names(s), function(nm)
        tags$li(tags$b(nm), ": ", prettyNum(s[[nm]], big.mark = "'"))
      )
    )
  )
}

determine_ach_lv <- function(ach_conc) {
  cut(
    ach_conc,
    breaks = c(0, 0.01, 0.1, 0.3, 0.6, 12.3, 13.7, 100),
    labels = c("undosed", "underdosed", "precisely_dosed", "over_dosed", "highest_survived", "highest_observed", "impossible"),
    right = FALSE  # interval is [a, b)
  ) %>% as.character()
}


pick_vehicle_gif_path_and_comment <- function(ach_conc, status){
  thr_data <- read.csv(file.path("www/alcohol_level_thre.csv"))
  
  allowed_vehicle <- thr_data$Name[thr_data$BAC.Limit > ach_conc]
  not_allowed_vehicle <- thr_data$Name[!(thr_data$BAC.Limit > ach_conc)]
  dir_name <- "can_drive"
  pics_names <- list.files(path = paste0("www/",dir_name))
  vehicle_names2pick <- switch(
    status,
    "allowed" = allowed_vehicle,
    "not_allowed" = not_allowed_vehicle,
    stop("Status not supported: ", status)
  )
  
  
  pics_names2use <- pics_names[
    sapply(pics_names, function(name) {
      any(sapply(vehicle_names2pick, function(pattern) {
        grepl(pattern, name, ignore.case = TRUE)
      }))
    })
  ]

  pic_name2use <- sample(pics_names2use,1)
  pic_path2use <- file.path(dir_name,pic_name2use)
  vehicle_name2use <- str_split(pic_name2use,"_")[[1]][1]
  vehicle_selected <- vehicle_names2pick[tolower(vehicle_names2pick) == vehicle_name2use]
  comment2use <- switch(
    status,
    "allowed" = thr_data$Explanation[thr_data$Name == vehicle_selected],
    "not_allowed" = paste0("You are over the BAC limite of ",thr_data$BAC.Limit.perc[thr_data$Name == vehicle_selected]),
    stop("Status not supported: ", status)
  )
  
  comment2use <- paste0(vehicle_selected,": ",comment2use)
  v_c_list <<- list(pic_path2use,comment2use)
  return(v_c_list)
}

pick_lv_gif_path_and_comment <- function(ach_conc){
  
  dir_name <- determine_ach_lv(as.numeric(ach_conc))
  pics_names <- list.files(path = paste0("www/",dir_name))
  pic_name2use <- sample(pics_names,1)
  pic_path2use <- file.path(dir_name,pic_name2use)
  pic_name <- basename(pic_path2use) %>% tools::file_path_sans_ext()
  comment2use <- pic_name
  l_c_list <<- list(pic_path2use, comment2use)
  return(l_c_list)
}


build_dataset <- function(
    input_rate,
    input_amt,
    input_age,
    input_wt,
    rep = 1,
    cp_0 = NA,
    CMP = 1) {
  print(paste("Building dataset with rate:", input_rate, "and amount:", input_amt))
  rate <- input_amt / (input_rate)            # g/h
  tibble(
    ID   = 1:rep,
    TIME = c(0),
    AMT  = c(input_amt),      # dose row, obs row
    RATE = c(rate),     # infusion rate for the dose
    CMT  = c(1),               # depot, central
    EVID = c(1),               # dose, observation
    AGE  = input_age,
    WT   = input_wt,
    CPO = ifelse(is.na(cp_0), 0, cp_0),
  )
}

sim_convert <- function(AMT, RATE, AGE, WT, CPO = 0, CMP = 2, rep = 1) {
  df <- build_dataset(
    input_rate = RATE,
    input_amt  = AMT,
    input_age  = AGE,
    input_wt   = WT,
    rep = rep,
    cp_0 = CPO,
    CMP = CMP
  )
  
  raw_out <- mrgsim_df(mod %>% zero_re(), df, end = 24*60, carry_out = c("AMT", "RATE"))
  
  out <- raw_out %>%
    group_by(ID) %>%
    summarise(
      AMT = max(AMT, na.rm = TRUE) %>% round(2),
      RATE = max(RATE, na.rm = TRUE) %>% round(2),
      C_MAX = (max(CMAX, na.rm = TRUE)) %>% round(2),
      T_MAX = (max(TMAX, na.rm = TRUE)/60) %>% round(2),
      CUM_GZT = (max(CUM_GZT, na.rm = TRUE)/60) %>% round(2),
      HR_ABOVE_003 = round(sum(GT003) / 60, 2),
      HR_ABOVE_005 = round(sum(GT005) / 60, 2),
      HR_ABOVE_010 = round(sum(GT010) / 60, 2),
      HR_ABOVE_035 = round(sum(GT020) / 60, 2),
      HR_ABOVE_050 = round(sum(GT050) / 60, 2),
      HR_ABOVE_100 = round(sum(GT100) / 60, 2),
      HR_ABOVE_123 = round(sum(GT123) / 60, 2),
      HR_ABOVE_137 = round(sum(GT137) / 60, 2),
      BELOW_005 = NA
    )
  
  return(list(sum_out = out, raw_out = raw_out))
}

optimize_sim <- function(
    param,           # e.g. "C_MAX", "CUM_GZT", "MIN_ABOVE_050", ...
    target_value,    # e.g., C_MAx of interest
    age, wt,         # fixed covariates
    init_amt, init_rate,
    lower_amt, upper_amt
) {
  
  # objective: NEGATIVE because optim() MINimizes
  obj_fun <- function(AMT_init) {
    cat("Evaluating AMT =", AMT_init, "\n")
    
    res_list <- sim_convert(AMT = AMT_init,
                            RATE = init_rate,
                            AGE  = age,
                            WT   = wt)
    
    sum_out <- res_list[["sum_out"]]
    CMAX <- sum_out[["C_MAX"]]
    
    cat(" -> CMAX =", CMAX, "\n")
    
    if (is.null(CMAX) || is.na(CMAX)) {
      return(Inf)
    }
    
    return(abs(target_value - CMAX))
  }
  
  
  opt <- optimize(obj_fun, lower = lower_amt, upper = upper_amt)
  
  
  
  # package up results
  obj <- opt$objective
  
  tibble(
    AMT           = opt$minimum,
    RATE         = init_rate,
    !!paste("Target",param) := target_value,
    !!paste("Achieved",param) := target_value + obj,
    !!paste("Delta",param) := obj
  )
}





function(input, output, session) {
  
  # --- Reactives --- #
  scenario_result <- reactiveVal(NULL)
  
  # add beer
  observeEvent(input$add_beer, {
    current_value <- input$beer_slider
    max_value <- 10  
    # increment beer count 
    new_value <- min(current_value + 1, max_value)
    
    updateSliderInput(session, "beer_slider", value = new_value)
  })
  

  # add white wine 
  observeEvent(input$add_white_wine, {
    current_value <- input$white_wine_slider
    max_value <- 10
    new_value <- min(current_value + 1, max_value)
    updateSliderInput(session, "white_wine_slider", value = new_value)
  })
  
  # add shot
  observeEvent(input$add_shot, {
    current_value <- input$shot_slider
    max_value <- 10
    new_value <- min(current_value + 1, max_value)
    updateSliderInput(session, "shot_slider", value = new_value)
  })
  
  
  # --- Reactive text field for drink volumes --- #

  #beer volume
  observeEvent(input$beer_slider, {

    selected_volume <- (as.numeric(input$beer_slider))*0.5

    formatted_volume_string <- format(
      selected_volume,
      decimal.mark = ",",
      nsmall = 2
    )

    updateNumericInput(session, "beer_vol", value = formatted_volume_string)
  })

  #wine volume
  observeEvent(input$white_wine_slider, {

    selected_volume <- (as.numeric(input$white_wine_slider))*0.2

    formatted_volume_string <- format(
      selected_volume,
      decimal.mark = ",",
      big.mark = ".",
      nsmall = 2
    )

    updateNumericInput(session, "white_wine_vol", value = formatted_volume_string)
  })

  #shot volume
  observeEvent(input$shot_slider, {

    selected_volume <- (as.numeric(input$shot_slider))*20

    formatted_volume_string <- format(
      selected_volume,
      decimal.mark = ",",
      nsmall = 2
    )
    updateNumericInput(session, "shot_vol", value = formatted_volume_string)
  })


  # --- Reset all drink sliders to zero ---
  observeEvent(input$reset_drinks_btn, {
    updateSliderInput(session, "beer_slider", value = 0)
    updateSliderInput(session, "white_wine_slider", value = 0)
    updateSliderInput(session, "shot_slider", value = 0)
  })
  
  
#--- Total ethanol amount reactive ---

   total_ethanol_g <- reactive({
     # Volumes per drink (per slider unit)
     beer_vol_per_unit <- as.numeric(gsub(",", ".", input$beer_vol))   # liters per beer
     wine_vol_per_unit <- as.numeric(gsub(",", ".", input$white_wine_vol))  # liters per wine
     shot_vol_per_unit <- as.numeric(gsub(",", ".", input$shot_vol))     # ml per shot


     # Default concentrations (in % v/v), adjust if you allow custom conc input!
     beer_conc <- 5 #as.numeric(gsub(",", ".", input$beer_conc))   # e.g., "5,0" to 5.0
     wine_conc <- 12.0 #as.numeric(gsub(",", ".", input$white_wine_conc)) # e.g., "12,0"
     shot_conc <- 40.0 #as.numeric(gsub(",", ".", input$shot_conc))   # e.g., "40,0"

     # Convert slider values to total volumes
     beer_total_vol_ml <- input$beer_slider * beer_vol_per_unit * 1000      # mL
     wine_total_vol_ml <- input$white_wine_slider * wine_vol_per_unit * 1000 # mL
     shot_total_vol_ml <- input$shot_slider * shot_vol_per_unit             # mL

     # Ethanol volume in mL = total volume * (ethanol % / 100)
     ethanol_ml <-
       (beer_total_vol_ml * (beer_conc / 100)) +
       (wine_total_vol_ml * (wine_conc / 100)) +
       (shot_total_vol_ml * (shot_conc / 100))

     # Ethanol density = 0.789 g/mL
     ethanol_g <- ethanol_ml * 0.789

     return(ethanol_g)
   })

  # add a list to store and inspect variables outside shinyapp
  shared <<- new.env()
  observe({
    shared$input <- reactiveValuesToList(input)
  })
  
  # --- A Priori Prediction ---
  aprioriData <- reactive({
    
    # build dosing event
    ev <- build_dataset(
      input_rate = 1,  # rate is not used in this case
      input_amt = total_ethanol_g(),  # total ethanol amount in grams
      input_age = as.numeric(input$ap_age),
      input_wt = as.numeric(input$ap_weight),
      rep = 1
    )
    
    # simulate with covariate
    sim <- mrgsolve::mrgsim_df(mod %>% zero_re(), ev, end = 24 * 60)
    
    
    out <- tibble(
      time = sim$TIME,
      cp  = sim$CP
    )
    out
  })
  
  # --- Time to drive (legal BAC crossing) ---
  drive_status <- reactive({
    df <- aprioriData()
    legal_limit <- 0.5  # ‰, general German driving limit
    
    peak_cp <- max(df$cp, na.rm = TRUE)
    
    if (peak_cp < legal_limit) {
      return(list(status = "clear", hours = 0))
    }
    
    above_idx  <- which(df$cp >= legal_limit)
    last_above <- max(above_idx)
    
    if (last_above >= nrow(df)) {
      # still above the limit at the edge of the simulated window
      return(list(status = "still_high", hours = NA))
    }
    
    x1 <- df$time[last_above]     / 60; y1 <- df$cp[last_above]
    x2 <- df$time[last_above + 1] / 60; y2 <- df$cp[last_above + 1]
    slope <- (y2 - y1) / (x2 - x1)
    time_cross_h <- x1 + (legal_limit - y1) / slope
    
    list(status = "waiting", hours = time_cross_h)
  })
  
  output$drive_status_box <- renderUI({
    ds <- drive_status()
    
    if (ds$status == "clear") {
      bg <- "#DFF0D8"; fg <- "#3c763d"
      headline <- "Already under 0.5‰ — model predicts no waiting time"
    } else if (ds$status == "still_high") {
      bg <- "#F2DEDE"; fg <- "#a94442"
      headline <- "Still above 0.5‰ at the end of the simulated window — extend the timeframe slider"
    } else {
      hrs  <- floor(ds$hours)
      mins <- round((ds$hours - hrs) * 60)
      bg <- "#FCF8E3"; fg <- "#8a6d3b"
      headline <- sprintf("🚫 Not safe to drive — about %dh %02dm remaining", hrs, mins)
    }
    
    div(
      style = paste0(
        "background:", bg, "; color:", fg, "; border-radius:10px; ",
        "padding:14px 20px; margin-bottom:10px; font-size:1.7em; ",
        "font-weight:700; text-align:center;"
      ),
      headline,
      tags$div(
        style = "font-size:0.45em; font-weight:400; margin-top:4px;",
        "Estimate only — not a legal or medical determination. When in doubt, don't drive."
      )
    )
  })
  
  output$aprioriPlot <- renderPlot({
    df <- aprioriData()
    ds <- drive_status()  # from the reactive added earlier
    
    # --- Base plot ---
    p <- ggplot(df, aes(x = time / 60, y = cp)) +
      geom_line(linewidth = 1.3, color = "steelblue4") +
      labs(
        x = "Time since dose (h)",
        y = "Blood ethanol (‰)",
        title = "Predicted breath ethanol curve"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      ) +
      coord_cartesian(xlim = c(0, input$ap_time), ylim = c(0, NA))
    
    maxcp <- max(df$cp, na.rm = TRUE)
    
    # --- Reference threshold lines (drawn only if curve actually reaches them) ---
    thresholds <- tibble::tribble(
      ~value, ~label,     ~color,
      0.3,    "0.3 ‰",    "seagreen",
      0.5,    "0.5 ‰ (driving limit)", "darkorange",
      1.0,    "1.0 ‰",    "firebrick",
      2.5,    "2.5 ‰",    "firebrick4",
      5.0,    "5.0 ‰",    "black"
    )
    
    for (i in seq_len(nrow(thresholds))) {
      th <- thresholds[i, ]
      if (maxcp > th$value) {
        is_limit <- th$value == 0.5
        p <- p +
          geom_hline(
            yintercept = th$value,
            linetype = if (is_limit) "solid" else "dashed",
            color = th$color,
            linewidth = if (is_limit) 1 else 0.6
          ) +
          annotate(
            "text",
            x = input$ap_time * 0.82,
            y = th$value + 0.05 * max(maxcp, 1),
            label = th$label,
            color = th$color,
            size = if (is_limit) 5 else 4,
            fontface = "bold"
          )
      }
    }
    
    # --- "Time to drive" crossing marker (uses the shared drive_status reactive) ---
    if (ds$status == "waiting") {
      p <- p +
        geom_vline(xintercept = ds$hours, linetype = "dotted",
                   color = "darkorange", linewidth = 1.1) +
        annotate(
          "point", x = ds$hours, y = 0.5, color = "darkorange", size = 3
        ) +
        annotate(
          "label",
          x = ds$hours, y = max(maxcp, 1) * 0.9,
          label = sprintf("Can drive at %.1f h", ds$hours),
          color = "white", fill = "darkorange", fontface = "bold", size = 4.5
        )
    }
    
    # --- Measured point (field breathalyzer reading) ---
    if (!(is.na(input$measured_time) || is.na(input$measured_conc))) {
      measured_time <- as.numeric(input$measured_time) / 60
      measured_conc <- as.numeric(gsub(",", ".", input$measured_conc))
      p <- p +
        annotate("point", x = measured_time, y = measured_conc,
                 color = "red", size = 3.5, shape = 18) +
        annotate("text", x = measured_time, y = measured_conc + 0.08 * max(maxcp, 1),
                 label = paste0("Measured: ", input$measured_conc, " ‰"),
                 color = "red", size = 4, fontface = "bold")
    }
    
    p
  })
  
  
  # output$aprioriPlot <- renderPlot({
  #   df <- aprioriData()
  #   p <- ggplot(df, aes(x = time/60, y = cp)) +
  #     geom_line(linewidth = 1.2, color = "blue") +
  #     labs(
  #       x = "Time since dose (h)",
  #       y = "Blood ethanol (g/L ≈ ‰)"
  #     ) +
  #     theme_minimal(base_size = 14) +
  #     coord_cartesian(xlim = c(0, input$ap_time), ylim = c(0.0, NA))
  #   
  #   maxcp <- max(df$cp, na.rm = TRUE)
  #   
  #   if (maxcp > 0.3) {
  #     p <- p +
  #       geom_hline(yintercept = 0.3, linetype = "dashed", color = "green4") +
  #       annotate("text", x = input$ap_time * 0.8, y = 0.35, label = "0.3 g/L", color = "green4", size = 5, fontface = "bold")
  #   }
  #   if (maxcp > 0.5) {
  #     p <- p +
  #       geom_hline(yintercept = 0.5, linetype = "dashed", color = "orange") +
  #       annotate("text", x = input$ap_time * 0.8, y = 0.55, label = "0.5 g/L", color = "orange", size = 5, fontface = "bold")
  #   }
  #   if (maxcp > 1.0) {
  #     p <- p +
  #       geom_hline(yintercept = 1.0, linetype = "dashed", color = "red") +
  #       annotate("text", x = input$ap_time * 0.8, y = 1.05, label = "1.0 g/L", color = "red", size = 5, fontface = "bold")
  #   }
  #   if (maxcp > 2.5) {
  #     p <- p +
  #       geom_hline(yintercept = 2.5, linetype = "dashed", color = "red") +
  #       annotate("text", x = input$ap_time * 0.8, y = 2.55, label = "2.5 g/L", color = "red", size = 5, fontface = "bold")
  #   }
  #   if (maxcp > 5) {
  #     p <- p +
  #       geom_hline(yintercept = 5, linetype = "dashed", color = "black") +
  #       annotate("text", x = input$ap_time * 0.8, y = 5, label = "5 g/L", color = "black", size = 5, fontface = "bold")
  #   }
  #   
  #   # Find intersection with y=0.5 (descending limb)
  #   idx <- which(df$cp >= 0.5)
  #   if (length(idx) >= 2) {
  #     # Find the second crossing (descending limb)
  #     idx2 <- idx[length(idx)]
  #     if (idx2 < nrow(df)) {
  #       x1 <- df$time[idx2-1]/60; y1 <- df$cp[idx2-1]
  #       x2 <- df$time[idx2]/60;   y2 <- df$cp[idx2]
  #       # Linear interpolation for crossing
  #       slope <- (y2 - y1) / (x2 - x1)-
  #       time_cross <- x1 + (0.5 - y1) / slope
  #       # Add vline and annotation
  #       p <- p +
  #         geom_vline(xintercept = time_cross, linetype = "dotted", color = "orange", linewidth = 1) +
  #         annotate("text", x = time_cross + 0.3, y = 0.3, label = sprintf("Second crossing: %.2f h", time_cross),
  #                  color = "orange", fontface = "bold", vjust = -0.5, hjust = 0.1)
  #     }
  #   }
  #   
  #   if (!(is.na(input$measured_time) || is.na(input$measured_conc))) {
  #     measured_time <- as.numeric(input$measured_time) / 60  # convert to hours
  #     measured_conc <- as.numeric(gsub(",", ".", input$measured_conc))  # convert to numeric
  #     p <- p +
  #       annotate("point", x = measured_time, y = measured_conc, color = "red", size = 3) +
  #       annotate("text", x = measured_time, y = measured_conc + 0.1 * measured_conc,
  #                label = paste0("Measured: ", measured_conc, " g/L"), color = "red", size = 4)
  #   }
  #   
  #   p
  # })
  
  
  
  output$measuredValueInfo <- renderUI({
    req(input$po_time1, input$po_conc1)
    if (!is.na(input$po_time1) && !is.na(input$po_conc1)) {
      div(
        style = "margin-top: 1em; color: #444; font-size: 1.1em;",
        paste0("Measured: ", input$po_conc1, " g/dL at ", round(input$po_time1/60,1), " hours after dose")
      )
    }
  })
  
  # Reactives triggered by button
  l_c_list <- eventReactive(input$redraw_btn, {
    level <- as.numeric(gsub(",", ".", input$measured_conc))
    pick_lv_gif_path_and_comment(level)
  })
  
  v_c_list_allowed <- eventReactive(input$redraw_btn, {
    level <- as.numeric(gsub(",", ".", input$measured_conc))
    pick_vehicle_gif_path_and_comment(level, status = "allowed")
  })
  
  v_c_list_not_allowed <- eventReactive(input$redraw_btn, {
    level <- as.numeric(gsub(",", ".", input$measured_conc))
    pick_vehicle_gif_path_and_comment(level, status = "not_allowed")
  })
  
  # UI rendering
  output$gif_panel <- renderUI({
    fluidRow(
      column(
        width = 4,
        box(
          title = "Where is my alcohol level?",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          div(
            style = "text-align:center;",
            tags$img(src = l_c_list()[[1]], height = "200px"),
            tags$p(l_c_list()[[2]])
          )
        )
      ),
      column(
        width = 4,
        box(
          title = "What can I drive home?",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          div(
            style = "text-align:center;",
            tags$img(src = v_c_list_allowed()[[1]], height = "200px"),
            tags$p(v_c_list_allowed()[[2]])
          )
        )
      ),
      column(
        width = 4,
        box(
          title = "What can't I drive home?",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          div(
            style = "text-align:center;",
            tags$img(src = v_c_list_not_allowed()[[1]], height = "200px"),
            tags$p(v_c_list_not_allowed()[[2]])
          )
        )
      )
    )
  })
  
  
  # --- Scenario simulation and output rendering ---
  
  # ui component rendering
  
  output$aposthoc_body <- renderUI({
    tabItem(
      tabName = "aposthoc",
      fluidRow(
        box(
          title = "What-if Scenarios", status = "primary", solidHeader = TRUE, width = 4,
          selectInput(
            "scenario", "Choose a scenario:",
            choices = c(
              "One more beer"         = "one_more_beer",
              "Hit 1.0% peak"         = "hit_1p0",
              "20 kg lighter"         = "lighter",
              "20 years older"        = "older",
              "Chug vs. sip"          = "chug"
            )
          ),
          
          ## ── SCENARIO-SPECIFIC INPUTS ────────────────────────────────────
          conditionalPanel(
            condition = "input.scenario == 'one_more_beer'",
            sliderInput("n_extra_drinks", "Extra beers:", value = 1, min = 1, max = 10),
            sliderInput("drinks_per_hour", "Beers per hour (bph)", value = 3, min = 1, max = 10),
            radioButtons(
              "extra_bev_types", "Extra drinks:",
              choices = c("Beer" = "beer", "White Wine" = "wine", "Shot" = "shot"),
              selected = c("beer")
            ),
          ),
          
          conditionalPanel(
            condition = "input.scenario == 'hit_1p0'",
            numericInput("target_peak", "Target peak (‰):", value = 1.0, min = 0, step = 0.1)
          ),
          
          
          conditionalPanel(
            condition = "input.scenario == 'lighter'",
            numericInput("lose_kg", "Lose weight (kg):", value = 20, min = 0, step = 1)
          ),
          
          conditionalPanel(
            condition = "input.scenario == 'older'",
            numericInput("add_years", "Add years:", value = 20, min = 0, step = 1)
          ),
          
          conditionalPanel(
            condition = "input.scenario == 'chug'",
            sliderInput("n_beers_sip_chug", "Number of beers:", min = 1, max = 10, value = 2),
            sliderInput("sipping_speed", "Sipping speed (beers per hour):", min = 1, max = 4, value = 2),
            h5("Note: chugging assumes a speed of 1 beer/min"),
            br()
          ),
          
          ## ────────────────────────────────────────────────────────────────
          
          # the always-visible inputs:
          # fluidRow(
          #   column(6, numericInput("ap_age",    "Age (years)", value = 40, min = 0)),
          #   column(6, numericInput("ap_weight", "Weight (kg)",  value = 80, min = 0))
          # ),
          actionButton("run_scenario", "Run scenario", icon = icon("play"), class = "btn-block")
        ),
        
        uiOutput("scenario_ui")
        
      )
    )
    
  })
  
  output$scenario_ui <- renderUI({
    val <- scenario_result()
    if (is.null(val)) return(NULL)
    
    box(
      title = "Results", status = "info", solidHeader = TRUE, width = 8,
      conditionalPanel(
        condition = "input.scenario != chug'",
        fluidRow(
          valueBoxOutput("vb_amt",   width = 3),
          valueBoxOutput("vb_cmax",  width = 3),
          valueBoxOutput("vb_tmax",  width = 3),
          valueBoxOutput("vb_gzt",   width = 3)
        )
      ),
      tabBox(
        width = 12,
        tabPanel("Profile",    plotOutput("scenario_plot", height = "320px")),
        tabPanel("Table",      DT::dataTableOutput("scenario_tbl")),
        tabPanel("Thresholds", plotOutput("scenario_bars", height = "250px")),
        tabPanel("Details",    uiOutput("scenario_text"))
      )
    )
  })
  
  
  output$vb_amt <- renderValueBox({
    val <- scenario_result()
    if (is.null(val)) return(NULL)
    # Format values to 2 sig figs, remove trailing zeroes
    val1 <- formatC(val$AMT[1], digits = 2, format = "f")
    label <- val1
    valueBox(
      label,
      "Dose (g)",
      icon = icon("wine-glass"),
      color = "light-blue"
    )
  })
  
  output$vb_cmax <- renderValueBox({
    val <- scenario_result()
    if (is.null(val)) return(NULL)
    val1 <- formatC(val$C_MAX[1], digits = 2, format = "f")
    label <- val1
    if (length(val$C_MAX) > 1) {
      val2 <- formatC(val$C_MAX[2], digits = 2, format = "f")
      label <- paste(val1, "|", val2)
    }
    valueBox(
      label,
      "Peak Conc. (‰)",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$vb_tmax <- renderValueBox({
    val <- scenario_result()
    if (is.null(val)) return(NULL)
    val1 <- formatC(val$T_MAX[1], digits = 2, format = "f")
    label <- val1
    if (length(val$T_MAX) > 1) {
      val2 <- formatC(val$T_MAX[2], digits = 2, format = "f")
      label <- paste(val1, "|", val2)
    }
    valueBox(
      label,
      "Time to Peak (h)",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  output$vb_gzt <- renderValueBox({
    val <- scenario_result()
    if (is.null(val)) return(NULL)
    val1 <- formatC(val$CUM_GZT[1], digits = 2, format = "f")
    label <- val1
    if (length(val$CUM_GZT) > 1) {
      val2 <- formatC(val$CUM_GZT[2], digits = 2, format = "f")
      label <- paste(val1, "|", val2)
    }
    valueBox(
      label,
      "Golden-Zone Time (h)",
      icon = icon("hourglass-half"),
      color = "orange"
    )
  })
  observeEvent(input$run_scenario, {
    # Pack inputs
    AGE  <- input$ap_age
    WT   <- input$ap_weight
    print(WT)
    
    # Dispatch scenarios
    result_list <- switch(input$scenario,
                     
                     one_more_beer = {
                       beer_amt_g  <- 10 * input$n_extra_drinks
                       wine_amt_g  <- 20 * input$n_extra_drinks
                       shot_amt_g  <- 8 * input$n_extra_drinks
                       extra_etoh <- switch(
                         input$extra_bev_types,
                         beer = beer_amt_g,
                         wine = wine_amt_g,
                         shot = shot_amt_g
                       )
                       duration_h  <- input$n_extra_drinks / input$drinks_per_hour
                       RATE        <- duration_h       # hours of infusion
                       sim_convert(
                         AMT  = extra_etoh,
                         RATE = RATE*60,
                         AGE  = AGE,
                         WT   = WT,
                         CPO  = input$measured_conc
                       )
                     },
                     
                     hit_1p0 = {
                       
                       opt_out <- optimize_sim(
                         param       = "C_MAX",
                         target_value = input$target_peak,
                         age         = AGE,
                         wt          = WT,
                         init_amt    = 40,
                         init_rate   = 60,
                         lower_amt   = 0,   upper_amt  = 9999
                       )
                       res_list <- sim_convert(AMT  = opt_out$AMT,
                                               RATE = 60,
                                               AGE  = AGE,
                                               WT   = WT)
                       
                       raw_out <- res_list[["raw_out"]]
                       sum_out <- res_list[["sum_out"]]
                       
                       list(sum_out = sum_out,
                            raw_out = raw_out)
                     },
                     
                     lighter = {
                       out <- sim_convert(AMT  = total_ethanol_g(),
                                   RATE = 60,
                                   AGE  = AGE,
                                   WT   = c(WT, WT - input$lose_kg),
                                   rep = 2)
                       out
                     },
                     
                     older = {
                       sim_convert(AMT  = total_ethanol_g(),
                                   RATE = 60,
                                   AGE  = c(AGE, AGE + input$add_years),
                                   WT   = WT, 
                                   rep = 2)
                     },
          
                     chug = {
                       # compare RATE fast vs. slow
                       # normal RATE = 60 g/hr (6 beer/hr)
                       RATE_sipping <- 60/input$sipping_speed 
                       RATE_chugging <- 60/6 # 1 beer/min (10g/min)
                       
                       
                       sim_convert(
                         AMT  = input$n_beers_sip_chug*10, # g of etoh consumed
                         RATE = c(RATE_sipping, RATE_chugging),
                         AGE  = AGE,
                         WT   = WT, 
                         rep = 2) 
                     }
                     
    )
    
 
    
    if (is.list(result_list) && !is.data.frame(result_list)){
      result <- result_list[["sum_out"]]
    } else {
      result <- result_list
    }
 
    scenario_result(result)


    output$scenario_summary <- renderUI({
      summary_text(result)
    })
    
    output$scenario_tbl <- DT::renderDataTable({
      # start from the raw result
      df <- result
      
      # rename for display
      display <- df %>%
        rename(
          "Dose (g)"               = AMT,
          "Rate (g/h)"             = RATE,
          "Peak Conc. (‰)"         = C_MAX,
          "Time to Peak (h)"       = T_MAX,
          "Golden Zone Time (h)"   = CUM_GZT,
          "hs where %. >0.3‰"             = HR_ABOVE_003,
          "hs where %.  >0.5‰"             = HR_ABOVE_005,
          "hs where %.  >1.0‰"             = HR_ABOVE_010,
          "hs where %.  >3.5‰"             = HR_ABOVE_035,
          "hs where %.  >5.0‰"             = HR_ABOVE_050,
          "hs where %.  >10.0‰"             = HR_ABOVE_100,
          "hs where %.  >12.3‰"             = HR_ABOVE_123,
          "hs where %.  >13.7‰"             = HR_ABOVE_137
        )
      
      DT::datatable(
        display,
        rownames = FALSE,
        class = "stripe hover compact",
        options = list(
          pageLength    = 5,
          scrollX       = TRUE,
          autoWidth     = TRUE,
          dom           = 'Bfrtip',
          buttons       = c('copy', 'csv', 'excel'),
          columnDefs    = list(list(className = 'dt-center', targets = "_all"))
        )
      ) %>%
        DT::formatRound(
          columns = names(display)[sapply(display, is.numeric)],
          digits  = 2
        )
    })
    
    output$scenario_plot <- renderPlot({
      sim_out <- result_list[["raw_out"]]
      validate(
        need("TIME" %in% names(sim_out), "'TIME' column is missing."),
        need("CP"   %in% names(sim_out), "'CP' column is missing.")
      )
      
      # Add 'group' variable based on scenario, if needed
      group_labels <- NULL
      has_group <- FALSE
      if ("lighter" %in% input$scenario && "ID" %in% names(sim_out) && length(unique(sim_out$WT)) == 2) {
        wt_delta <- abs(diff(range(sim_out$WT)))
        sim_out$group <- factor(sim_out$ID, levels = c(1,2), labels = c("Base", paste0(wt_delta, " kg lighter")))
        group_labels <- c("Base", paste0(wt_delta, " kg lighter"))
        has_group <- TRUE
      } else if ("older" %in% input$scenario && "ID" %in% names(sim_out)) {
        sim_out$group <- factor(sim_out$ID, levels = c(1,2), labels = c("Base", paste0(max(sim_out$AGE)-min(sim_out$AGE),"y older")))
        group_labels <- c("Base", paste0(max(sim_out$AGE)-min(sim_out$AGE),"y older"))
        has_group <- TRUE
      } else if ("chug" %in% input$scenario && "ID" %in% names(sim_out)) {
        sim_out$group <- factor(sim_out$ID, levels = c(1,2), labels = c("Sipping", "Chugging"))
        group_labels <- c("Sipping", "Chugging")
        has_group <- TRUE
      }
      
      # Build base plot
      if (has_group) {
        p <- ggplot(sim_out, aes(x = TIME/60, y = CP, color = group))
      } else {
        p <- ggplot(sim_out, aes(x = TIME/60, y = CP))
      }
      
      p <- p +
        geom_line(size = 1.5) +
        geom_rect(
          inherit.aes = FALSE,
          xmin = 0, xmax = max(sim_out$TIME)/60,
          ymin = 0.2, ymax = 0.6,
          fill = "#DFF0D8", alpha = 0.1
        ) +
        scale_x_continuous(
          breaks = seq(0, max(sim_out$TIME)/60, by = 2),
          expand = expansion(add = c(0, 0.5))
        ) +
        labs(
          title    = "Blood Ethanol over Time",
          subtitle = paste0("Scenario: ", input$scenario),
          x        = "Time since start (hours)",
          y        = "Concentration (‰)",
          color    = "Scenario"
        ) +
        theme_bw(base_size = 14) +
        theme(
          plot.title       = element_text(face = "bold", size = 16),
          plot.subtitle    = element_text(size = 12, color = "gray40"),
          axis.title       = element_text(face = "bold"),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          legend.position  = if (has_group) "right" else "none",
          legend.title     = element_text(face = "bold")
        )
      
      # Color palette if group
      if (has_group) {
        p <- p + scale_color_manual(values = c("#0072B2", "#E69F00"), labels = group_labels)
      }
      
      print(p)
    })
    
    
    
    
    # 4. THRESHOLD BARPLOTS (if available)
    output$scenario_bars <- renderPlot({
      gtcols <- names(result)[grepl("^MIN_ABOVE_", names(result))]
      if(length(gtcols)==0) return(NULL)
      df <- result[1,gtcols,drop=FALSE]
      # Get numeric thresholds from names
      thresh <- as.numeric(gsub("MIN_ABOVE_0*", "0.", gsub("MIN_ABOVE_","",gtcols)))
      bar_df <- data.frame(
        Threshold = thresh,
        Minutes   = as.numeric(df[1,])
      )
      ggplot(bar_df, aes(x=factor(Threshold), y=Minutes, fill=Threshold)) +
        geom_col(width=0.7, show.legend=FALSE) +
        scale_fill_gradient(low="#B5EAD7", high="#F08A5D") +
        labs(
          x = "Threshold (‰)",
          y = "Minutes above",
          title = "Time Above Selected Thresholds"
        ) +
        theme_minimal(base_size=13)
    })
    
    # 5. CLEAN TEXT BLOCK (first row)
    output$scenario_text <- renderUI({
      # Only show for simple, non-timecourse, non-category scenarios
      if("msg" %in% names(result)) return(tags$div(style="font-size:1.1em;", result$msg[1]))
      if(nrow(result)==1 && !("TIME" %in% names(result))) {
        tags$div(
          style="font-size:1.15em;",
          tags$b("Scenario result:"),
          tags$ul(
            lapply(names(result), function(nm)
              tags$li(tags$b(nm), ": ", prettyNum(result[[nm]][1], big.mark = "'"))
            )
          )
        )
      }
      
    })
  })
  
  
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

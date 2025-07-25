# Define the SUVR range from calibration
suvr_young <- 1.0  # Example SUVR for young controls (CL = 0)
suvr_ad <- 2.0     # Example SUVR for AD patients (CL = 100)

server <- function(input, output) {
  #bs_themer()
  #source("global.R")
  
  # DATA -----------------------------------
  PiB_gaain <- read_csv("data/PiB_gaain.csv")
  software_clinical_table <- read_csv("data/software_clinical.csv") %>%
    select(-Website)
  software_research_table <- read_csv("data/software_research.csv") 
  h2h_table <- read_csv("data/h2h_f18_tracers.csv")
  
  #---------------------------
  # INTRO OVERLAY
  #---------------------------

  observeEvent("", {
    showModal(modalDialog(
      includeHTML("texts/intro_text.Rhtml"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))
  })

  #---------------------------
  # TAB 1: Why quantify Ab?
  #---------------------------
  

  output$image_cl_vr <- renderImage({
    req(image_files)
    list(
      src = file.path("./images/CL_VR.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  output$cl_sota <- renderImage({
    req(image_files)
    list(
      src = file.path("./images/CL_SOTA.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  #---------------------------
  # TAB 2: Centiloid 101
  #---------------------------
  output$cl_scale <- renderImage({
    req(image_files)
    list(
      src = file.path("./images/CL_simple.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  
  output$cl_image <- renderImage({
    req(image_files)
    list(
      src = file.path("./www/CL_image.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  
  suvr_data <- PiB_gaain |> 
    dplyr::select(sub_id, suvr_wc) |>
    dplyr::mutate(
      dx = dplyr::case_when(
        grepl("^AD", sub_id) ~ "AD",
        grepl("^YC", sub_id) ~ "YC",
        TRUE ~ "Other"
      )
    )
  
  # Extract means for AD and YC
  mean_AD <- reactive({
    suvr_data |> 
      dplyr::filter(grepl("^AD", sub_id)) |> 
      dplyr::pull(suvr_wc) |> 
      mean(na.rm = TRUE)
  })
  
  mean_YC <- reactive({
    suvr_data |> 
      dplyr::filter(grepl("^YC", sub_id)) |> 
      dplyr::pull(suvr_wc) |> 
      mean(na.rm = TRUE)
  })
  
  
  output$stretchPlot1 <- renderPlot({
    suvr_min <- input$stretch[1]
    suvr_max <- input$stretch[2]
    
    # Linear transformation: SUVR to CL
    suvr_data <- suvr_data |>
      dplyr::mutate(CL = 100 * (suvr_wc - suvr_min) / (suvr_max - suvr_min))
    
    ggplot(suvr_data, aes(x = suvr_wc, y = 0)) +
      geom_hline(yintercept = 0) +
      geom_point(aes(x = 93.755 * suvr_wc - 94.642, y=0.01, color = dx), shape = 4, size = 5,stroke = 1.5) + 
      scale_color_manual(values = c("AD" = "red", "YC" = "blue")) +
      # Add mean for AD (red) and YC (blue)
      geom_point(aes(x = 93.755 * suvr_min - 94.642, y = 0), 
                 color = "blue", size = 5, shape = 19, stroke = 1.5) +
      geom_point(aes(x = 93.755 * suvr_max - 94.642, y = 0), 
                 color = "red", size = 5, shape = 19, stroke = 1.5) +
      scale_x_continuous(limits = c(-20, 115)) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(title = "Dynamic SUVR-to-Centiloid Stretch",
           subtitle = paste("AD mean:", round(mean_AD(), 2), " | YC mean:", 
                            round(mean_YC(), 2)))
  }, height = 300, res = 72)
  

  
  # Compute centiloid from suvr
  cl_value <- reactive({
    100 * (input$suvr_input - suvr_young) / (suvr_ad - suvr_young)
  })
  
  output$cl_output <- renderText({
    paste0("Centiloid value: ", round(cl_value(), 1), " CL")
  })
  
  output$centiloidPlot <- renderPlot({
    suvr_range <- seq(1.0, 2.0, length.out = 100)
    cl_range <- 100 * (suvr_range - suvr_young) / (suvr_ad - suvr_young)
    
    ggplot(data.frame(SUVR = suvr_range, CL = cl_range), aes(x = SUVR, y = CL)) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_point(aes(x = input$suvr_input, y = cl_value()), color = "red", size = 4) +
      geom_text(aes(x = input$suvr_input, y = cl_value(), 
                    label = paste0(round(cl_value(), 1), " CL")), 
                vjust = -1, hjust = -1, color = "red") +
      labs(
           x = "SUVR (custom pipeline)",
           y = "Centiloid (CL)") +
      theme_minimal(base_size = 16) 
  })
  
  #-------- 3D Viewer
  
  base_nifti <- readNIfTI("data/MNI152_T1_2mm.nii", reorient = FALSE)
  overlay1_nifti <- readNIfTI("data/voi_ctx_2mm.nii", reorient = FALSE)
  overlay2_nifti <- readNIfTI("data/voi_WhlCbl_2mm.nii", reorient = FALSE)
  
  render_slice_plot <- function(base_slice, overlay1, overlay2, orientation = "axial") {
    
    dim_x <- dim(base_slice)[1]
    dim_y <- dim(base_slice)[2]
    mat_base <- base_slice
    mat_ov1 <- overlay1
    mat_ov2 <- overlay2

    # Build data frame for ggplot
    df <- expand.grid(x = 1:dim_x, y = 1:dim_y)
    df$base <- as.vector(mat_base)
    df$ov1 <- as.vector(mat_ov1)
    df$ov2 <- as.vector(mat_ov2)
    
    ggplot(df, aes(x = x, y = y)) +
      geom_raster(aes(fill = base)) +
      scale_fill_gradient(low = "black", high = "white", guide = "none") +
      
      geom_raster(
        data = subset(df, ov1 > 0),
        aes(x = x, y = y),
        fill = "purple",
        alpha = 0.6,
        inherit.aes = FALSE
      ) +
      
      geom_raster(
        data = subset(df, ov2 > 0),
        aes(x = x, y = y),
        fill = "orange",
        alpha = 0.6,
        inherit.aes = FALSE
      ) +
      
      coord_fixed() +
      theme_void()+
      theme(
        plot.background = element_rect(fill = "black", color =  NA),
        panel.background = element_rect(fill = "black", color =  NA),
        plot.margin = margin(0, 0, 0, 0),
        axis.ticks = element_blank(),
        axis.text = element_blank()
      )
  }
  
  
  # Axial: no reorientation needed
  output$axialPlot <- renderPlot({
    par(bg = "black")
    z <- input$z_axial
    render_slice_plot(
      base_nifti[, , z],
      overlay1_nifti[, , z],
      overlay2_nifti[, , z],
      orientation = "axial"
    )
  }, bg = "black")
  
  # Coronal: needs transpose and flip
  output$coronalPlot <- renderPlot({
    par(bg = "black")
    y <- input$y_coronal
    render_slice_plot(
      base_nifti[, y, ],
      overlay1_nifti[, y, ],
      overlay2_nifti[, y, ],
      orientation = "coronal"
    )
  }, bg = "black")
  
  # Sagittal: same logic
  output$sagittalPlot <- renderPlot({
    par(bg = "black")
    x <- input$x_sagittal
    render_slice_plot(
      base_nifti[x, , ],
      overlay1_nifti[x, , ],
      overlay2_nifti[x, , ],
      orientation = "sagittal"
    )
  }, bg = "black")
  
  
  
  #---------------------------
  # TAB: Centiloid 101
  #---------------------------
  
  output$qc <- renderImage({
    req(image_files)
    list(
      src = file.path("./www/QC.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  
  output$atypical <- renderImage({
    req(image_files)
    list(
      src = file.path("./images/rare_patterns.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  
  output$acq <- renderImage({
    req(image_files)
    list(
      src = file.path("./www/Acq_time.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  
  
  #---------------------------
  # TAB: GAAIN
  #---------------------------
  
  output$gaain <- renderImage({
    req(image_files)
    list(
      src = file.path("./images/gaain.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  #-----------------------------------
  # TAB: Validate your own pipeline
  #-----------------------------------
  
  # Example processing
  #-----------------------------------
  output$processing <- renderImage({
    req(image_files)
    list(
      src = file.path("images/processing.png"),
      contentType = "image/png",
      width = "70%", height = "90%"
    )
  }, deleteFile = FALSE)
  
  
  # Level-1
  #-----------------------------------
  output$suvr_pib <- renderImage({
    req(image_files)
    list(
      src = file.path("www/suvr_pib.png"),
      contentType = "image/png",
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  #-----------------------------------
  
  validate_result <- eventReactive(input$validateBtn, {
    # Generate new data
    set.seed(sample(1:1e6, 1))
    df_new <- PiB_gaain[, c("sub_id", "cl_wc")]
    df_new$cl_rand <- round(sapply(df_new$cl_wc, function(x) {
      sign <- sample(c(-1, 1), 1)
      diff_pct <- runif(1, -0.01, 0.18)
      x * (1 + sign * diff_pct)
    }), 1)
    
    # Fit model
    model <- lm(cl_rand ~ cl_wc, data = df_new)
    coef_est <- coef(model)
    slope <- coef_est[2]
    intercept <- coef_est[1]
    r2 <- summary(model)$r.squared
    
    # Run tests
    tests <- data.frame(
      Condition = c("Slope between 0.98 and 1.02",
                    "Intercept between -2 and 2",
                    "R² > 0.98"),
      Passed = c(
        slope >= 0.98 & slope <= 1.02,
        intercept >= -2 & intercept <= 2,
        r2 > 0.98
      )
    )
    tests$Result <- ifelse(tests$Passed, "\u2713", "\u2717")  # ✓ or ✗
    
    list(df = df_new, model = model, tests = tests)
  }, ignoreNULL = FALSE)  # Run once on startup
  
  output$scatterPlot <- renderPlot({
    df <- validate_result()$df
    model <- validate_result()$model
    
    eq <- paste0("y = ", round(coef(model)[2], 2), "x + ", round(coef(model)[1], 2))
    r2 <- summary(model)$r.squared
    
    ggplot(df, aes(x = cl_wc, y = cl_rand)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      annotate("text", 
               x = 5, y = 140, 
               label = paste(eq, "\nR² = ", round(r2, 3)),
               hjust = 0, vjust = 1, 
               size = 5) +
      labs(
        x = "Centiloid (GAAIN)",   
        y = "New pipeline Centiloid values" 
      ) +
      theme_minimal() + xlim(c(0, 150)) + ylim(c(0, 150)) +  
      theme(
        axis.title = element_text(size = 16),  
        axis.text = element_text(size = 14)   
      )
  }, width = 350, height = 350)
  
  output$testResults <- renderTable({
    validate_result()$tests[, c("Condition", "Result")]
  }, colnames = FALSE)
  
  # Level-2
  #-----------------------------------
  output$suvrfpib <- renderPlot({
    
    h2h_data <- h2h_table 
    
    ggplot(h2h_data, aes(x = pib_suvr_wcb, y = f18_suvr_wcb, color = tracer)) +
      geom_abline(color="darkgrey") +
      geom_point(size = 2) +
      
      # Linear model per tracer
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
      
      # Add equations and R²
      stat_poly_eq(
        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
        formula = y ~ x,
        parse = TRUE,
        label.x = "left",
        label.y = "top",
        size = 4,
        coef.digits = 2
      ) +
      scale_color_manual(values = c(
        "Flutemetamol" = "#640acc", 
        "Florbetapir" = "#ed1c23",
        "Florbetaben" = "#011f5f",
        "NAV4694" = "#027f47"
      )) +
      labs(
        title = "New pipeline SUVr for PiB and F-18 tracers",
        x = "PiB SUVr (WCB)",
        y = "F-18 SUVr (WCB)",
        color = "Tracer"
      ) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 13)
      ) +
      guides(color = guide_legend(nrow = 2))
  })
  
  output$suvrtocl <- renderPlot({
    
    h2h_data <- h2h_table 
    
    ggplot(h2h_data, aes(x = f18_suvr_wcb, y = f18_cl, color = tracer)) +
      geom_point(size = 2) +
      
      # Linear model per tracer
      geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
      
      # Add equations and R²
      stat_poly_eq(
        aes(label = paste(..eq.label..)),
        formula = y ~ x,
        parse = TRUE,
        label.x = "left",
        label.y = "top",
        size = 5,
        coef.digits = 5
      ) +
      scale_color_manual(values = c(
        "Flutemetamol" = "#640acc", 
        "Florbetapir" = "#ed1c23",
        "Florbetaben" = "#011f5f",
        "NAV4694" = "#027f47"
      )) +
      labs(
        title = "New pipeline SUVr for PiB and F-18 tracers",
        x = "F-18 SUVr (WCB)",
        y = "F-18 Centiloid (WCB)",
        color = "Tracer"
      ) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 13)
      ) +
      guides(color = guide_legend(nrow = 2))
  })
  
  #-----------------------------------
  # TAB: Interpretation
  #-----------------------------------
  
  image_files <- mixedsort(list.files(file.path("./images/interpretation"), 
                                      pattern = ".(png|jpg|jpeg|gif)$", 
                                      full.names = FALSE))
  current_index <- reactiveVal(1)
  
  observeEvent(input$next_img, {
    if (current_index() < length(image_files)) {
      current_index(current_index() + 1)
    }
  })
  
  observeEvent(input$prev_img, {
    if (current_index() > 1) {
      current_index(current_index() - 1)
    }
  })
  
  # Display current image
  output$image_display <- renderImage({
    req(image_files)
    list(
      src = file.path("./images/interpretation/", image_files[current_index()]),
      contentType = "image/png",
      alt = image_files[current_index()],
      width = "80%", height = "80%"
    )
  }, deleteFile = FALSE)
  
  # Counter
  output$image_counter <- renderText({
    paste("Slide", current_index(), "of", length(image_files))
  })
  
  #-----------------------------------
  # TAB: What not to do
  #-----------------------------------

  output$cl_eq_plot <- renderPlot({
    suvr <- input$suvr_input_eq
    
    eq_params <- data.frame(
      Equations = paste0("y = ", c(174.52, 183.07, 184.12, 188.22, 203.68, 230.47, 205.72), 
                        " * x ", 
                        c(-187.26, -177.26, -233.72, -189.16, -207.15, -209.63, -233.33)),
      Slope = c(174.52, 183.07, 184.12, 188.22, 203.68, 230.47, 205.72),
      Intercept = c(-187.26, -177.26, -233.72, -189.16, -207.15, -209.63, -233.33)
    )
    
    df <- data.frame(
      Equations = eq_params$Equations,
      SUVR = suvr,
      Centiloid = as.integer(round(eq_params$Slope * suvr + eq_params$Intercept, 0))
    )
    
    
    output$cl_eq_table_1 <- renderTable({
      df
    })
    
    eq_params_true <- data.frame(
      Equations = paste0("y = ", c(174.52, 183.07, 184.12, 188.22, 203.68, 230.47, 205.72), 
                        " * x ", 
                        c(-187.26, -177.26, -233.72, -189.16, -207.15, -209.63, -233.33)),
      Slope = c(174.52, 183.07, 184.12, 188.22, 203.68, 230.47, 205.72),
      Intercept = c(-187.26, -177.26, -233.72, -189.16, -207.15, -209.63, -233.33),
      fake_CL = c(70, 62, 65, 74, 77, 71, 68)
    )
    df_true <- data.frame(
      Equations = eq_params_true$Equations,
      SUVR_true = (eq_params_true$fake_CL - eq_params_true$Intercept)/eq_params_true$Slope,
      Centiloid = as.integer(round(eq_params_true$fake_CL,0))
    )
    
    
    output$cl_eq_table_2 <- renderTable({
      df_true
    })
    
    
    ggplot() +
      geom_abline(data = eq_params, 
                  aes(slope = Slope, intercept = Intercept, color = Equations), 
                  linewidth = 1.2) +
      geom_point(data = df, aes(x = SUVR, y = Centiloid, color = Equations), size = 3) +
      #geom_text(data = df, aes(x = SUVr, y = Centiloid, label = paste0(round(Centiloid, 1), " CL"), color = Equation),
      #          vjust = -1, hjust = -0.1) +
      labs(
        title = "SUVr Values Transformed with Different Linear Equations",
        x = "SUVr",
        y = "Centiloid"
      ) +
      xlim(c(1, 1.8)) + ylim(c(-20, 200)) +
      theme_minimal(base_size = 16
      )
  }, 
  #width = 600, height = 350
  ) 
  
  #-----------------------------------
  # TAB: Software
  #-----------------------------------
  
  output$table_software_clinical <- renderTable({software_clinical_table}, align = "lcccc", striped = TRUE) 
  output$table_software_research <- renderTable({software_research_table}, align = "ll", striped = TRUE) 
  

  
}


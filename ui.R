ui <- page_fillable(
  titlePanel("AAIC25 | GAAIN Centiloid workshop"),
  theme = bs_theme(bootswatch = "lumen"),
  navset_card_tab(id = "navs",  
                  
    nav_panel(icon = bsicons::bs_icon("thermometer-half"),"Why quantify Aβ?",
              accordion(  
                accordion_panel(
                  open = TRUE,
                  title = "From visual read to quantification", 
                  icon = bsicons::bs_icon("eye-fill"),
                  div(
                    style = "text-align: left; font-size: 1em; margin-top: 1px;margin-bottom: 5px;",
                    p("Amyloid PET imaging has revolutionized our ability to detect and quantify amyloid-beta plaques in vivo — a defining pathological characterisic of Alzheimer's disease."),
                    p("")
                  ),
                  div(
                    style = "display: flex; justify-content: center;",
                    imageOutput("image_cl_vr", height = "75%", width = "75%")
                  ),
                  p(""),
                  div(
                    style = "text-align: center; font-size: 1.5em; margin-top: 1px;font-style: bold;",
                    p("Image interpretation beyond \"positive\" or \"negative\"")
                    ),
                  p(""),
                  div(
                    style = "display: flex; justify-content: center;",
                    imageOutput("cl_sota", height = "75%", width = "75%")
                  ),
                  div(
                    style = "font-size: 0.85em; line-height: 0.5em; margin-top: 1px;font-style: italic;",
                    p(""),
                    p("Adapted from:"),
                    p("Sabri, Osama, et al. \"Beta-amyloid imaging with florbetaben.\" Clinical and translational imaging (2015)"),
                    p("Cortes‐Blanco, Anabel, et al. \"Florbetapir (18F) for brain amyloid positron emission tomography: Highlights on the European marketing approval.\" Alzheimer's & Dementia (2014)"),
                    p("Pemberton, Hugh, et al. \"Quantification of amyloid PET for future clinical use: a state-of-the-art review.\" EJNMMI (2022)")                 
                    ),
                  p(""),
                  accordion_panel("Regulatory note:",
                                  open = FALSE,
                    div(
                      style = "font-size: 1em; line-height: 0.5em; margin-top: 1px",
                    p("> Until recently, FDA labels of amyloid tracers relied on visual read as \"positive\" or \"negative\" for image interpretation."),
                    p("> In May 2024, EMA published a qualification opinion for Centiloid measure of amyloid-PET."),
                    p("> Since June 2025, updated labels enable the use of quantification as ajunct to visual inspection."),),                
                  )),  
                accordion_panel(
                  open = FALSE,
                  title = "Need for a unified metric of amyloid load",
                  icon = bsicons::bs_icon("bar-chart-fill"),
                  includeMarkdown("texts/intro.Rmd")
                )
              )
              ), 
    # nav_panel("🧠 Centiloid 101", 
    #           accordion(
    #             accordion_panel("Centiloid scale",
    #                div(
    #                  style = "display: flex; justify-content: center;",
    #                  imageOutput("cl_scale", height = "100%", width = "100%")
    #                  )
    #                )
    #             )
    #           ),
    nav_panel("🧠 Centiloid 101", 
              card(                   
                div(
                style = "display: flex; justify-content: center;",
                imageOutput("cl_scale", height = "100%", width = "120%")
              )
              ),
              card(
              layout_columns( 
                card(
                  card_header("Anchoring with PiB data"),
                  div(
                    style = "text-align: center;",
                    div(
                      style = "display: block; margin-left: auto; margin-right: auto; width: 100%;",
                      imageOutput("cl_image", height = "auto", width = "100%")
                    )
                  ),
                  div(
                    style = "font-size: 0.85em; line-height: 0.5em; margin-top: 1px;font-style: italic;",
                    p(""),
                    p("Courtesy of Renaud La Joie"),)
                ),
                card(
                  card_header("Mapping SUVR to Centiloid"),
                  plotOutput("centiloidPlot"),
                  sliderInput("suvr_input", "Select SUVR value", min = 1.0, max = 2.0, value = 1.4, step = 0.01),
                  textOutput("cl_output")
                ),
                card(
                  card_header("Important"),
                  div(style = "padding-left: 1rem; padding-bottom: 0.5rem;",
                      tags$ul(
                        tags$li(HTML("The Centiloid scale uses <i>11C-PiB</i> as the reference tracer.")),
                        tags$li(HTML("<b>One Standard Calibration Dataset:</b> Use the official GAAIN dataset, which includes <i>young controls</i> (low amyloid) and <i>mild AD</i> cases (high amyloid), for Level 1 and 2 analyses.")),
                        tags$li(HTML("<b>Interpretation of CL Values:</b> The scale is anchored at 0 CL (mean of young controls) and 100 CL (mean of AD cases), but values often fall <b>below 0</b> or <b>above 100</b>.")),
                        tags$li(HTML("Some (limited) degree of measurement unvertainty should be expected."))
                        
                      )
                  )
                )),

              )
              ),
    nav_panel("🟣 GAAIN", 
                  card( 
                    full_screen = TRUE,           
                    card_header("🧠 GAAIN masks"),
                    card_title(  div(
                      style = "text-align: center;",
                      tags$a(
                        href = "https://www.gaain.org/centiloid-project",
                        target = "_blank",
                        "https://www.gaain.org/centiloid-project"
                      )
                    )
                    ),
                    layout_columns(
                      sliderInput("z_axial", "Axial (Z):", min = 1, max = 91, value = 50),
                      sliderInput("y_coronal", "Coronal (Y):", min = 1, max = 109, value = 50),
                      sliderInput("x_sagittal", "Sagittal (X):", min = 1, max = 91, value = 50),
                      col_widths = c(4, 4, 4)
                    ),
                    card(
                      style = "background-color: black; margin: 0; border: none;",
                    layout_columns(
                      plotOutput("axialPlot"),
                      plotOutput("coronalPlot"),
                      plotOutput("sagittalPlot"),
                      col_widths = c(4, 4, 4)
                    )
                   )
                    
                  )
    ),
    nav_panel("❓ How to interpret it",  value = "interpretation",
              layout_columns(
                card(
                  card_header("General guidelines for Centiloid scale interpretation"),
                  div(
                    style = "text-align: center;",
                    imageOutput(
                      "image_display",
                      height = "auto",
                      width = "75%",
                      inline = FALSE
                    ) %>% 
                      tagAppendAttributes(style = "display: block; margin: 0 auto;")
                  ),
                  fluidRow(
                    column(1, actionButton("prev_img", "⬅️")),
                    column(10, textOutput("image_counter")),
                    column(1, actionButton("next_img", "➡️"))
                  )
                ),
                col_widths = c(12)
              ),
              "Collij, Bollack, et al., Centiloid recommendations for clinical 
              context‐of‐use from the AMYPAD consortium, Alzheimer's & Dementia (2024)"
              ),
    nav_panel("⚠️ Dos and Don'ts",
              accordion(
                open = FALSE,
                accordion_panel("🚫 Do NOT use any Centiloid equations from the literature",
                                layout_columns( 
                                  card(
                                    div(style = "display: flex; justify-content: center;",
                                        sliderInput("suvr_input_eq", "Select SUVR value", min = 1.1, max = 1.8, value = 1.4, step = 0.1)
                                    ),
                                    plotOutput("cl_eq_plot")
                                  ),
                                  card(
                                    tableOutput("cl_eq_table_1"),
                                    accordion_panel("Why?",
                                    "In reality, each pipeline would give slightly different SUVR. For a specific amyloid PET scan, this could look like",
                                    tableOutput("cl_eq_table_2")
                                    )
                                  ),
                                  col_widths = c(6, 6) 
                                ), 
                                "Iaccarino, et al. A practical overview of the use of amyloid-PET Centiloid values in clinical trials and research. NeuroImage: Clinical (2025)"
                                )
              ),
              accordion(
                open = FALSE,
                accordion_panel(icon = bsicons::bs_icon("eye-fill"),"Quantification does NOT replace Visual Read | ⚠️ Atypical pattern",
                                imageOutput("atypical", height = "60%", width = "60%"),
                                div(style = "font-size: 0.85em; line-height: 0.5em; margin-top: 1px; font-style: italic;",
                                    p("Courtesy of Renaud La Joie")
                                )
                )
              ),
              accordion(
                open = FALSE,
                  accordion_panel("⏱️ Acquisition time window can impact Centiloid measurements",
                                  layout_columns(
                                    card(
                                      p("Centiloid equations for each tracers were calculated with data acquired during the following time windows:"),
                                      tags$ul(
                                        tags$li("PiB: 50-70 min post injection (p.i.)"),
                                        tags$li("NAV: 50-70 min p.i."),
                                        tags$li("FMM: 90-110 min p.i."),
                                        tags$li("FBB: 90-110 min p.i."),
                                        tags$li("FBP: 50-60 min p.i.")
                                      )
                                    ),
                                    card(
                                  imageOutput("acq", height = "auto", width = "100%")
                                  ),
                                  col_widths = c(6,6))
                  )

              )
              ),

    nav_panel("🧑‍💻 Validate your own pipeline",
              accordion(
                open = FALSE,
                accordion_panel("0 | Overview",
                                card("Step-by-step",
                                     div(style = "padding-left: 1rem;",
                                         tags$ul(
                                           tags$li(HTML("<b>Image Preprocessing</b>: Start with raw amyloid PET images. 
          <ul>
            <li>Spatially normalize each scan to MNI152 standard space (with or without MRI).</li>
            <li>Download GAAIN VOIs masks.</li>
            <li>Extract SUV from the masks and compute SUVRs.</li>
            <li>Perform quality control (e.g., check for registration failures).</li>
          </ul>
        ")),
                                           
                                           tags$li(HTML("<b>Level 1 Validation – Reproduce PiB Centiloid Analysis</b>: 
          <ul>
            <li>Download the GAAIN PiB dataset containing PET scans from young controls and mild-to-moderate AD patients.</li>
            <li>Process these scans through your pipeline to compute SUVRs and CL using the defined GAAIN target (cortex) and reference regions (e.g. whole cerebellum).</li>
            <li>Compare your pipeline’s results with those published on GAAIN.</li>
          </ul>
        ")),
                                           
                                           tags$li(HTML("<b>Level 2 Calibration – Derive Centiloid Conversion for Your Tracer</b>:
          <ul>
            <li>Download the GAAIN ‘head-to-head’ datasets, which include participants scanned with PiB and F-18 tracers.</li>
            <li>Compute SUVRs for both tracers using identical processing steps.</li>
            <li>Perform a linear regression of the F-18 tracer’s SUVRs against PiB SUVRs to obtain a conversion equation (slope and intercept).</li>
            <li>Derive tracer-specific SUVR-to-Centiloid conversion equations.</li>
          </ul>
        "))
                                         )
                                     )
                                )
                ),
                accordion_panel("1 | Image Processing",
                  card(
                    p("Example of a processing pipeline (SPM-based, with MR)"),
                    imageOutput("processing"),

                  )
                ),
              accordion_panel("2 | Calibration: step-by-step",
                card(
                layout_columns(
                  
                  
                  ## Card 1: Level-1
                  card(
                    h3("1️⃣ Level-1"),
                    accordion(
                      open = FALSE,
                      accordion_panel(
                        "Test & Validate with GAAIN PiB data",
                        div(style = "display: flex; justify-content: center;",
                            div(style = "width: 600px;", plotOutput("scatterPlot"))
                        ),
                        div(style = "display: flex; justify-content: center;",
                            actionButton("validateBtn", "Generate Data & Run Test")
                        ),
                        div(style = "display: flex; margin-top: 2px; justify-content: center;",
                            tableOutput("testResults")
                        )
                      )
                    )
                  ),
                  
                  ## Card 2: Level-2 Step 1
                  card(
                    h3("2️⃣ Level-2 (Step 1)"),
                    div(style = "text-align: center;",
                        div(style = "display: block; margin-left: auto; margin-right: auto; width: 85%;",
                            imageOutput("suvr_pib", height = "auto", width = "100%")
                        )
                    ),
                    div(style = "font-size: 0.85em; line-height: 0.5em; margin-top: 1px; font-style: italic;",
                        p("Courtesy of Renaud La Joie")
                    ),
                    accordion(
                      open = FALSE,
                      accordion_panel(
                        "Compute PiB-equivalent SUVR",
                        plotOutput("suvrfpib"),
                        div(
                          style = "text-align: center; font-size: 1em; line-height: 1.5em; margin-top: 1px;",
                          tags$ul(
                            tags$li("NAV SUVR = 1.1 x PiB SUVR - 0.07"),
                            tags$li("FMM SUVR = 0.61 x PiB SUVR + 0.39"),
                            tags$li("FBB SUVR = 0.77 x PiB SUVR + 0.22"),
                            tags$li("FBP SUVR = 0.54 x PiB SUVR + 0.50")
                          ),
                          p(HTML("Test condition: R<sup>2</sup> > 0.70 (satisfied for all tracers here)."))
                        )
                      )
                    )
                  ),
                  
                  ## Card 3: Level-2 Step 2
                  card(
                    h3("2️⃣ Level-2 (Step 2)"),
                    accordion(
                      open = FALSE,
                      accordion_panel(
                        "Compute SUVR-to-Centiloid Conversion",
                        plotOutput("suvrtocl"),
                        div(
                          style = "text-align: center; font-size: 1em; line-height: 1.5em; margin-top: 1px;",
                          tags$ul(
                            tags$li("NAV CL = 85.36 x NAV SUVR - 88.66"),
                            tags$li("FMM CL = 121.47 x FMM SUVR - 121.3"),
                            tags$li("FBB CL = 153.55 x FBB SUVR - 155.09"),
                            tags$li("FBP CL = 175.18 x FBP SUVR - 182.24")
                          ),
                          p(HTML("<span style='color: red; font-weight: bold;'>⚠️ THESE EQUATIONS CANNOT BE USED TO CONVERT ANY SUVR VALUES. (except if the data has been acquired and processed in the EXACT same way as desbribed in the papers used to derive these equations (see Resources)).</span>"))
                        )
                      )
                    )
                  ),
                  
                  col_widths = c(4,4,4)
                
              ))) )
    )
    ,
    nav_panel("💿 Software", 
    accordion(  
      open = FALSE,
      accordion_panel( 
        title = "Clinical software", 
        icon = bsicons::bs_icon("file-earmark-medical"),
        "(List is not exhaustive!)",
        tableOutput("table_software_clinical"),
        "* Regulatory approval as of July 2025"
       
      ),  
      accordion_panel(
        title = "Research software",
        icon = bsicons::bs_icon("easel2"),
        "(List is not exhaustive!)",
        tableOutput("table_software_research")
        
      ))),
    nav_menu( 
      "Resources", 
      nav_panel("Literature", 
                includeMarkdown("texts/refs.Rmd")), 
      "----", 
      "Websites:", 
      nav_item( 
        a("GAAIN", href = "http://www.gaain.org/centiloid-project"),
        a("EMA Qualification Opinion",  href = "https://www.ema.europa.eu/en/documents/other/qualification-opinion-centiloid-measure-amyloid-pet-quantify-brain-amyloid-deposition_en.pdf"),
        a("PubMed Centiloid", href = "https://pubmed.ncbi.nlm.nih.gov/?term=centiloid")
        
      ), 
    ), 
  ), 
  id = "tab",
  fluid = TRUE
)



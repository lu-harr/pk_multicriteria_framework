# Define UI for dataset viewer application
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  prompter::use_prompt(),
  
  titlePanel("Tool for multicriteria surveillance site selection"),
  
  sidebarLayout(
    sidebarPanel(width=3,
                 id="sidebar",
                 # OBJECTIVE
                 h4("Objective Function"),
                 div(style="display: inline-block;vertical-align:middle; width: 100px;",
                     selectInput("objective_direction","",c("Minimise", "Maximise"), "Maximise")),
                 div(style="display: inline-block;vertical-align:middle; width: 250px;",
                     selectInput("objective_variable","",
                                 c("Mean / Std Dev", "Mean x Std Dev", "Mean"), "Mean / Std Dev")),
                 
                 # NUMBER OF SITES
                 numericInput("nsites","Number of sites","10"),
                 prompter::add_prompt(
                   checkboxInput("include_excluded_sites", "Show excluded sites"), # what is the $name of this?
                   position = "right",
                   rounded =  TRUE,
                   message = paste("Show sites with objective values higher than those selected,",
                                    "or sites that would be selected in the absence of specified constraints",
                                   sep=" ")),
                 # shinyBS::bsTooltip("include_excluded_sites", "Show sites with objective values higher than those selected, or sites that would be selected in the absence of specified constraints",
                 #                    placement = "bottom", trigger = "hover",
                 #                    options = NULL),
                 
                 h4("Sites to exclude (%):"),
                 
                 # CONSTRAINTS
                 splitLayout(cellWidths=c("60%","20%", "20%"),
                             uiOutput("access_thresh_s"),
                             uiOutput("access_thresh_n_lower"),
                             uiOutput("access_thresh_n_upper")
                 ),
                 
                 splitLayout(cellWidths=c("60%","20%", "20%"),
                             uiOutput("forest_thresh_s"),
                             uiOutput("forest_thresh_n_lower"),
                             uiOutput("forest_thresh_n_upper")
                 ),
                 
                 h5("Distance from selected sites:"),p("Exclude sites less than "),
                 div(style="display: inline-block;vertical-align:middle; width: 80px;",
                     numericInput("dist_sites","","50")),
                 div(style="display: inline-block;vertical-align:moddle; width: 20px;",
                     p("km")),
                 p("from selected sites"), # include a slider down here?
                 
                 br(),
                 br(),
                 actionButton("update", "Go!"), # apply changes
                 
                 br(),
                 
    ),
    
    
    mainPanel(width=9,
              tabsetPanel(
                # landing tab
                tabPanel("Get Started",
                         br(),
                         includeMarkdown("get_started.md")),
                
                # map of objective, table of sites
                tabPanel("Map",
                         prompter::add_prompt(
                           actionButton("info", label = "Hint!"),
                           position="bottom", 
                           rounded = TRUE,
                           message="Brush an area in the map to see only the captured subset of sites in the downloadable table!"),
                         # shinyBS::bsTooltip("info", "Brush an area in the map to see only the captured subset of sites in the downloadable table!",
                         #                    placement = "bottom", trigger = "hover",
                         #                    options = NULL),
                         br(),
                         plotOutput("overlay_plot", width="100%", brush="overlay_plot_brush"),
                         br(),
                         DT::dataTableOutput("sites_table"),
                         downloadButton("download_sites", "Download ranked sites")),
                         
                # visualise effect of multiple? constraints
                tabPanel("Constraints",
                         br(),
                         plotOutput("constraint_plot", width="100%")
                         ), # big ol plot in here
                id="tabset"
              ),
              id="main"
    )
  )
)







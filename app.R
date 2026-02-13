####################################### WELCOME TO THE SHINY APP ##################################
####################################### from Sandra K. (2026) #####################################
###################################################################################################

# # Get template for the dataset
# library(writexl)
# library(readxl)
# 
# data <- reflimR::livertests
# 
# write.csv(data, "reflim_csv.csv", row.names = FALSE)
# write.csv2(data, "reflim_csv2.csv", row.names = FALSE)
# write_xlsx(data, "reflim_excel.xlsx")
# 
# dataset_original1 <- read.csv("reflim_csv.csv")
# dataset_original2 <- read.csv2("reflim_csv2.csv")
# dataset_original3 <- read_excel("reflim_excel.xlsx")
# 
# write.csv2(dataset_original1, "reflim_data1.csv", row.names = FALSE)
# write.csv2(dataset_original2, "reflim_data2.csv", row.names = FALSE)
# write.csv2(dataset_original3, "reflim_data3.csv", row.names = FALSE)

####################################### Load Script and Example-Dataset ###########################

source("functions.R")

####################################### Libraries #################################################

if ("DT" %in% rownames(installed.packages())) {
  library(DT)} else{
    install.packages("DT")
    library(DT)}

if ("mclust" %in% rownames(installed.packages())) {
  library(mclust)} else{
    install.packages("mclust")
    library(mclust)}

if ("refineR" %in% rownames(installed.packages())) {
  library(refineR)} else{
    install.packages("refineR")
    library(refineR)}

if ("reflimR" %in% rownames(installed.packages())) {
  library(reflimR)} else{
    install.packages("reflimR")
    library(reflimR)}

if ("rhandsontable" %in% rownames(installed.packages())) {
  library(rhandsontable)} else{
    install.packages("rhandsontable")
    library(rhandsontable)}

if ("readxl" %in% rownames(installed.packages())) {
  library(readxl)} else{
    install.packages("readxl")
    library(readxl)}

if ("shinydashboard" %in% rownames(installed.packages())) {
  library(shinydashboard)} else{
    install.packages("shinydashboard")
    library(shinydashboard)}

dataset_original <- reflimR::livertests
text1 <- HTML(paste0(
  "This Shiny App is based on the package ", a("reflimR", href = "https://cran.r-project.org/web/packages/reflimR/index.html"), 
  " for the estimation of reference limits from routine laboratory results:", br(), br(), 
  "These columns should be used for new data: Category: Name of the category to filter the data; Age: Age in years; Sex: m for male and f for female;
  Value: Column name is the analyte name, values are the laboratory measures.Starting with the fourth column, enter the laboratory value; the other three columns can be in any order. The data from *livertests* serves as a template. 
  To load new data, the data should be in CSV format with values separated by semicolons (;), and decimal numbers should use a comma (,) as the decimal separator. The first row should contain column headers.
  Alternatively, the data can be loaded into the editable table using the copy-and-paste function or with .xlsx.", br(), br(),
  "On the left side, the sidebar allows you to select the laboratory parameter, category, age and gender group. 
  In the “Target Values” section, you can load target values from targetvalues, load reference intervals estimated with refineR, or manually enter custom values."
))
text2<- HTML(paste0(
  "These tab displays the corresponding plot and the outputs of the reflim() function, providing an estimation of new reference intervals or a verification of the selected target values. 
  By clicking “Visualization of all plots across every process step”, all plots generated throughout the workflow can be displayed."
))
text3<- HTML(paste0(
  "If, during the verification with reflimR and its target values or own target values, a yellow or red bar appears, 
  a follow-up analysis using refineR is recommended. The resulting reference intervals from refineR can be used as new target values
  and re-verified with reflimR. If all indicators turn green, this suggests that the manufacturer’s target values are likely incorrect. 
  If one or more indicators remain yellow or red, the data are considered too challenging for indirect methods. This assumption can be further 
  evaluated in the “mclust” tab using a Gaussian mixture model (mclust)."
))
text4 <- HTML(paste0(
  "Gaussian mixture modelling for the verification of reference intervals."
))

####################################### User Interface ############################################

ui <- dashboardPage(
  dashboardHeader(title = "VeRIf", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebarid",
      
      div(
        style = "text-align:center",
        br(),
        "Verification of reference limits", br(),
        "from routine laboratory results", hr()
      ),
      
      uiOutput("parameters"),
      uiOutput("category"),
      
      selectInput(
        "sex",
        "Select the sex:",
        choices = c("Female (F) & Male (M)" = "t", "Female (F)" = "f", "Male (M)" = "m")
      ),
      
      sliderInput(
        "age_end",
        "Select age-range:",
        min = 0,
        max = 100,
        value = c(0, 100)),
      
      numericInput(
        "nmin",
        "Select n.min:",
        200,
        min = 40,
        max = 1000
      ),
      
      hr(),
      
      checkboxInput("check_targetvalues", "Load preinstalled target values", value = FALSE),
      checkboxInput("check_target", "Load own target values", value = FALSE),
      
      conditionalPanel(
        condition = "input.check_target == true",
        
        numericInput(
          "target_low",
          "Lower value:",
          10,
          min = 0,
          max = 10000
        ),
        
        numericInput(
          "target_upper",
          "Upper value:",
          15,
          min = 0,
          max = 10000
        )
      ), 
      uiOutput("refineR_checkbox_ui"),
      hr()
    )
  ),
  
  dashboardBody(
    fluidRow(
      
      tabsetPanel( 
        tabPanel("Data", 
                 icon = icon("upload"),
                 
                 box(
                   title = "",
                   status = "info",
                   width = 7,
                   solidHeader = TRUE,
                   
                   p(text1),
                   
                   fluidRow(
                     column(6, checkboxInput("show_table", "Show and use editable table for upload. Please click Submit!", value = FALSE)),
                     column(6, actionButton("submit", "Submit"))
                   ),
                   conditionalPanel(
                     condition = "input.show_table == true",
                     rHandsontableOutput("editable_table")
                   ), hr(),
                   
                   uiOutput("dataset_file"),
                   actionButton('reset', 'Reset Input', icon = icon("trash")), hr(),
                   
                   DT::dataTableOutput("table")
                 )
        ),
        
        tabPanel("reflimR", 
                 icon = icon("chart-line"), 
                 
                 box(
                   title = "",
                   width = 7,
                   solidHeader = TRUE,
                   status = "info",
                   
                   p(text2),
                   checkboxInput("check_plot.all", "Visualization of all plots across every process step"),
                   plotOutput("plot", height = "700px")
                 )
        ),
         
        tabPanel( "refineR", 
                  icon = icon("table"),
                  
                  box(
                    title = "",
                    status = "info",
                    width = 7,
                    solidHeader = TRUE,
                    
                    p(text3),
                    plotOutput("plotrefineR", height = "700px"),
                    DT::dataTableOutput("table_report_refineR")
                  )
        ),
        
        tabPanel( "mclust", 
                  icon = icon("table"),
                  
                  box(
                    title = "",
                    status = "info",
                    width = 7,
                    solidHeader = TRUE,
                    
                    p(text4),
                    plotOutput("plotmclust", height = "700px"),
                  )
        )
      ),
      
      box(
        title = tagList(shiny::icon("table"), "Report:"),
        status = "info",
        width = 5,
        solidHeader = TRUE,
        
        DT::dataTableOutput("table_report")
      )
    )
  )
)

####################################### Server ####################################################

server <- function(input, output, session) {
  
  ##################################### Observe Events ############################################
  
  options(shiny.sanitize.errors = TRUE)
  options(warn = -1)
  
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$dataset_file1, {
    values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$reset, {
    values$upload_state <- 'reset'
  })
  
  dataset_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$dataset_file1)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  output$dataset_file <- renderUI({
    input$reset ## Create a dependency with the reset button
    fileInput('dataset_file1', label = NULL,  multiple = FALSE)
  })
  
  output$parameters <- renderUI({
    if(input$show_table && input$submit) {
      choices <- colnames(data_store())[4:length(colnames(data_store()))]
    } else{
    if (is.null(dataset_input())) { 
      choices <- colnames(dataset_original)[4:length(colnames(dataset_original))]
    } 
    else{
      datapath <- dataset_input()[["datapath"]]
    
      validate(need(
        grepl("\\.(csv|xlsx)$", datapath, ignore.case = TRUE),
        "Check if you have used the correct template! It must be a CSV or XLSX file!"
      ))
      
      if (grepl("\\.csv$", datapath, ignore.case = TRUE)) {
        dataset <- read.csv2(datapath)
      } else {
        dataset <- as.data.frame(readxl::read_excel(datapath), stringsAsFactors = FALSE)
      }
      
      choices <- colnames(dataset)[4:ncol(dataset)]
      }}
    selectInput("parameter","Select laboratory value:", choices = choices, selected = TRUE)
  })   
  
  output$category <- renderUI({
    if(input$show_table && input$submit) {
      choices <- unique(data_store()[[1]])
    } else{
      if (is.null(dataset_input())) { 
        choices <- unique(dataset_original[[1]])
      } else{
        datapath <- dataset_input()[["datapath"]]
        validate(need(
          grepl("\\.(csv|xlsx)$", datapath, ignore.case = TRUE),
          "Check if you have used the correct template! It must be a CSV or XLSX file!"
        ))
        if (grepl("\\.csv$", datapath, ignore.case = TRUE)) {
          dataset <- read.csv2(datapath)
        } else {
          dataset <- as.data.frame(readxl::read_excel(datapath), stringsAsFactors = FALSE)
        }
        choices <- unique(dataset[[1]])
      }
    }
    choices <- c("Not selected", choices)
    selectInput("category", "Select category:", choices = choices, selected = "Not selected")
  })
  
  
  # Create a reactive values to track the state of the checkboxes
  reactive_values <- reactiveValues(
    check_targetvalues = FALSE,
    check_target = FALSE,
    check_refineR = FALSE
  )
  
  # Observe changes in check_targetvalues and update the reactive value
  observeEvent(input$check_targetvalues, {
    if (input$check_targetvalues) {
      reactive_values$check_targetvalues <- TRUE
      reactive_values$check_target <- FALSE
      reactive_values$check_refineR <- FALSE
    } else {
      reactive_values$check_targetvalues <- FALSE
    }
  })
  
  # Observe changes in check_target and update the reactive value
  observeEvent(input$check_target, {
    if (input$check_target) {
      reactive_values$check_target <- TRUE
      reactive_values$check_targetvalues <- FALSE
      reactive_values$check_refineR <- FALSE
    } else {
      reactive_values$check_target <- FALSE
    }
  })
  
  # Observe changes in check_refineR and update the reactive value
  observeEvent(input$check_refineR, {
    if (input$check_refineR) {
      reactive_values$check_refineR <- TRUE
      reactive_values$check_targetvalues <- FALSE
      reactive_values$check_target <- FALSE
    } else {
      reactive_values$check_refineR <- FALSE
    }
  })
  
  # Update the checkboxes based on the reactive value
  observe({
    updateCheckboxInput(session, "check_targetvalues", value = reactive_values$check_targetvalues)
    updateCheckboxInput(session, "check_target", value = reactive_values$check_target)
    updateCheckboxInput(session, "check_refineR", value = reactive_values$check_refineR)
  })
  
  observeEvent(input$submit, {
    if (!is.null(input$editable_table)) { data_store(hot_to_r(input$editable_table))}
  })

  initial_data <- data.frame(
    Category = character(50),
    Age = numeric(50),
    Sex = character(50),
    Analyte = numeric(50),
    #Analyte1 = numeric(50),
    #Analyte2 = numeric(50),
    #Analyte3 = numeric(50),
    stringsAsFactors = FALSE
  )

  data_store <- reactiveVal(initial_data)

  observeEvent(input$show_table, {
    if (input$show_table) {
      output$editable_table <- renderRHandsontable({
        rhandsontable(data_store(), width = '800',
                      height = 550, rowHeaders = NULL, colHeaders = colnames(data_store()))
      })
    }
  }, ignoreNULL = FALSE)
  ##################################### Reactive Expressions ######################################
  
  # Create the table with the dataset as reactive expression 
  reflim_data <- reactive({
    
    input$nmin
    input$sex
    input$parameter
    input$category
    input$check_plot.all
    input$check_targetvalues
    input$check_refineR
    input$check_target
    input$dataset_file
    input$age_end
    
    if(input$show_table && input$submit) {
       dataset <- data_store()
    } else{
    
      if (is.null(dataset_input())) {
        dataset <- dataset_original
      } else {
        datapath <- dataset_input()[["datapath"]]
        
        validate(need(
          grepl("\\.(csv|xlsx)$", datapath, ignore.case = TRUE),
          "Check if you have used the correct template! It must be a CSV or XLSX file!"
        ))
        
        if (grepl("\\.csv$", datapath, ignore.case = TRUE)) {
          dataset <- read.csv2(datapath)
        } else if (grepl("\\.xlsx$", datapath, ignore.case = TRUE)) {
          dataset <- as.data.frame(readxl::read_excel(datapath), stringsAsFactors = FALSE)
        }
        
        validate(need(
          nrow(dataset) > 0,
          "Check if you have used the correct template! The dataset is empty!"
        ))
      }}
    
    column_number <- which(names(dataset) == input$parameter)
    dataset <- dataset[c(1, 2, 3, column_number)]
    
    if (!is.null(input$category) && input$category != "Not selected") {
      dataset <- subset(dataset, dataset[[1]] == input$category)
    }
    
    validate(need(
      ncol(dataset) == 4, 
      "Check if you have used the correct template! You need 4 columns (Category, Age, Sex, Value)!"
    ))
    dataset[, 4] <- as.numeric(dataset[, 4])
    
    dataset <- subset(dataset, Age >= input$age_end[1] & Age <= input$age_end[2])
    
    if (input$sex %in% c("m", "f")) {
      dataset <- subset(dataset, Sex == input$sex)
    }
    
    return(dataset)
  })
  
  get_alldata_file <- reactive({
    
    input$nmin
    input$sex
    input$parameter
    input$category
    input$check_plot.all
    input$check_targetvalues
    input$check_refineR
    input$check_target
    input$dataset_file
    input$age_end
    
    if(input$show_table && input$submit) {
      dataset <- data_store()
    } else{
      
      if (is.null(dataset_input())) {
        dataset <- dataset_original
      } else {
        datapath <- dataset_input()[["datapath"]]
        
        validate(need(
          grepl("\\.(csv|xlsx)$", datapath, ignore.case = TRUE),
          "Check if you have used the correct template! It must be a CSV or XLSX file!"
        ))
        
        if (grepl("\\.csv$", datapath, ignore.case = TRUE)) {
          dataset <- read.csv2(datapath)
        } else if (grepl("\\.xlsx$", datapath, ignore.case = TRUE)) {
          dataset <- as.data.frame(readxl::read_excel(datapath), stringsAsFactors = FALSE)
        }
        
        validate(need(
          nrow(dataset) > 0,
          "Check if you have used the correct template! The dataset is empty!"
        ))
      }}
    
    
    if (!is.null(input$category) && input$category != "Not selected") {
      dataset <- subset(dataset, category == input$category)
    }
    
    dataset <- subset(dataset, Age >= input$age_end[1] & Age <= input$age_end[2])
    
    if (input$sex %in% c("m", "f")) {
      dataset <- subset(dataset, Sex == input$sex)
    }
    
    return(dataset)
  })
  
  get_data_report <- reactive({
    
    dat <- reflim_data()
    validate(need(nrow(dat) > 39,
                  "(reflim) n = 0. The absolute minimum for reference limit estimation is 40."))
    
    if (input$check_target == FALSE && input$check_targetvalues == FALSE && input$check_refineR == FALSE) {
      reflim_text <- reflim(dat[,4], n.min = input$nmin, plot.all = FALSE)
    }
    
    if (input$check_target) {
      validate(need(input$target_low < input$target_upper,
                    "(reflim) the upper target limit must be greater than the lower target limit."))
      
      validate(need(input$target_low > 0, 
                    "(reflim) the lower target limit must be greater than 0."))
      
      validate(need(input$target_upper > 0, 
                    "(reflim) the upper target limit must be greater than 0."))
      
      validate(need(input$target_low > 0 && input$target_upper > 0, 
                    "(reflim) the lower and upper target limit must be greater than 0."))
      
      reflim_text <- reflim(dat[,4], targets = c(input$target_low, input$target_upper), n.min = input$nmin, plot.all = FALSE)
    }
    
    if (input$check_targetvalues) {
      
      validate(need(input$sex != "t", 
                    "(reflim) The reference intervals are sex-specific. Please select a sex."))
      
      targets <- reflimR::targetvalues
      targets_values <- targets[targets$analyte == input$parameter, ]
      
      if (input$sex == "m") {
        targetvalues_low <-  targets_values[, 5]
        targetvalues_upper <- targets_values[, 6]
      }
      if (input$sex == "f") {
        targetvalues_low <- targets_values[, 3]
        targetvalues_upper <- targets_values[, 4]
      }
      
      validate(need(nrow(targets_values) > 0, 
                    "(reflim) There are no preloaded target values for this parameter!"))
      
      reflim_text <- reflim(dat[,4], targets = c(targetvalues_low, targetvalues_upper), n.min = input$nmin, plot.all = FALSE)
    }
    
    if(input$check_refineR) {
      
      validate(need(refineR_done(),"(refineR) Please perform the refineR calculation first."))
      
      table_refineR <- getRI(fit_refineR())
      targetvalues_low <- table_refineR$PointEst[1]
      targetvalues_upper <- table_refineR$PointEst[2]
      
      reflim_text <- reflim(dat[,4], targets = c(targetvalues_low, targetvalues_upper), n.min = input$nmin, plot.all = FALSE)
    }
    
    report <- reflim_text
    
    return(report)
  })
  
  ##################################### Output ####################################################
  
  output$plot <- renderPlot({
    
    dat <- reflim_data()
    validate(need(nrow(dat) > 39,
                  "(reflim) n = 0. The absolute minimum for reference limit estimation is 40."))
    
    reflimR.plot.all <- FALSE
    
    if (input$check_plot.all) {
      reflimR.plot.all <- TRUE
    }
    
    if (input$check_target) {
      validate(need(input$target_low < input$target_upper,
                    "(reflim) the upper target limit must be greater than the lower target limit."))
      
      validate(need(input$target_low > 0, 
                    "(reflim) the lower target limit must be greater than 0."))
      
      validate(need(input$target_upper > 0, 
                    "(reflim) the upper target limit must be greater than 0."))
      
      validate(need(input$target_low > 0 && input$target_upper > 0, 
                    "(reflim) the lower and upper target limit must be greater than 0."))
      
      reflim_result <- reflim(dat[, 4], targets = c(input$target_low, input$target_upper), n.min = input$nmin, plot.all = reflimR.plot.all)
    }
    
    if (input$check_targetvalues) {
      validate(need(input$sex != "t",
                    "(reflim) The reference intervals are sex-specific. Please select a sex."))
      
      targets <- reflimR::targetvalues
      targets_values <- targets[targets$analyte == input$parameter,]
      
      if (input$sex == "m") {
        targetvalues_low <-  targets_values[, 5]
        targetvalues_upper <- targets_values[, 6]
      }
      if (input$sex == "f") {
        targetvalues_low <- targets_values[, 3]
        targetvalues_upper <- targets_values[, 4]
      }
      
      validate(need(nrow(targets_values) > 0, 
                    "(reflim) There are no preloaded target values for this parameter!"))
      
      reflim_result <- reflim(dat[, 4], targets = c(targetvalues_low, targetvalues_upper), n.min = input$nmin, plot.all = reflimR.plot.all)
    }
    
    if(input$check_refineR) {
      
      validate(need(refineR_done(), "(refineR) Please perform the refineR calculation first."))
      
      table_refineR <- getRI(fit_refineR())
      targetvalues_low <- table_refineR$PointEst[1]
      targetvalues_upper <- table_refineR$PointEst[2]
      
      reflim_result <- reflim(dat[,4], targets = c(targetvalues_low, targetvalues_upper), n.min = input$nmin, plot.all = reflimR.plot.all)
    }
    
    if (input$check_target == FALSE && input$check_targetvalues == FALSE && input$check_refineR == FALSE) {
      reflim_result <- reflim(dat[, 4], n.min = input$nmin, plot.all = reflimR.plot.all)
    }
    
    reflim_result
  })
  
  output$table <- DT::renderDataTable({
    
    DT::datatable(reflim_data(), caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;','Dataset'))
  })
  
  output$table_report <- DT::renderDataTable({
    
    report <- get_data_report()
    if (!is.na(report$limits[1])) {
      converted_sex <- switch(input$sex,
                              "f" = "Female(F)",
                              "m" = "Male(M)",
                              "t" = "Female(F) & Male(M)")
      
      if(report$lognormal){
        lambda = 0
      } else{
        lambda = 1
      }
      
      report_versus <- verus.limits(report$limits[1], report$limits[2], lambda = lambda)
      
      report_lower_VeRUS <- c(round(report_versus$lower.lim.low, 1), round(report_versus$lower.lim.upp, 1))
      report_upper_VeRUS <- c(round(report_versus$upper.lim.low, 1), round(report_versus$upper.lim.upp, 1))
      
      if(is.na(report$targets[1])){
        report_lower_target_VeRUS <- c(NA, NA)
        report_upper_target_VeRUS <- c(NA, NA)
      } else{
        report_target_versus <- verus.limits(report$targets[1], report$targets[2], lambda = lambda)
        
        report_lower_target_VeRUS <- c(round(report_target_versus$lower.lim.low, 1), round(report_target_versus$lower.lim.upp, 1))
        report_upper_target_VeRUS <- c(round(report_target_versus$upper.lim.low, 1), round(report_target_versus$upper.lim.upp, 1))
      }
      
      table_report <- t(data.frame(
        "Sex and Age:" = paste0(converted_sex, " (", input$age_end[1], "-", input$age_end[2], ")"),
        "Category:" = input$category,
        "Mean (sd):" = paste0(round(report$stats[1], 2), " (", round(report$stats[2], 2), ")"),
        "Lognormal Distribution:" = report$lognormal,
        "Reference limit:" =  paste0(report$limits[1] , " - " , report$limits[2]),
        "Lower tolerance intervals (pU):" = paste0(report$limits[3], " - " , report$limits[4]),
        "Upper tolerance intervals (pU):" = paste0(report$limits[5], " - " , report$limits[6]),
        "Lower VeRUS intervals:" = paste0(report_lower_VeRUS[1], " - " , report_lower_VeRUS[2]),
        "Upper VeRUS intervals:" = paste0(report_upper_VeRUS[1], " - " ,report_upper_VeRUS[2]),
        "Lower confidence intervals:" = paste0(report$confidence.int[1], " - " , report$confidence.int[2]),
        "Upper confidence intervals:" = paste0(report$confidence.int[3], " - " , report$confidence.int[4]),
        "Target Limits:" = paste0(report$targets[1], " - " , report$targets[2]),
        "Lower target tolerance intervals (pU):" = paste0(report$targets[3], " - " , report$targets[4]),
        "Upper target tolerance intervals (pU):" = paste0(report$targets[5], " - " , report$targets[6]),
        "Lower VeRUS target intervals:" = paste0(report_lower_target_VeRUS[1], " - " , report_lower_target_VeRUS[2]),
        "Upper VeRUS target intervals:" = paste0(report_upper_target_VeRUS[1], " - " , report_upper_target_VeRUS[2]),
        "Interpretation of the lower limit:" = report$interpretation[1],
        "Interpretation of the upper limit:" = report$interpretation[2],
        check.names = FALSE))
      colnames(table_report) <- input$parameter
      
      DT::datatable(table_report, extensions = 'Buttons',
                    options = list(dom = 'Bt', pageLength = 18, buttons = c('copy', 'csv', 'pdf', 'print')))
    }
  })
   
  refineR_done <- reactiveVal(FALSE)
  
  observeEvent(input$parameters, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$category, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$sex, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$age_end, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$nmin, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$reset, { 
    refineR_done(FALSE)
  })
  
  observeEvent(input$dataset_file1, { 
    refineR_done(FALSE)
  })
  
  fit_refineR <- reactive({
    
    input$parameters
    input$category
    input$sex
    input$age_end
    input$nmin
    input$reset
    input$dataset_file1
    
    withProgress(message = "RI calculation with refineR …", {
      
      dat <- reflim_data()
      fit <- findRI(Data = dat[, 4])
      refineR_done(TRUE)
      
      fit
    })
  })
  
  output$plotrefineR <- renderPlot({
    plot(fit_refineR())
  })
  
  output$refineR_checkbox_ui <- renderUI({
    checkboxInput(
      "check_refineR",
      "Load calculated refineR values",
      value = FALSE
    )
  })
  
  output$table_report_refineR <- DT::renderDataTable({
    
    table_refineR <- getRI(fit_refineR())
    
    converted_sex <- switch(input$sex,
                            "f" = "Female(F)",
                            "m" = "Male(M)",
                            "t" = "Female(F) & Male(M)")
    
    table_report <- t(data.frame(
      "Sex and Age:" = paste0(converted_sex, " (", input$age_end[1], "-", input$age_end[2], ")"),
      "Category:" = input$category,
      "Reference limit:" =  paste0(round(table_refineR$PointEst[1], 3) , " - " , round(table_refineR$PointEst[2], 3)),
      check.names = FALSE))
    colnames(table_report) <- input$parameter
    
    DT::datatable(table_report, extensions = 'Buttons',
                  caption = htmltools::tags$caption(
                    style = "caption-side: top; text-align: left; font-weight: bold; font-size: 16px;",
                    "Report for refineR"
                  ), options = list(dom = 'Bt', pageLength = 15, buttons = c('copy', 'csv', 'pdf', 'print')))
  })
  
  output$plotmclust <- renderPlot({
    
    withProgress(message = "mclust Calculation …", {
      dat <- reflim_data()
      lab_mclust(dat[, 4])
    })
  })
}
####################################### Run the application #######################################
shinyApp(ui = ui, server = server)
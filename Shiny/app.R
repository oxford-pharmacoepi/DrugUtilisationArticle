library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(PatientProfiles)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(IncidencePrevalence)
library(DiagrammeR)
library(DiagrammeRsvg)

load("mergedData.Rdata")

source(here("functions.R"))

# theme -----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#0c0e0c" 
  ),
  adminlte_sidebar(
    # width = "400px",
    dark_bg = "#78B7C5",
    dark_hover_bg = "#3B9AB2",
    dark_color = "white"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
  ),
  adminlte_vars(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)

# ui -----
ui <- dashboardPage(
  dashboardHeader(title = "DrugUtilisation"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      menuItem(
        text = "Databases",
        tabName = "dbs",
        menuSubItem(
          text = "Database details",
          tabName = "cdm_snapshot"
        )
      ),
      menuItem(
        text = "Study cohorts",
        tabName = "cohorts",
        menuSubItem(
          text = "Cohort settings",
          tabName = "settings"
        ),
        menuSubItem(
          text = "Cohort counts",
          tabName = "counts"
        ),
        menuSubItem(
          text = "Cohort attrition",
          tabName = "attrition"
        )
      ),
      menuItem(
        text = "Population-level DrugUtilisation",
        tabName = "pop_level",
        menuSubItem(
          text = "Population-level incidence",
          tabName = "incidence"
        ),
        menuSubItem(
          text = "Population-level prevalence",
          tabName = "prevalence"
        )
      ),
      menuItem(
        text = "Patient-level DrugUtilisation",
        tabName = "pat_level",
        menuSubItem(
          text = "Indications",
          tabName = "indication"
        ),
        menuSubItem(
          text = "Dosage",
          tabName = "dose"
        ),
        menuSubItem(
          text = "Treatment discontinuation",
          tabName = "discontinuation"
        ),
        menuSubItem(
          text = "Alternative treatments",
          tabName = "patterns"
        )
      ),
      menuItem(
        text = "Patient-level Characterisation",
        tabName = "char",
        menuSubItem(
          text = "Individuals characteristics",
          tabName = "characteristics"
        ),
        menuSubItem(
          text = "Large scale characteriscs",
          tabName = "lsc"
        )
      )
    )
  ),
  
  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        a(
          img(
            src = "https://raw.githubusercontent.com/darwin-eu/DrugUtilisation/main/man/figures/DrugUtilisation.png", 
            align = "right", width = "200px"
          ),
          href = "https://github.com/darwin-eu/DrugUtilisation/",
          target = "_blank"
        ),
        h3("DrugUtilisation"),
        h5("DrugUtilisation is an R package that contains functions to conduct Patient level and Population level Drug Utilisation Studies in the OMOP common data model."),
        h5("In this study the main functionalities of the package are shown in a 'demonstration' study."),
        h5("The study shows: ... simvarstatin."),
        h5("The study was conducted in IQVIA Pharmetrics for academics (R).")
      ),
      # cdm snapshot ------
      tabItem(
        tabName = "cdm_snapshot",
        h4("Information about the databases that participated in the study"),
        selectors(cdmSnapshot, "cdm_snapshot", c("cdm_name")),
        tabsetPanel(
          type = "tabs",
          # tabPanel(
          #   "Raw table",
          #   downloadButton("cdm_snapshot_raw_download", "Download csv"),
          #   DT::dataTableOutput("cdm_snapshot_raw") %>% withSpinner()
          # ),
          tabPanel(
            "Tidy table",
            downloadButton("cdm_snapshot_tidy_download_word", "Download word"),
            downloadButton("cdm_snapshot_tidy_download_csv", "Download csv"),
            DTOutput("cdm_snapshot_tidy") %>% withSpinner()
          )
        )
      ),
      # cohort settings ------
      tabItem(
        tabName = "settings",
        h4("Settings for the study cohorts"),
        selectors(settings, "settings", c("cdm_name", "cohort_name")),
        tabsetPanel(
          type = "tabs",
          # tabPanel(
          #   "Raw table",
          #   downloadButton("settings_raw_download", "Download csv"),
          #   DT::dataTableOutput("settings_raw") %>% withSpinner()
          # ),
          tabPanel(
            "Tidy table",
            downloadButton("settings_tidy_download_csv", "Download csv"),
            downloadButton("settings_tidy_download_word", "Download word"),
            DTOutput("settings_tidy") %>% withSpinner()
          )
        )
      ),
      # cohort counts ------
      tabItem(
        tabName = "counts",
        h4("Counts for the study cohorts"),
        selectors(counts, "counts", c("cdm_name", "cohort_name")),
        pickerInput(
          inputId = "counts_mode",
          label = "Mode",
          choices = c("number_subjects", "number_records"),
          selected = "number_subjects",
          multiple = FALSE,
          inline = TRUE
        ),
        tabsetPanel(
          type = "tabs",
          # tabPanel(
          #   "Raw table",
          #   downloadButton("counts_raw_download", "Download csv"),
          #   DT::dataTableOutput("counts_raw") %>% withSpinner()
          # ),
          tabPanel(
            "Tidy table",
            downloadButton("counts_tidy_download_csv", "Download csv"),
            downloadButton("counts_tidy_download_word", "Download word"),
            DT::dataTableOutput("counts_tidy") %>% withSpinner()
          )
        )
      ),
      # cohort attrition ------
      tabItem(
        tabName = "attrition",
        h4("Attrition for the study cohorts"),
        selectors(attrition, "attrition", c("cdm_name", "cohort_name"), multiple = FALSE),
        tabsetPanel(
          type = "tabs",
          # tabPanel(
          #   "Raw table",
          #   downloadButton("attrition_raw_download", "Download csv"),
          #   DT::dataTableOutput("attrition_raw") %>% withSpinner()
          # ),
          tabPanel(
            "Tidy table",
            downloadButton("attrition_tidy_download_csv", "Download csv"),
            DT::dataTableOutput("attrition_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Diagram",
            downloadButton("attrition_diagram_download_png", "Download png"),
            grVizOutput("attrition_diagram", width = "400px", height = "100%") %>% withSpinner()
          )
        )
      ),
      # incidence ----
      tabItem(
        tabName = "incidence",
        h4("Incidence of simvarstatin"),
        selectors(incidence, "incidence", c("cdm_name", "outcome_cohort_name", "analysis_outcome_washout", "denominator_age_group", "denominator_sex"), multiple = TRUE),
        selectors(incidence, "incidence", c("analysis_interval"), multiple = FALSE),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw table",
            downloadButton("incidence_raw_download", "Download csv"),
            DT::dataTableOutput("incidence_raw") %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            downloadButton("incidence_plot_download_png", "Download png"),
            plotlyOutput("incidence_plot") %>% withSpinner()
          )
        )
      ),
      # prevalence ----
      tabItem(
        tabName = "prevalence",
        h4("Prevalence of simvarstatin"),
        selectors(prevalence, "prevalence", c("cdm_name", "outcome_cohort_name", "denominator_age_group", "denominator_sex"), multiple = TRUE),
        selectors(prevalence, "prevalence", c("analysis_interval"), multiple = FALSE),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw table",
            downloadButton("prevalence_raw_download", "Download csv"),
            DT::dataTableOutput("prevalence_raw") %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            downloadButton("prevalence_plot_download_png", "Download png"),
            plotlyOutput("prevalence_plot") %>% withSpinner()
          )
        )
      ),
      # indication ----
      tabItem(
        tabName = "indication",
        h4("Indication for new users of simvastatin"),
        selectors(
          data = indication, 
          prefix = "indication", 
          columns = c("cdm_name", "cohort_name", "age_group", "sex", "variable", "estimate_type"),
          multiple = TRUE, 
          default = list(age_group = "0 to 150", sex = "Both")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw table",
            downloadButton("indication_raw_download", "Download csv"),
            DT::dataTableOutput("indication_raw") %>% withSpinner()
          ),
          tabPanel(
            "Tidy table",
            downloadButton("indication_tidy_download_word", "Download word"),
            gt_output("indication_tidy") %>% withSpinner()
          )
        )
      ),
      # dose ----
      tabItem(
        tabName = "dose",
        h4("Dose use for new users of simvastatin"),
        selectors(
          data = dose, 
          prefix = "dose", 
          columns = c("cdm_name", "cohort_name", "age_group", "sex", "variable", "estimate_type"),
          multiple = TRUE, 
          default = list(age_group = "0 to 150", sex = "Both", estimate_type = c("median", "q25", "q75"))
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw table",
            downloadButton("dose_raw_download", "Download csv"),
            DT::dataTableOutput("dose_raw") %>% withSpinner()
          ),
          tabPanel(
            "Tidy table",
            downloadButton("dose_tidy_download_word", "Download word"),
            gt_output("dose_tidy") %>% withSpinner()
          )
        )
      ),
      # discontinuation ----
      tabItem(
        tabName = "discontinuation",
        h4("Treatment discontinuation new users of simvastatin"),
        selectors(discontinuation, "discontinuation", c("cdm_name", "Cohort", "age_group", "sex"), default = list(age_group = "0 to 150", sex = "Both")),
        selectors(discontinuation, "discontinuation", "variable", multiple = FALSE),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw table",
            downloadButton("discontinuation_raw_download", "Download csv"),
            DT::dataTableOutput("discontinuation_raw") %>% withSpinner()
          ),
          tabPanel(
            "Tidy table",
            downloadButton("discontinuation_tidy_download", "Download csv"),
            DT::dataTableOutput("discontinuation_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            downloadButton("discontinuation_plot_download", "Download png"),
            plotlyOutput("discontinuation_plot") %>% withSpinner()
          )
        )
      ),
      # patterns ----
      tabItem(
        tabName = "patterns",
        h4("Treatment patterns of new users of simvastatin"),
        selectors(
          data = patterns,
          prefix = "patterns",
          columns = c("cdm_name", "cohort_name", "age_group", "sex", "variable", "variable_level"),
          default = list(age_group = "0 to 150", sex = "Both")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw table",
            downloadButton("patterns_raw_download", "Download csv"),
            DT::dataTableOutput("patterns_raw") %>% withSpinner()
          ),
          tabPanel(
            "Tidy table",
            downloadButton("patterns_tidy_download_word", "Download word"),
            gt_output("patterns_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            downloadButton("patterns_plot_download", "Download png"),
            plotlyOutput("patterns_plot") %>% withSpinner()
          )
        )
      ),
      # characteristics ----
      tabItem(
        tabName = "characteristics",
        h4("Characteristics of new users of simvastatin"),
        selectors(
          data = characteristics,
          prefix = "characteristics",
          columns = c("cdm_name", "cohort_name", "age_group", "sex", "variable", "estimate_type"),
          default = list(age_group = "0 to 150", sex = "Both", estimate_type = c("count", "percentage", "median", "q25", "q75"))
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw table",
            downloadButton("characteristics_raw_download", "Download csv"),
            DT::dataTableOutput("characteristics_raw") %>% withSpinner()
          ),
          tabPanel(
            "Tidy table",
            downloadButton("characteristics_tidy_download_word", "Download word"),
            gt_output("characteristics_tidy") %>% withSpinner()
          )
        )
      ),
      # lsc ----
      tabItem(
        tabName = "lsc",
        h4("Large scale characteristics of new users of simvastatin"),
        selectors(
          data = lsc,
          prefix = "lsc",
          columns = c("cdm_name", "cohort_name", "age_group", "sex", "table_name", "window"),
          default = list(age_group = "0 to 150", sex = "Both")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw data",
            downloadButton("lsc_raw_download", "Download csv"),
            DT::dataTableOutput("lsc_raw") %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            uiOutput("lsc_ui"),
            plotlyOutput("lsc_compare") %>% withSpinner()
          )
        )
      )
      # end ----
    )
  )
)


server <- function(input, output, session) {
  # cdm snapshot ----
  getSnapshot <- reactive({
    filterData(cdmSnapshot, "cdm_snapshot", input)
  })
  output$cdm_snapshot_tidy <- renderDataTable({
    datatable(
      getSnapshot(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  output$cdm_snapshot_tidy_download_word <- downloadHandler(
    filename = "cdm_snapshot.docx",
    content = function(file) {
      gt(getSnapshot()) %>% gtsave(filename = file)
    }
  )
  output$cdm_snapshot_tidy_download_csv <- downloadHandler(
    filename = "cdm_snapshot.csv",
    content = function(file) {
      write_csv(getSnapshot(), file = file)
    }
  )
  # cohort settings ----
  getSettings <- reactive({
    filterData(settings, "settings", input)
  })
  output$settings_tidy_download_csv <- downloadHandler(
    filename = "cohort_settings.csv",
    content = function(file) {
      write_csv(getSettings(), file = file)
    }
  )
  output$settings_tidy_download_word <- downloadHandler(
    filename = "cohort_settings.docx",
    content = function(file) {
      gt(getSettings()) %>% gtsave(filename = file)
    }
  )
  output$settings_tidy <- renderDataTable({
    datatable(
      getSettings(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  # cohort count ----
  getCounts <- reactive({
    filterData(counts, "counts", input) %>%
      rename("count" = dplyr::all_of(input$counts_mode)) %>%
      select("cohort_name", "cdm_name", "count") %>%
      pivot_wider(names_from = "cdm_name", values_from = "count")
  })
  output$counts_tidy_download_csv <- downloadHandler(
    filename = "cohort_counts.csv",
    content = function(file) {
      write_csv(getCounts(), file = file)
    }
  )
  output$counts_tidy_download_word <- downloadHandler(
    filename = "cohort_counts.docx",
    content = function(file) {
      gt(getCounts()) %>% gtsave(filename = file)
    }
  )
  output$counts_tidy <- renderDataTable({
    datatable(
      getCounts(),
      rownames = FALSE,
      options = list(dom = 't')
    )
  })
  # cohort attrition ----
  getAttrition <- reactive({
    filterData(attrition, "attrition", input) %>%
      select(-c("cdm_name", "cohort_name")) %>%
      arrange(reason_id)
  })
  output$attrition_tidy_download_csv <- downloadHandler(
    filename = "cohort_attrition.csv",
    content = function(file) {
      write_csv(getAttrition(), file = file)
    }
  )
  output$attrition_tidy <- renderDataTable({
    datatable(
      getAttrition(),
      rownames = FALSE,
      options = list(dom = 't', pageLength = -1, ordering = F)
    )
  })
  output$attrition_diagram_download_png <- downloadHandler(
    filename = "cohort_attrition.png",
    content = function(file) {
      #render_graph(attritionChart(table))
      export_graph(
        graph = attritionChart(getAttrition()),
        file_name = file,
        file_type = "png",
        width = 800
      )
    }
  )
  output$attrition_diagram <- renderGrViz({
    render_graph(attritionChart(getAttrition()))
  })
  # incidence ----
  getIncidence <- reactive({
    filterData(incidence, "incidence", input)
  })
  output$incidence_raw <- renderDataTable({
    datatable(getIncidence(), options = list(scrollX = TRUE))
  })
  output$incidence_plot <- renderPlotly({
    inc <- getIncidence()
    class(inc) <- c("IncidencePrevalenceResult", "IncidenceResult", class(inc))
    plotIncidence(result = inc, colour = c("denominator_age_group", "denominator_sex"))
  })
  output$incidence_plot_download_png <- downloadHandler(
    filename = "incidence.png",
    content = function(file) {
      inc <- getIncidence()
      class(inc) <- c("IncidencePrevalenceResult", "IncidenceResult", class(inc))
      p <- plotIncidence(result = inc, colour = c("denominator_age_group", "denominator_sex"))
      ggsave(filename = file, plot = p)
    }
  )
  # prevalence ----
  getPrevalence <- reactive({
    filterData(prevalence, "prevalence", input)
  })
  output$prevalence_raw <- renderDataTable({
    datatable(getPrevalence(), options = list(scrollX = TRUE))
  })
  output$prevalence_plot <- renderPlotly({
    prev <- getPrevalence()
    class(prev) <- c("IncidencePrevalenceResult", "IncidenceResult", class(prev))
    plotPrevalence(result = prev, colour = c("denominator_age_group", "denominator_sex"))
  })
  output$prevalence_plot_download_png <- downloadHandler(
    filename = "prevalence.png",
    content = function(file) {
      prev <- getPrevalence()
      class(prev) <- c("IncidencePrevalenceResult", "PrevalenceResult", class(prev))
      p <- plotPrevalence(result = prev, colour = c("denominator_age_group", "denominator_sex"))
      ggsave(filename = file, plot = p)
    }
  )
  # indication ----
  getIndication <- reactive({
    filterData(indication, "indication", input)
  })
  output$indication_raw <- renderDataTable({
    datatable(getIndication())
  })
  output$indication_raw_download <- downloadHandler(
    filename = "indication.csv", 
    content = function(file) {
      write_csv(getIndication(), file = file)
    }
  )
  gtIndication <- reactive({
    gtResult(
      summarisedResult = getIndication(), 
      wide = list(
        "CDM name" = c(level = "cdm_name"), 
        "Age group" = c(level = "age_group"),
        "Sex" = c(level = "sex")
      ), 
      long = list(
        "Indication_window" = c(level = "variable"), 
        "Indication_name" = c(level = "variable_level"), 
        "Format" = c(level = "format")
      )
    )
  })
  output$indication_tidy <- render_gt({
    gtIndication()
  })
  output$indication_tidy_download_word <- downloadHandler(
    filename = "indication.docx", 
    content = function(file) {
      gtsave(gtIndication(), file = file)
    }
  )
  # dose ----
  getDose <- reactive({
    filterData(dose, "dose", input)
  })
  output$dose_raw <- renderDataTable({
    datatable(getDose())
  })
  output$dose_raw_download <- downloadHandler(
    filename = "dose.csv", 
    content = function(file) {
      write_csv(getDose(), file = file)
    }
  )
  gtDose <- reactive({
    gtResult(
      summarisedResult = getDose(), 
      wide = list(
        "CDM name" = c(level = "cdm_name"), 
        "Age group" = c(level = "age_group"),
        "Sex" = c(level = "sex")
      ), 
      long = list(
        "Variable" = c(level = "variable"), 
        "Format" = c(level = "format")
      )
    )
  })
  output$dose_tidy <- render_gt({
    gtDose()
  })
  output$dose_tidy_download_word <- downloadHandler(
    filename = "dose.docx", 
    content = function(file) {
      gtsave(gtDose(), file = file)
    }
  )
  # discontinuation ----
  getDiscontinuation <- reactive({
    filterData(discontinuation, "discontinuation", input)
  })
  output$discontinuation_raw <- renderDataTable({
    datatable(getDiscontinuation())
  })
  output$discontinuation_raw_download <- downloadHandler(
    filename = "discontinuation.csv", 
    content = function(file) {
      write_csv(getDiscontinuation(), file = file)
    }
  )
  output$discontinuation_tidy <- renderDataTable({
    getDiscontinuation() %>%
      pivot_wider(names_from = "estimate_type", values_from = "estimate") %>%
      mutate(estimate = paste0(estimate, " [", estimate_95CI_lower, "; ", estimate_95CI_upper, "]")) %>%
      select(-c(estimate_95CI_upper, estimate_95CI_lower)) %>%
      datatable()
  })
  output$discontinuation_tidy_download <- downloadHandler(
    filename = "discontinuation.csv", 
    content = function(file) {
      getDiscontinuation() %>%
        pivot_wider(names_from = "estimate_type", values_from = "estimate") %>%
        mutate(estimate = paste0(estimate, " [", estimate_95CI_lower, "; ", estimate_95CI_upper, "]")) %>%
        select(-c(estimate_95CI_upper, estimate_95CI_lower)) %>%
        write_csv(file = file)
    }
  )
  discontinuationPlot <- reactive({
    getDiscontinuation() %>%
      pivot_wider(names_from = "estimate_type", values_from = "estimate") %>%
      mutate(label = paste0(cdm_name, "; ", age_group, "; ", sex)) %>%
      ggplot(aes(x = time, y = estimate, col = label)) +
      geom_line() +
      geom_line(aes(y = estimate_95CI_lower), show.legend = FALSE, linetype = "dashed", linewidth = .2) +
      geom_line(aes(y = estimate_95CI_upper), show.legend = FALSE, linetype = "dashed", linewidth = .2) +
      labs(y = input$discontinuation_variable)
  })
  output$discontinuation_plot <- renderPlotly({
    discontinuationPlot()
  })
  output$discontinuation_plot_download <- downloadHandler(
    filename = "discontinuation.png", 
    content = function(file) {
       ggsave(filename = file, plot = discontinuationPlot())
    }
  )
  # patterns ----
  getPatterns <- reactive({
    filterData(patterns, "patterns", input)
  })
  output$patterns_raw <- renderDataTable({
    datatable(getPatterns())
  })
  output$patterns_raw_download <- downloadHandler(
    filename = "patterns.csv", 
    content = function(file) {
      write_csv(getPatterns(), file = file)
    }
  )
  gtPatterns <- reactive({
    gtResult(
      summarisedResult = getPatterns(), 
      wide = list(
        "CDM name" = c(level = "cdm_name"), 
        "Age group" = c(level = "age_group"),
        "Sex" = c(level = "sex"),
        "Window" = c(level = "variable_level")
      ), 
      long = list(
        "Treatment" = c(level = "variable"), 
        "Format" = c(level = "format")
      )
    )
  })
  output$patterns_tidy <- render_gt({
    gtPatterns()
  })
  output$patterns_tidy_download_word <- downloadHandler(
    filename = "patterns.docx", 
    content = function(file) {
      gtsave(gtPatterns(), filename = file)
    }
  )
  plotPatterns <- reactive({
    getPatterns() %>%
      filter(estimate_type == "percentage") %>%
      mutate(estimate = as.numeric(estimate)) %>%
      rename("window" = "variable_level") %>%
      ggplot(aes(y = estimate, x = variable, fill = variable)) + 
      geom_col(width = 0.7) +
      coord_flip() +
      facet_grid(cdm_name + cohort_name + age_group + sex ~ window)
      #scale_y_continuous(name = "percentage (%)", breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100"), limits = c(0.1, 100), trans = "log10")
  })
  output$patterns_plot <- renderPlotly({
    plotPatterns()
  })
  output$patterns_plot_download <- downloadHandler(
    filename = "patterns.png", 
    content = function(file) {
      ggsave(filename = file, plot = plotPatterns())
    }
  )
  # characteristics ----
  getCharacteristics <- reactive({
    filterData(characteristics, "characteristics", input)
  })
  output$characteristics_raw <- renderDataTable({
    datatable(getCharacteristics(), options = list(scrollX = TRUE))
  })
  output$characteristics_raw_download <- downloadHandler(
    filename = "characteristics.csv", 
    content = function(file) {
      write_csv(getCharacteristics(), file = file)
    }
  )
  gtCharacteristics <- reactive({
    gtResult(
      summarisedResult = getCharacteristics(), 
      wide = list(
        "CDM name" = c(level = "cdm_name"), 
        "Age group" = c(level = "age_group"),
        "Sex" = c(level = "sex")
      ), 
      long = list(
        "Variable" = c(level = "variable"),
        "Level" = c(level = "variable_level"),
        "Format" = c(level = "format")
      )
    )
  })
  output$characteristics_tidy <- render_gt({
    gtCharacteristics()
  })
  output$characteristics_tidy_download_word <- downloadHandler(
    filename = "characteristics.docx", 
    content = function(file) {
      gtsave(gtCharacteristics(), filename = file)
    }
  )
  # lsc ----
  getLsc <- reactive({
    filterData(lsc, "lsc", input)
  })
  getLscWithName <- reactive({
    getLsc() %>%
      mutate(
        name = paste0(cdm_name, "; ", cohort_name, "; ", age_group, "; ", sex, "; ", window),
        concept = paste0(concept_name, " (", concept, ")")
      ) %>%
      select(name, concept, percentage)
  })
  possibilitiesLsc <- reactive({
    getLscWithName() %>%
      pull(name) %>%
      unique()
  })
  output$lsc_ui <- renderUI({
    x <- possibilitiesLsc()
    pickerInput("lsc_ref", "Reference", choices = x, selected = x[1], multiple = FALSE, inline = TRUE)
  })
  output$lsc_raw <- renderDataTable({
    x <- getLsc()
    x %>%
      pivot_wider(names_from = "window", values_from = "percentage") %>%
      datatable(options = list(scrollX = TRUE)) %>%
      formatPercentage(columns = unique(x$window))
  })
  output$lsc_raw_download <- downloadHandler(filename = "lsc.csv", content = function(file) {
    x %>%
      pivot_wider(names_from = "window", values_from = "percentage") %>%
      write_csv(file = file)
  })
  output$lsc_compare <- renderPlotly({
    pos <- possibilitiesLsc()
    validate(need(length(pos) > 1, "At least two set of results must be selected"))
    ref <- input$lsc_ref
    validate(need(length(ref) == 1, "At least two set of results must be selected"))
    x <- getLscWithName()
    x <- x %>%
      filter(name == ref) %>%
      rename(reference = percentage) %>%
      select(-"name") %>%
      right_join(x %>% filter(name != ref), by = "concept") %>%
      mutate(percentage = 100*coalesce(percentage, 0)) %>%
      mutate(reference = 100*coalesce(reference, 0))
    ggplot(data = x, aes(x = reference, y = percentage, col = name, group = concept)) +
      geom_point()
    
  })
  # end ----
}

shinyApp(ui = ui, server = server)
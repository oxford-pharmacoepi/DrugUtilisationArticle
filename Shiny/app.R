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
          text = "Treatment patterns",
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
      )
      # # incidence ----
      # # prevalence ----
      # # indication ----
      # tabItem(
      #   tabName = "indication",
      #   h4("Indication for new users of simvastatin"),
      #   selectors(indication, "indication", c("cdm_name", "cohort_name", "variable")),
      #   tabsetPanel(
      #     type = "tabs",
      #     tabPanel(
      #       "Raw table",
      #       downloadButton("indication_raw_download", "Download csv"),
      #       DT::dataTableOutput("indication_raw") %>% withSpinner()
      #     ),
      #     tabPanel(
      #       "Tidy table",
      #       downloadButton("indication_tidy_download_csv", "Download csv"),
      #       #downloadButton("indication_tidy_download_word", "Download word"),
      #       DT::dataTableOutput("indication_tidy") %>% withSpinner()
      #     )
      #   )
      # ), 
      # # dose ----
      # tabItem(
      #   tabName = "dose",
      #   h4("Dose use for new users of simvastatin"),
      #   selectors(dose, "dose", c("cdm_name", "cohort_name", "variable", "estimate_type")),
      #   tabsetPanel(
      #     type = "tabs",
      #     tabPanel(
      #       "Raw table",
      #       downloadButton("indication_raw_download", "Download csv"),
      #       DT::dataTableOutput("indication_raw") %>% withSpinner()
      #     ),
      #     tabPanel(
      #       "Tidy table",
      #       downloadButton("indication_tidy_download_csv", "Download csv"),
      #       #downloadButton("indication_tidy_download_word", "Download word"),
      #       DT::dataTableOutput("indication_tidy") %>% withSpinner()
      #     )
      #   )
      # ),
      # # discontinuation ----
      # tabItem(
      #   tabName = "discontinuation",
      #   h4("Treatment discontinuation new users of simvastatin"),
      #   selectors(discontinuation, "discontinuation", c("cdm_name", "Cohort")),
      #   tabsetPanel(
      #     type = "tabs",
      #     tabPanel(
      #       "Raw table",
      #       downloadButton("discontinuation_raw_download", "Download csv"),
      #       DT::dataTableOutput("discontinuation_raw") %>% withSpinner()
      #     ),
      #     tabPanel(
      #       "Tidy table",
      #       downloadButton("discontinuation_tidy_download_csv", "Download csv"),
      #       #downloadButton("discontinuation_tidy_download_word", "Download word"),
      #       DT::dataTableOutput("discontinuation_tidy") %>% withSpinner()
      #     ),
      #     tabPanel(
      #       "Plot",
      #       downloadButton("discontinuation_plot_download", "Download png"),
      #       plotlyOutput("discontinuation_plot") %>% withSpinner()
      #     )
      #   )
      # ),
      # # patterns ----
      # tabItem(
      #   tabName = "patterns",
      #   h4("Treatment patterns of new users of simvastatin"),
      #   selectors(patterns, "patterns", c("cdm_name", "cohort_name", "variable_level", "variable")),
      #   tabsetPanel(
      #     type = "tabs",
      #     tabPanel(
      #       "Raw table",
      #       downloadButton("patterns_raw_download", "Download csv"),
      #       DT::dataTableOutput("patterns_raw") %>% withSpinner()
      #     ),
      #     tabPanel(
      #       "Tidy table",
      #       downloadButton("patterns_tidy_download_csv", "Download csv"),
      #       #downloadButton("discontinuation_tidy_download_word", "Download word"),
      #       DT::dataTableOutput("discontinuation_tidy") %>% withSpinner()
      #     ),
      #     tabPanel(
      #       "Plot",
      #       downloadButton("discontinuation_plot_download", "Download png"),
      #       plotlyOutput("discontinuation_plot") %>% withSpinner()
      #     )
      #   )
      # )
      # # characteristics ----
      # # lsc ----
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
    filterData(attrition, "counts", input) %>%
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
  # end ----
}

shinyApp(ui = ui, server = server)
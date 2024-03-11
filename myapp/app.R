library(shiny)
library(shinyjs)
library(scales)
library(dplyr)
library(formattable)
library(shinythemes)
library(shinydashboard)

# Obtenir la date du jour
# date_du_jour <- "2024-03-09"

# Créer le nom du fichier avec la date du jour incluse
# nom_du_fichier3 <- paste0("predictshiny_", date_du_jour, ".rds")
# nom_du_fichier3 <- paste0("predictshiny_", date_du_jour, ".csv")

# data <- readRDS(nom_du_fichier3)
# data <- read.csv2(nom_du_fichier3)


# Sauvegarder df_back avec le nom de fichier dynamique
#
# write.csv2(df_pred, file = nom_du_fichier3, row.names = F)


# Sauvegarder df_back avec le nom de fichier dynamique
# df_pred <- read.csv2(nom_du_fichier3)

ui <- fluidPage(
  tags$head(
    HTML(
      "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
    )
  ),
  useShinyjs(),
  navbarPage(
    theme = shinytheme('cerulean'),

    "PMU",

    tabPanel("Graphiques",
             fluidRow(
               div(id = "Sidebar", sidebarPanel(width = 12,
                                                fluidRow(
                                                  fileInput("file1", "Charger le .csv",
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,
                                                                       .csv"))
                                                ),
                                                fluidRow(textOutput("keepAlive")),

                                                fluidRow(
                                                  column(4, uiOutput('hipp_id_graph')),
                                                  column(6, uiOutput('course_filter_ui_graph'))),
                                                fluidRow(textOutput("confiance")))
               ),
               mainPanel(#width = "100%",
                             fluidRow(formattableOutput(outputId = "mytable"))
                         )))

  )
)


# Définir le serveur Shiny
server <- function(input, output, session) {

  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })

  data <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv2(infile$datapath, header = TRUE)
  })

  output$hipp_id_graph <- renderUI({
    req(data())
    data <- data()
    hipp = unique(data$R_name)
    selectInput('hipp_filter_id_graph', 'Réunion', hipp)
  })

  output$course_filter_ui_graph <- renderUI({
    req(data())
    data <- data()
    selected_hippodrome <- input$hipp_filter_id_graph
    courses <- sort(unique(filter(data, R_name == selected_hippodrome)$C_number))
    selectInput("course_filter_graph", "Choisissez une course", choices = courses)
  })

  # Fonction de filtrage des données
  filtered_data <- reactive({
    req(data())
    filter(data(), R_name == input$hipp_filter_id_graph, C_number == input$course_filter_graph) %>%
      mutate(across(where(is.numeric), round, 2))
  })

  # Réaction pour mettre à jour les données filtrées
  observeEvent(input$update, {
    req(data())
    filtered_data()
  })

  # Mise à jour des choix pour C_number basé sur R_name sélectionné

  output$confiance <- renderText({
    req(data())
    filtered <- filtered_data()

    if (nrow(filtered) == 0) {
      return(NULL)
    }

    paste0('Indice de confiance : ', unique(filtered$IndiceConfianceCourse))
  })


  output$mytable <- renderFormattable({
    req(data())
    filtered <- filtered_data()

    if (nrow(filtered) == 0) {
      return(NULL)
    }

    data <-  filtered %>%
      select(saddle, Success_Rate_Jockey, Success_Rate_Trainer, Success_Rate_Smiley, IndiceConfianceCheval,
                        Percent_Dai, BRUT_min, CORRIGE_min,
                         CORDE_min, PISTE_min, DEF_min, AUTO_min, DISC_min,
                         BRUT_moy, CORRIGE_moy, CORDE_moy, PISTE_moy,
                         DEF_moy, AUTO_moy, DISC_moy)

    formattable(data,
                 list(
                   Success_Rate_Jockey = color_bar("pink"),
                   Success_Rate_Trainer = color_bar("lightblue")
                 ))
      })

  # output$table2 <- renderDataTable(filtered_data())

}

# Lancer l'application Shiny
shinyApp(ui, server)

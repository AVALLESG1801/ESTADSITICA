# Librerías necesarias
library(shiny)
library(lavaan)
library(readxl)
library(readr)
library(DT)
library(shinydashboard)
library(semPlot)
library(openxlsx)

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Análisis Factorial Confirmatorio"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cargar Datos", tabName = "datos", icon = icon("upload")),
      menuItem("Especificar Modelo", tabName = "modelo", icon = icon("code")),
      menuItem("Resultados", tabName = "resultados", icon = icon("chart-bar")),
      menuItem("Diagrama de Paths", tabName = "path", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "datos",
              fluidRow(
                box(title = "Cargar archivo", status = "primary", solidHeader = TRUE, width = 12,
                    fileInput("archivo", "Selecciona archivo Excel o CSV",
                              accept = c(".xlsx", ".xls", ".csv")),
                    conditionalPanel(
                      condition = "output.archivo_cargado",
                      h4("Vista previa de los datos:"),
                      DT::dataTableOutput("vista_datos")
                    )
                )
              )
      ),
      
      tabItem(tabName = "modelo",
              fluidRow(
                box(title = "Especificación del modelo", status = "primary", solidHeader = TRUE, width = 12,
                    h4("Especifica tu modelo AFC usando sintaxis lavaan:"),
                    p("Ejemplo: Factor1 =~ item1 + item2 + item3"),
                    textAreaInput("modelo_texto", "Modelo AFC:", rows = 10),
                    selectInput("estimador", "Selecciona el estimador:",
                                choices = c("ML", "WLSMV", "GLS"), selected = "ML"),
                    actionButton("ejecutar_afc", "Ejecutar AFC", class = "btn-primary"),
                    actionButton("limpiar", "Limpiar Todo", class = "btn-danger"),
                    br(), br(),
                    conditionalPanel(
                      condition = "output.modelo_ejecutado",
                      h4("Variables disponibles en tus datos:"),
                      verbatimTextOutput("variables_disponibles")
                    )
                )
              )
      ),
      
      tabItem(tabName = "resultados",
              fluidRow(
                box(title = "Índices de Ajuste", status = "success", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("indices_ajuste")
                ),
                box(title = "Resumen del Modelo", status = "info", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("resumen_modelo")
                )
              ),
              
              fluidRow(
                box(title = "Cargas Factoriales", status = "warning", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("cargas_factoriales")
                )
              ),
              
              fluidRow(
                box(title = "Descargar Resultados", status = "primary", solidHeader = TRUE, width = 12,
                    downloadButton("descargar_resultados", "Descargar .TXT", class = "btn-success"),
                    downloadButton("descargar_excel", "Descargar Excel", class = "btn-info"),
                    downloadButton("descargar_pdf", "Descargar PDF Diagrama", class = "btn-warning")
                )
              )
      ),
      
      tabItem(tabName = "path",
              fluidRow(
                box(title = "Diagrama del Modelo", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("path_diagram")
                )
              )
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  datos <- reactiveVal(NULL)
  modelo_fit <- reactiveVal(NULL)
  
  # Cargar datos
  observeEvent(input$archivo, {
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$datapath)
    
    if(ext == "csv"){
      df <- read_csv(input$archivo$datapath)
    } else if(ext %in% c("xlsx","xls")){
      df <- read_excel(input$archivo$datapath)
    }
    datos(df)
  })
  
  output$archivo_cargado <- reactive({
    !is.null(datos())
  })
  outputOptions(output, "archivo_cargado", suspendWhenHidden = FALSE)
  
  output$vista_datos <- DT::renderDataTable({
    req(datos())
    DT::datatable(datos(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$variables_disponibles <- renderText({
    req(datos())
    paste("Variables:", paste(names(datos()), collapse = ", "))
  })
  
  observeEvent(input$ejecutar_afc, {
    req(datos(), input$modelo_texto)
    
    tryCatch({
      fit <- cfa(input$modelo_texto, data = datos(), estimator = input$estimador)
      modelo_fit(fit)
      showNotification("AFC ejecutado exitosamente!", type = "message")
    }, error = function(e){
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$modelo_ejecutado <- reactive({
    !is.null(modelo_fit())
  })
  outputOptions(output, "modelo_ejecutado", suspendWhenHidden = FALSE)
  
  output$indices_ajuste <- DT::renderDataTable({
    req(modelo_fit())
    fit_measures <- fitMeasures(modelo_fit())
    indices <- data.frame(
      Índice = c("CFI", "TLI", "RMSEA", "SRMR", "Chi-cuadrado", "df", "p-value"),
      Valor = c(
        round(fit_measures["cfi"],3),
        round(fit_measures["tli"],3),
        round(fit_measures["rmsea"],3),
        round(fit_measures["srmr"],3),
        round(fit_measures["chisq"],3),
        fit_measures["df"],
        round(fit_measures["pvalue"],3)
      ),
      Interpretación = c(
        ifelse(fit_measures["cfi"] > 0.95, "Excelente", ifelse(fit_measures["cfi"] > 0.90, "Aceptable", "Pobre")),
        ifelse(fit_measures["tli"] > 0.95, "Excelente", ifelse(fit_measures["tli"] > 0.90, "Aceptable", "Pobre")),
        ifelse(fit_measures["rmsea"] < 0.05, "Excelente", ifelse(fit_measures["rmsea"] < 0.08, "Aceptable", "Pobre")),
        ifelse(fit_measures["srmr"] < 0.05, "Excelente", ifelse(fit_measures["srmr"] < 0.08, "Aceptable", "Pobre")),
        "-", "-", ifelse(fit_measures["pvalue"] > 0.05, "No significativo (bueno)", "Significativo (revisar modelo)")
      )
    )
    DT::datatable(indices, options = list(dom = 't'))
  })
  
  output$resumen_modelo <- renderText({
    req(modelo_fit())
    capture.output(summary(modelo_fit(), fit.measures = TRUE, standardized = TRUE))
  })
  
  output$cargas_factoriales <- DT::renderDataTable({
    req(modelo_fit())
    params <- parameterEstimates(modelo_fit())
    cargas <- params[params$op == "=~",]
    DT::datatable(cargas[,c("lhs","rhs","est","se","z","pvalue")],
                  options = list(pageLength=15))
  })
  
  output$path_diagram <- renderPlot({
    req(modelo_fit())
    semPaths(modelo_fit(), whatLabels = "std", layout = "tree", sizeMan = 6)
  })
  
  output$descargar_resultados <- downloadHandler(
    filename = function() { paste("resultados_AFC_", Sys.Date(), ".txt", sep="") },
    content = function(file){
      sink(file)
      cat("RESULTADOS DEL ANÁLISIS FACTORIAL CONFIRMATORIO\n")
      cat(paste(rep("=", 50), collapse=""), "\n\n")
      cat("ÍNDICES DE AJUSTE:\n")
      print(fitMeasures(modelo_fit()))
      cat("\n\nRESUMEN DEL MODELO:\n")
      print(summary(modelo_fit(), fit.measures = TRUE, standardized = TRUE))
      sink()
    }
  )
  
  output$descargar_excel <- downloadHandler(
    filename = function() { paste("resultados_AFC_", Sys.Date(), ".xlsx", sep = "") },
    content = function(file){
      wb <- createWorkbook()
      addWorksheet(wb, "Indices Ajuste")
      writeData(wb, "Indices Ajuste", fitMeasures(modelo_fit()))
      addWorksheet(wb, "Resumen Modelo")
      writeData(wb, "Resumen Modelo", capture.output(summary(modelo_fit(), fit.measures = TRUE, standardized = TRUE)))
      saveWorkbook(wb, file)
    }
  )
  
  output$descargar_pdf <- downloadHandler(
    filename = function() { paste("AFC_Diagrama_", Sys.Date(), ".pdf", sep = "") },
    content = function(file){
      pdf(file)
      semPaths(modelo_fit(), whatLabels = "std", layout = "tree", sizeMan = 6)
      dev.off()
    }
  )
  
  observeEvent(input$limpiar, {
    datos(NULL)
    modelo_fit(NULL)
    updateTextAreaInput(session, "modelo_texto", value = "")
    showNotification("Todo ha sido limpiado.", type = "message")
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)

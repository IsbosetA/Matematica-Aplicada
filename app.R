library(shiny)

# Función para calcular la mejor decisión considerando los factores, tiempos y beneficios
calcular_mejor_decision <- function(decisiones) {
  resultados <- sapply(decisiones, function(decision) {
    corto_plazo <- sum(unlist(decision$corto_valor))  # Sumar los valores a corto plazo
    mediano_plazo <- sum(unlist(decision$mediano_valor)) * 30  # Convertir meses a días
    largo_plazo <- sum(unlist(decision$largo_valor)) * 365  # Convertir años a días
    beneficio <- decision$beneficio / 100  # Convertir el beneficio a un porcentaje
    
    # Aquí ponderamos cada valor y sumamos para obtener el resultado final ponderado
    resultado <- (corto_plazo + mediano_plazo + largo_plazo) * beneficio
    return(resultado)
  })
  mejor <- which.max(resultados)
  return(list(mejor = mejor, resultados = resultados))
}

# Definir UI
ui <- fluidPage(
  titlePanel("Facilitador de Toma de Decisiones"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_decisiones", "Número de decisiones:", value = 2, min = 2),
      actionButton("ingresar_decisiones", "Ingresar decisiones")
    ),
    mainPanel(
      uiOutput("decisiones_ui"),
      actionButton("calcular", "Calcular mejor decisión"),
      verbatimTextOutput("resultados"),
      plotOutput("grafico")
    )
  )
)

# Definir lógica del servidor
server <- function(input, output, session) {
  decisiones <- reactiveVal(list())
  
  observeEvent(input$ingresar_decisiones, {
    num_decisiones <- input$num_decisiones
    
    decisiones_ui <- fluidRow(
      lapply(1:num_decisiones, function(i) {
        wellPanel(
          textInput(paste("decision", i, sep = "_"), paste("Nombre de la Decisión", i)),
          numericInput(paste("beneficio", i, sep = "_"), "Beneficio estimado (%):", value = 0, min = 0, max = 100),
          numericInput(paste("num_factores", i, sep = "_"), "Número de factores:", value = 2, min = 1),
          uiOutput(paste("factores_ui", i, sep = "_"))
        )
      })
    )
    output$decisiones_ui <- renderUI({ decisiones_ui })
  })
  
  lapply(1:100, function(i) {
    observeEvent(input[[paste("num_factores", i, sep = "_")]], {
      num_factores <- input[[paste("num_factores", i, sep = "_")]]
      
      factores_ui <- fluidRow(
        lapply(1:num_factores, function(j) {
          tagList(
            selectInput(paste("factor_tipo", i, j, sep = "_"), paste("Factor", j, "es a corto, mediano o largo plazo?"),
                        choices = c("Corto plazo", "Mediano plazo", "Largo plazo")),
            numericInput(paste("factor_tiempo", i, j, sep = "_"), paste("Tiempo estimado para el Factor", j), value = 0),
            numericInput(paste("factor_valor", i, j, sep = "_"), paste("Valor del Factor", j), value = 0)
          )
        })
      )
      output[[paste("factores_ui", i, sep = "_")]] <- renderUI({ factores_ui })
    }, ignoreInit = TRUE)
  })
  
  observeEvent(input$calcular, {
    num_decisiones <- input$num_decisiones
    
    decisiones_list <- list()
    nombres_decisiones <- character(num_decisiones)
    
    for (i in 1:num_decisiones) {
      decision <- list(corto_valor = numeric(0), mediano_valor = numeric(0), largo_valor = numeric(0), beneficio = input[[paste("beneficio", i, sep = "_")]])
      num_factores <- input[[paste("num_factores", i, sep = "_")]]
      for (j in 1:num_factores) {
        tipo <- input[[paste("factor_tipo", i, j, sep = "_")]]
        tiempo <- input[[paste("factor_tiempo", i, j, sep = "_")]]
        valor <- input[[paste("factor_valor", i, j, sep = "_")]]
        
        if (tipo == "Corto plazo") {
          decision$corto_valor <- c(decision$corto_valor, valor)
          cat("Factor", j, "de la decisión", i, "es a corto plazo (días):", tiempo, "días\n")
        } else if (tipo == "Mediano plazo") {
          decision$mediano_valor <- c(decision$mediano_valor, valor)
          cat("Factor", j, "de la decisión", i, "es a mediano plazo (meses):", tiempo, "meses\n")
        } else if (tipo == "Largo plazo") {
          decision$largo_valor <- c(decision$largo_valor, valor)
          cat("Factor", j, "de la decisión", i, "es a largo plazo (años):", tiempo, "años\n")
        }
        
        decision[[paste("factor", j, "tipo", sep = "_")]] <- tipo
        decision[[paste("factor", j, "tiempo", sep = "_")]] <- tiempo
        decision[[paste("factor", j, "valor", sep = "_")]] <- valor
      }
      nombres_decisiones[i] <- input[[paste("decision", i, sep = "_")]]
      decisiones_list[[nombres_decisiones[i]]] <- decision
    }
    
    resultados <- calcular_mejor_decision(decisiones_list)
    
    output$resultados <- renderPrint({
      mejor <- nombres_decisiones[resultados$mejor]
      cat("La mejor decisión es:", mejor, "con un puntaje de", resultados$resultados[resultados$mejor])
    })
    
    output$grafico <- renderPlot({
      if (length(resultados$resultados) > 0 && all(is.finite(resultados$resultados))) {
        barplot(resultados$resultados, names.arg = nombres_decisiones,
                main = "Resultados de las decisiones", col = "lightblue",
                ylab = "Puntaje", xlab = "Decisiones")
        abline(h = resultados$resultados[resultados$mejor], col = "red", lwd = 2)
      } else {
        plot.new()
        text(0.5, 0.5, "No hay datos válidos para mostrar el gráfico.")
      }
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

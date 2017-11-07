##### App de Controle Financeiro ####


#### 1. Carrega Pacotes Necessários ####
list.of.packages <- c("shinydashboard", "shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=TRUE)

#Carrega fontes das comboboxes:
cbtipo <- read.table(file = "fontes/cb_tipo.csv", header = TRUE, sep = ";", blank.lines.skip = FALSE)
cbconta <- read.table(file = "fontes/cb_conta.csv", header = TRUE, sep = ";", blank.lines.skip = FALSE)
cbgrupo <- read.table(file = "fontes/cb_grupo.csv", header = TRUE, sep = ";", blank.lines.skip = FALSE)
cbsubgrupo <- read.table(file = "fontes/cb_subgrupo.csv", header = TRUE, sep = ";", blank.lines.skip = FALSE)

campos <- c("tipo", "conta", "grupo", "subgrupo", "data", "valor", "descricao")
epochTime <- function() {
  as.integer(Sys.time())
}



ui <- dashboardPage(
  dashboardHeader(title = "Controle Financeiro"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Execução Orçamentária", tabName = "execucao_orcamentaria", icon = icon("pie-chart")),
      menuItem("Lançamento", tabName = "lancamento", icon = icon("file-text-o"))
      )
    ),
  dashboardBody(
    tabItems(
      #conteúdo da 1ª página
      tabItem(tabName = "execucao_orcamentaria",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      #conteúdo da 2ª página
      tabItem(tabName = "lancamento",

              fluidRow(
                column(6,
                       div(
                         id = "form",
                         
                         selectInput("tipo", "Tipo:",
                                     as.character(cbtipo$combo)),
                         selectInput("conta", "Conta:",
                                     as.character(cbconta$combo)),
                         selectInput("grupo", "Grupo:",
                                     as.character(cbgrupo$combo)),
                         uiOutput("subgrupo_dep"),
                         dateInput("data", "Data:", 
                                   value = Sys.Date(), 
                                   format = "dd/mm/yyyy", 
                                   language = "pt-BR"),
                         textInput("valor", "Valor:"),
                         textAreaInput("descricao", "Descrição:"),
                         
                         actionButton("submit", "Enviar", class = "btn-primary")
                         ),
                       
                       
                       shinyjs::hidden(
                         div(
                           id = "thankyou_msg",
                           h3("Dados Importados com Sucesso"),
                           actionLink("submit_another", "Preencher Novamente")
                           )
                         ) 
                       )
                
                
                ) 
                )
              )

      )
    )


server <- function(input, output) {
  
  output$subgrupo_dep <- renderUI({
    selectInput("subgrupo", "Sub-Grupo:", choices = as.character(cbsubgrupo[cbsubgrupo$grupo==input$grupo,"subgrupo"]))
  })
  
  formData <- reactive({
    data <- sapply(campos, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  saveData <- function(data) {
    write.table(x = data, file = file.path("dados", "dados.csv"),sep = ";",
              row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
  }
  
    
  
  observeEvent(input$submit, {
    saveData(formData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  })
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  }) 
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)

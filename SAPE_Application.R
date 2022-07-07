
thematic::thematic_shiny(font = "auto")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


packages <- c("shiny","shinythemes","shinyjs","DT","shinydashboard","shinyWidgets","ggplot2","matrixStats","lsr","agricolae",
              "nortest","modeest","plyr","dplyr","data.table","purrr","shinyauthr","scales","DescTools","psycho","parameters",
              "see","corrplot","ltm","rcompanion","nortest","car","bslib","knitr","kableExtra","fBasics","stringi","tidyverse",
              "readr","stringr","stringi","berryFunctions","vcd","psych","polycor","ggcorrplot","GGally","Hmisc","corrplot","PerformanceAnalytics")

ipak(packages)

user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)



ui <- fluidPage(  theme = shinytheme("superhero"),
                  titlePanel("Satisfacción Alumnado Prácticas Externas"),
                  
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  shinyauthr::loginUI(id = "login"),
  uiOutput("sidebarpanel"),
                  
  
  
  
) #Close FluidPage

server <- function(input, output, session) {
  
  ruta <- "E:/Pruebas_R_CSV_WRITE/"
  ipak(packages)
   
  
  
  # credentials <- shinyauthr::loginServer(
    #  id = "login",
    #  data = user_base,
    #  user_col = user,
    #  pwd_col = password,
    #  sodium_hashed = TRUE,
    #  log_out = reactive(logout_init())
    #)
  
  # Logout to hide
  
  
  # logout_init <- shinyauthr::logoutServer(
  #  id = "logout",
  #   active = reactive(credentials()$user_auth)
  # )
  
  
  output$output_frecuency <- renderUI({
    
    h2("Indicadores del factor ", input$inSlider)
    
  })
  
  
  output$output_frecuency1 <- renderUI({
    
    if(input$Agrupa_percep_box== FALSE){
      h3("Tabla de frecuencias") # , input$inSlider
    }else{
      h3("Tabla de frecuencias para ", input$Select_compa_percep )
    }
    
  })
  
  
  output$output_caja_patilla <- renderUI({
    
    h3("Gráfico de caja y patilla para  ", input$Select_compa_boxplot )
    
  })
  
  output$output_headline <- renderUI({
    
    if(input$Agrupa_percep_box== FALSE){
      h3("Descriptiva") # , input$inSlider
    }else{
      h3("Descriptiva para ", input$Select_compa_percep )
    }
  })
  
  output$output_headline_hipotesis_Test <- renderUI({
    
    if(input$checkbox_hipo == TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        num = nrow(distinct(data1[input$Select_compa1])) 
  
        if(num <= 2){
          h2("Hipotesis") 
        }
      }
    }
  })


  output$output_headline_comp_9  <- renderUI({
    
    if(input$checkbox_hipo == TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        num = nrow(distinct(data1[input$Select_compa1])) 
        
        if(num <= 2){
          h3("Test de independencia") 
        }
      }
    }
  })
  
  
  output$output_headline_comp_0  <- renderUI({
    
    if(input$checkbox_hipo == TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        num = nrow(distinct(data1[input$Select_compa1])) 
  
        if(num <= 2){
          h3("Comparación de varianzas") 
        }
      }
    }
  })
  
  output$output_headline_varianza_1  <- renderUI({
    
    if(input$checkbox_hipo == TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        num = nrow(distinct(data1[input$Select_compa1])) 
        if(num <= 2){
          
          b <- unique(data1[,input$Select_compa1])
          b[1]
          
          if(b[1] != 2 ){
            data1[,input$Select_compa1][data1[,input$Select_compa1] == b[1]] <- 1
            data1[,input$Select_compa1][data1[,input$Select_compa1] == b[2]] <- 2
          }
          
          varianza <- var.test(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ as.numeric(data1[,input$Select_compa1]) )
          
          
          if(varianza[["p.value"]] > 0.05){
            return(h5("Se asume varianzas iguales"))
          }else{
            return(h5("Se asume varianzas distintas"))
          }
        }
      }
    }
  })
  
  output$output_headline_comp_1  <- renderUI({
    
    if(input$checkbox_hipo == TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        num = nrow(distinct(data1[input$Select_compa1])) 
  
        if(num <= 2){
         h3("Comparación de Medias (T-test)") 
        }
      }
    }
  })
  
  output$output_headline_normalidad <- renderUI({
    
    if(input$checkbox_hipo== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        
        h3("Normalidad") 
      }
    }
  })
  
  
  
  output$output_headline_anova_1 <- renderUI({
    
    if(input$checkbox_anova== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
          h2("Hipótesis") 
      }
    }
  })
  
  
  

  
  output$output_headline_anova_2 <- renderUI({
    
    if(input$checkbox_anova== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{

          h3("Normalidad (Residuos)") 
      }
    }
  })
  
  output$output_durbin <- renderUI({
    
    if(input$checkbox_anova== TRUE){
      fm <- funcion_aov_()
      durbin <- data.frame("Durbin" = durbinWatsonTest(fm$residuals) )
      
      if(round(durbin,0) == 2){
        h5("No hay autocorrelación") 
      }else{
        h5("Si hay autocorrelación") 
      }
      
    }
    
    
  })
  
  
  output$output_headline_anova_3 <- renderUI({
    
    if(input$checkbox_anova== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
          h3("Independencia") 
      }
    }
  })
  
  
  output$output_headline_anova_4 <- renderUI({
    
    if(input$checkbox_anova== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{

          h3("Homocedasticidad (Residuos)") 

      }
    }
  })
  
  output$output_headline_anova_5 <- renderUI({
    
    if(input$checkbox_anova== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        
        h3("Desviaciones (Residuos)") 
        
      }
    }
  })
  
  
  output$output_headline_anova_6 <- renderUI({
    
    if(input$checkbox_anova== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      fm <- funcion_aov_()
      
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        
        if( length(rownames(data1)) < 50){
          test_norma <-  shapiro.test(fm$residuals)
        }else{
          test_norma <- lillie.test(fm$residuals)
        }
        
        if(test_norma$statistic < 0.05){
          h5("Se asume normalidad en los datos")
        }else{
          h5("No se asume normalidad en los datos")
        }
      }
    }
  })
 
  
  
  
  output$output_headline_comp_6   <- renderUI({
    if(input$checkbox_Comparacion== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        if(length(input$Select_compa1) == 1){
          if( input$Select_metodo_PostHoc != "Pairwill-wilcox"){
              #input$Select_metodo_PostHoc
              h3("Test de ", input$Select_metodo_PostHoc)  
            
          }else{
              h3("Test de suma de rangos de Wilcoxon")  
            }
          }
        
      }
    }
  })
  
  output$output_headline_comp_7   <- renderUI({
    if(input$checkbox_Comparacion== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        
        if(length(input$Select_compa1) == 1){
          h2("Comparaciones multiples")  
        }
      }
    }
  })
  
  output$output_headline_comp_3  <- renderUI({
    if(input$checkbox_anova== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        num = nrow(distinct(data1[input$Select_compa1])) 
        
        if(input$checkbox_anova_noparam== TRUE){
          h3("Análisis de la Varianza") 
        }else{
          h3("Análisis de la Varianza")
        }
      }
    }
  })
  
  output$output_headline_comp_8  <- renderUI({
    if(input$checkbox_anova== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
        num = nrow(distinct(data1[input$Select_compa1])) 
        
        if(input$checkbox_anova_noparam== TRUE){
          h4("(Anova No Paramétrico)") 
        }else{
          h4("(Anova Paramétrico)")
        }
      }
    }
  })
  
  output$output_headline_comp_4  <- renderUI({
    if(input$checkbox_anova== TRUE){
      if(input$checkbox_anova_homoce == 1){
        ASIfactor <- funcion_tabla_policorica()
        Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
        data1 <- read.csv(Fichero_csv)
        
        temp <-  is.error(tempo <- data1[input$Select_compa1] )
        if(temp == TRUE){
        }
        else{
              h3("Efecto de la Anova") 
        }
      }
    }
  })
  
  output$output_headline_comp_5  <- renderUI({
    if(input$checkbox_anova== TRUE){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      temp <-  is.error(tempo <- data1[input$Select_compa1] )
      if(temp == TRUE){
      }
      else{
          h3("Gráficos para hipotesis") 

      }
    }
  })
  
  output$sidebarpanel <- renderUI({
    
    # Show only when authenticated
    
    #req(credentials()$user_auth)
    
    
    navbarPage("S.A.P.E.", id = "tabs",
               tabPanel("Subida de Datos",
                        tags$style(".nav-tabs {margin-bottom: 10px;} "), 
                        tags$style(".col-sm-4 {width: 20.333333%;margin-top: 4%;} "),   
                        tags$style(".btn-default {margin-bottom: 5px; background-color:#2c3c54; border-radius: 3px; width: 100%; } "),
                        
                        
                        tags$style(".well {background-color:#4c5b6b;height: 100%;display: flex;flex-direction: column; border-radius: 6px;} "), 
                        
                        
                        tags$style(".input {color: #bebebe;}"),
                        tags$style(".multi-wrapper .non-selected-wrapper {background: #2c3c54; height: 320px;}"),
                        tags$style(".multi-wrapper .selected-wrapper {background: #2c3c54; height: 320px;}"),
                        
                        tags$style(".multi-wrapper .search-input {background-color: #2c3c54;}"),
                        tags$style(".multi-wrapper  {border: 1px solid #4c5b6b;}"),
                        tags$style("input {color: #f5f5f5;}"),
                       # tags$style("td {text-align: center !important;}"),
                        
                      
                        
                        
                        sidebarLayout(
                          sidebarPanel(
                            
                            fileInput(
                              inputId = "files", 
                              label = "Elige los archivos CSV a cargar", 
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values",
                                         ".csv")
                            ),
                            uiOutput("selectfile"),
                            
                            
                            
                          ),
                          
                          mainPanel(
                            
                            #plotOutput("graph"),
                            
                            DTOutput("table1"),
                            
                            
                            
                            #tableOutput("table4"),
                          ),
                          
                        )
               ),
               
               tabPanel("Modificación de datos",
                        tabsetPanel(
                          tabPanel("Fusion", 
                                   
                                   conditionalPanel(condition = "input.toggleSidebarPanel % 2 == 0",sidebarPanel(
                                     uiOutput("selectfile_1"),
                                     #uiOutput("selecciom_columna_1"),
                                     hr(),
                                     actionButton("do", "Fusionar archivos"),
                                     downloadButton("downloadData", "Download")
                                     
                                   )),
                                   mainPanel(
                                     actionButton("toggleSidebarPanel", "", icon = icon("bars")),
                                     HTML("<br><br><br>"),
                                     
                                     
                                     DTOutput("table_fusion")
                                     #tableOutput("table5"),
                                   )
                                   
                          ),
                          
                          tabPanel("Borrar Fila/Columna",
                                   sidebarPanel(
                                     uiOutput("selectfile_arreglo"),
                                     uiOutput("selecciom_columna_arreglo"),
                                     actionButton("arreglo_do", "Columna a eliminar"),
                                     hr(),
                                     numericInput("rowno", "Fila a borrar:", 1, step = 1),
                                     actionButton("delete", "Fila a eliminar"),
                                     #hr(),
                                     #uiOutput("selecciom_columna_categorizar"),
                                     #actionButton("cambiar_elemento", "Categorizar"),
                                     
                                     
                                     
                                   ),
                                   mainPanel(
                                     
                                     
                                     
                                     DTOutput("table_elim"),
                                     
                                     
                                   )
                                   
                          ),
                          tabPanel("Correción Fila/Columna",
                                   sidebarPanel(
                                     uiOutput("selectfile_correcion"),
                                     uiOutput("selecciom_columna_correcion"),
                                     #hr(),
                                     # uiOutput("selecciom_columna_correcion"),
                                     textInput("caption", "Elemento a subtituir", ""),
                                     textInput("substituto", "Elemento cambiante", ""),
                                     actionButton("cambiar_elemento", "Realizar cambio"),
                                     #uiOutput("selecciom_columna_redondear"),
                                     #actionButton("redondear", "Redondear"),
                                     textInput("Nombre_Columna_Cambio","Introduce el nuevo nombre de la columna"),
                                     actionButton("Cambiar_nombre", "Cambiar Nombre"),
                                     
                                     
                                     
                                   ),
                                   mainPanel(

                                     DTOutput("table_correcion"),
                                     
                                   )
                                   
                          ),
                          tabPanel("Categorización",
                                   sidebarPanel(
                                     
                                     uiOutput("selectfile_categorizar"),
                                     uiOutput("selecciom_columna_categorizar"),
                                     textInput("marcas_cat", "Introduce las marcas separados por coma ", "0 , 5 , 6 "),
                                     textInput("nombres_cat", "Introduce el nombre de la columna", "Nombre_col_Categorizada"),
                                     actionButton("categorizacion", "Aplicar cambios"),
                                     actionButton("Escalar", "Escalar variable"),
                                     
                                   ),
                                   mainPanel(
                                     
                                     DTOutput("table_categorizacion"),
                                   )
                                   
                          ),
                          tabPanel("Variables",
                                  # sidebarPanel(),
                                  
                                  tags$div( class = "variables_main_container",
                                   #mainPanel(
                                     #theme = shinytheme("superhero"),
                                         # http://shinyapps.dreamrs.fr/shinyWidgets/
                                     
                                     # azul oscuro : #2c3c54
                                     
                                     # azul flojo: #4c5c6c 
                                     
                                     
                                     tags$style(".variables_container  {display: flex; justify-content: space-between}"),
                                     tags$style("a {color: #e1ebeb;}"),
                                     
                                      
                                     #column(6,div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("seleccion_variable_1")) ),
                                     
                                     #column(6,div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("seleccion_variable_2")) ),
                                     
                                     tags$div( class = "variables_container", 
                                     tags$div(uiOutput("seleccion_variable_1"),
                                              h5("Categoria Origen"),
                                              class = "origen_variable"),
                                     tags$div(uiOutput("seleccion_variable_2"),
                                              h5("Categoria Destino"),
                                              class = "destino_variable"),
                                     ),
                                     
                                     tags$style(".variables_container  {display: flex;}"),
                                     tags$style(".origen_variable  {}"),
                                     tags$style(".destino_variable  {}"),
                                      
                                     
                                     #column(6,div(style="display: inline-block;vertical-align:top; width: 200px;",h5("Categoria Origen") ) ),
                                     
                                     #column(6,div(style="display: inline-block;vertical-align:top; width: 200px;",h5("Categoria Destino")) ),
                                     
                                     # tags$div(`class` = "Mucho_Texto",
                                     #  uiOutput("seleccion_variable_1"),
                                     # uiOutput("seleccion_variable_2"), ),
                                     
                                     tags$style(".seleccion_variable { display: flex; flex-direction: column; align-items: center;}"),
                                     tags$style("#Multi_Variables { width: 100%;}"),
                                     tags$div(class = "seleccion_variable",
                                     uiOutput("Multi_Variables"),
                                     actionButton("intercambio_var", "Realizar Intercambio", class = "Intercambio_class"),
                                     ),
                                     tags$style(".Intercambio_class  {background-color: #4c5b6b; width: 40%; 
                                                min-width: 40%; border-radius: 6px; margin-top: 1px;}"),
                                     
                                     
                                     
                                  )
                                     
                                # mainpanel   )#
                                   
                          )
                          
                          ##
                        ), 
                        
                        
               ),
               
               navbarMenu("Análisis Estadisticos",
                          tabPanel("Exploratorio de datos",
                                   tabsetPanel(
                                     tabPanel("Unidimensional",
                                             sidebarPanel(
                                                # conditionalPanel(condition = "input.toggleSidebarPanelUni % 2 == 0", sidebarPanel(
                                                uiOutput("selectfile_4"),
                                                uiOutput("selecciom_columna_3"),
                                                checkboxInput("Todos_box", "Todos", FALSE),
                                                checkboxInput("Tablas_box", "Tablas", FALSE),
                                                checkboxInput("Graficos_box", "Gráficos", FALSE),
                                                checkboxInput("Estadisticos_box", "Estadisticos Descriptivos", FALSE),
                                                
                                                #uiOutput("selecciom_columna_4"),
                                                actionButton("Vamos", "Click Me"),
                                              ),
                                            #  ),
                                            # actionButton("toggleSidebarPanelUni", "", icon = icon("bars")),
                                              
                                              tags$style(".div_frecuencias  {display: flex; flex-direction: column; align-items: center;}"),
                                              tags$style(".div_estadistico  {display: flex; flex-direction: column; align-items: center;}"),
                                              tags$style(".div_plot  {display: flex; flex-direction: column; align-items: center;}"),
                                              tags$style(".div_pieplot  {display: flex; flex-direction: column; align-items: center;}"),
                                              tags$style(".div_histograma  {display: flex; flex-direction: column; align-items: center;}"),
                                              tags$style(".div_boxplot  {display: flex; flex-direction: column; align-items: center;}"),
                                              
                                              
                                              
                                              tags$style(".unidimensional_plot_container  {flex-direction: column;}"),
                                              
                                              
                                              
                                              tags$div( class = "unidimensional_main_container",
                                                
                                                conditionalPanel(
                                                  
                                                  condition = "input.Todos_box==1 || input.Tablas_box==1",
                                                  tags$div(h3("Tabla de Frecuencias"),
                                                  tableOutput("table_frecuencias"),
                                                  class = "div_frecuencias"),
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = "output.Estadistico_mostrar",
                                                
                                                  conditionalPanel(
                                                    condition = "input.Todos_box==1 || input.Estadisticos_box==1",
                                                    tags$div(hr(),
                                                      h3("Estadísticos Descriptivos"),
                                                      h5("Medidas Generales"),
                                                    tableOutput("table_descriptiva_1"),
                                                    h5("Medidas de Tendencias Centrales"),
                                                    tableOutput("table_descriptiva_2"),
                                                    h5("Medidas de Dispersión"),
                                                    tableOutput("table_descriptiva_3"),
                                                    h5("Medidas de Posición"),
                                                    tableOutput("table_cuartil"),
                                                    h5("Medidas de Forma"),
                                                    tableOutput("table_descriptiva_4"),
                                                    class = "div_estadistico"  ),
                                                  ), 
                                                ),
                                                
                                                
                                                tags$div( class = "unidimensional_plot_container",
                                                conditionalPanel(
                                                  condition = "input.Todos_box==1 || input.Graficos_box==1",
                                                  #h3("Graficos"),
                                                  #plotOutput("plot"),
                                                  # hr(),
                                                  # plotOutput("PiePlot"),
                                                  
                                                  conditionalPanel(
                                                    condition = "output.plot_mostrar",
                                                    tags$div(
                                                    hr(),
                                                    conditionalPanel(
                                                      condition = "output.plot_discretas",
                                                      h3("Diagrama de Barras"),
                                                    ),
                                                    
                                                    conditionalPanel(
                                                      condition = "output.plot_continuas",
                                                      h3("Diagrama de Rectángulos"),
                                                    ),
                                                            
                                                    plotOutput("plot"),
                                                    
                                                    class = "div_plot"  ), 
                                                  ),
                                                  
                                                  
                                                  conditionalPanel(
                                                    condition = "output.PiePlot_mostrar",
                                                    hr(),
                                                    tags$div(h3("Diagrama Circular"),
                                                    plotOutput("PiePlot"),
                                                    class = "div_pieplot"  ),
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "output.histograma_mostrar",
                                                    hr(),
                                                    tags$div(h3("Histograma"),
                                                    plotOutput("HistogramPlot"),
                                                    hr(),
                                                    hr(),
                                                    class = "div_histograma"  ),
                                                  ), 
                                                  
                                                  conditionalPanel(
                                                    condition = "output.BoxPlot_mostrar",
                                                    hr(),
                                                    tags$div(h3("Caja y Patilla"),
                                                    plotOutput("BoxPlot"),
                                                    class = "div_boxplot"  ),
                                                  ),
                                                  
                                                ),
                                                  
                                               ),
                                                
                                                
                                               # is.element(input$Select_mer3, variables_continuas) == TRUE
                                                
                                                #tableOutput("table_descriptiva_2"),
                                               # tableOutput("table_descriptiva_3"),
                                                
                                                
                                                
                                                
                                             )
                                     ),
                                     
                                     tabPanel("Bidimensional",
                                              
                                              sidebarPanel(
                                                
                                                uiOutput("selectfile_bidi"),
                                                uiOutput("selecciom_columna_bidi_1"),
                                                uiOutput("selecciom_columna_bidi_2"),
                                                checkboxInput("Todos_box_bidi", "Todos", FALSE),
                                                checkboxInput("Tablas_box_bidi", "Tablas", FALSE),
                                                checkboxInput("Graficos_box_bidi", "Gráficos", FALSE),
                                                checkboxInput("Independecia_box_bidi", "Test de Independencia", FALSE),
                                                checkboxInput("Asociacion_box_bidi", "Medidas de Asociación", FALSE),
                                                actionButton("Boton_agrup", "Click Me"),
                                              ),
                                              
                                              mainPanel(
                                                
                                                tags$style(".div_frecuencias_bidi  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_boxplot_bidi  {display: flex; flex-direction: column; align-items: center;}"),
                                                #tags$style(".div_independencia_bidi  {display: flex;flex-direction: row;justify-content: center;}"),
                                                tags$style(".div_pieplot_bidi  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_fisher_bidi  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_titulo_inde_bidi  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_asociacion_bidi  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_chi_bidi  {display: flex; flex-direction: column; align-items: center;}"),
                                                
                                                
                                                
                                                conditionalPanel(
                                                  
                                                  condition = "input.Todos_box_bidi==1 || input.Tablas_box_bidi==1",
                                                  tags$div(h3("Tabla de Frecuencias"),
                                                           tableOutput("table_frecuencias_bidi"),
                                                           class = "div_frecuencias_bidi"  ),
                                                ),
                                                
                                                
                                                conditionalPanel(
                                                  condition = "input.Todos_box_bidi==1 || input.Independecia_box_bidi==1",
                                                  
                                                  tags$div(
                                                        h3("Test de Independencia"),
                                                        class = "div_titulo_inde_bidi"  ),
                                                  
                                                  tags$div(
                                                  
                                                    tags$div(
                                                             
                                                             h5("Chi Cuadrado"), 
                                                             tableOutput("table_chi_cuadrado"),
                                                             class = "div_chi_bidi"  ),
                                                    
                                                    tags$div(
  
                                                             h5("Fisher"), 
                                                             tableOutput("table_fisher"),
                                                             class = "div_fisher_bidi"  ),
                                                    class = "div_independencia_bidi"  ),
                                                  
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = "input.Todos_box_bidi==1 || input.Asociacion_box_bidi==1",
                                                  tags$div(h3("Medidas de asosiación"),
                                                           #h5("CramerV"), 
                                                            tableOutput("table_cramer"),
                                                            #tableOutput("table_phi_coef"),
                                                           #tableOutput("table_contingencia"),
                                                            class = "div_asociacion_bidi"  ),
                                                
                                              ),
                                                
                                                
                                                conditionalPanel(
                                                  
                                                  condition = "input.Todos_box_bidi==1 || input.Graficos_box_bidi==1",
                                                  
                                                           
                                                           conditionalPanel(
                                                             condition = "output.boxplot_bidi_continuas1 && output.pieplot_bidi_discreta2 ||
                                                             output.boxplot_bidi_continuas2 && output.pieplot_bidi_discreta1",
                                                             tags$div(h3("Caja y Patilla"),
                                                             plotOutput("BoxPlot_bidi"),
                                                             h3("Histograma"),
                                                             plotOutput("Histogram_bidi"),
                                                             class = "div_boxplot_bidi"  ),
                                                           ),
                                                  
                                                           conditionalPanel(
                                                             condition = "output.pieplot_bidi_discreta1 && output.pieplot_bidi_discreta2",
                                                             tags$div(
                                                               
                                                             conditionalPanel(
                                                               condition = "output.plot_discretas_bidi1 || output.plot_discretas_bidi2",
                                                               h3("Diagrama de Barras Agrupados"),
                                                             ),
                                                             
                                                             
                                                               
                                                             plotOutput("Barras_bidi"),
                                                             h3("Diagramas Circulares Agrupados"),
                                                             plotOutput("Pieplot_bidi"),
                                                             class = "div_pieplot_bidi"  ),
                                                           ),
                                                  
                                                          
                                                            conditionalPanel(
                                                              condition = "output.boxplot_bidi_continuas1 && output.boxplot_bidi_continuas2",
                                                              h3("Diagrama de Dispersión de puntos"),
                                                              plotOutput("Dispersion_puntos_bidi"), 
                                                              class = "div_boxplot_bidi"  
                                                              ),
                                                             
                                                ),
                                                
                                                
                                              )
                                              
                                     )
                                     
                                     
                                   )   
                          ),
                          
                          tabPanel("Análisis de Satisfacción",
                                   tabsetPanel(
                                     tabPanel("Análisis de Fiabilidad",
                                              
                                              sidebarPanel(
                                                
                                                wellPanel( style=" background-color:#2c3c54",
                                                  h5("Datos"),
                                                  uiOutput("selectfile_graph_cat"),
                                                  uiOutput("selecciom_graph_cat1"),
                                                ),
                                                wellPanel( style=" background-color:#2c3c54",
                                                   h5("Relación entre indicadores"),
                                                   uiOutput("selecciom_fiabilid_matriz"),
                                                   
                                                   h5("Medidas de fiabilidad"),
                                                   checkboxInput("alfa_fia_box", "Alfa \u03B1", FALSE),
                                                   checkboxInput("split_fia_box", "Split Half", FALSE),
                                                   checkboxInput("otros_indic_box", "Otros indicadores", FALSE),
                                                   actionButton("Boton_fiabilidad", "Click Me",
                                                              style="color: #fff; background-color: #4c5b6b; border-color: #4c5b6b"),
                                                ),
                                                
                                                wellPanel( style=" background-color:#2c3c54",
                                                 h5("Adecuación del Análisis"),
                                                 checkboxInput("Bartlett_fia_box", "Test de esfericidad de Bartlett", FALSE),
                                                 checkboxInput("KMO_fia_box", "Índice muestral KMO", FALSE),
                                                 actionButton("Boton_Adecuacion", "Click Me",
                                                              style="color: #fff; background-color: #4c5b6b; border-color: #4c5b6b"), 
                                                    
                                                ),
                                                
                                                
                                                # uiOutput("selecciom_graph_cat2"),
                                                
                                              ),
                                              
                                              mainPanel(
                                                
                                                tags$style(".div_fiabilidad_1  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_fiabilidad_2  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_fiabilidad_5  {display: flex; flex-direction: column; align-items: center;}"),
                                                
                                                
                                               # tags$div(
                                                 
                                               
                                               
                                               tags$div(
                                                  h1("Análisis de Fiabilidad"),
                                                  conditionalPanel(
                                                    condition = " input.Select_fiabilid_matriz == 'Policórica'",
                                                        h3("Matriz Policórica"),
                                                       
                                                        
                                                    ),
                                                  conditionalPanel(
                                                    condition = " input.Select_fiabilid_matriz == 'Pearson'",
                                                    h3("Matriz Pearson"),
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = " input.Select_fiabilid_matriz == 'Tetracorica'",
                                                    h3("Matriz Tetracorica"),
                                                  ),
                                                  
                                                  class = "div_fiabilidad_1" ),
                                               plotOutput("Plot_Matriz_Corr_fiabilidad"),
                                                
                                                
                                               
                                               tags$div(
                                                 
                                                 conditionalPanel(
                                                   condition = " input.Select_fiabilid_matriz == 'Pearson' && input.alfa_fia_box == 1",
                                                   h3("Alpha de Cronbanch y Lambda de Guttman"),
                                                 ),
                                                 
                                                 conditionalPanel(
                                                   condition = " input.Select_fiabilid_matriz == 'Policórica' && input.alfa_fia_box == 1",
                                                   h3("Alpha y Lambda de Guttman"),
                                                 ),
                                                 
                                                 conditionalPanel(
                                                   condition = " input.Select_fiabilid_matriz == 'Tetracorica' && input.alfa_fia_box == 1",
                                                   h3("Alpha y Lambda de Guttman"),
                                                 ),
                                                 
                                             
                                                 
                                               conditionalPanel(
                                                 condition = " input.alfa_fia_box == 1",
                                                 tableOutput("table_alpha_total"), 
                                               ),
                                               
                                               conditionalPanel(
                                                 condition = " input.alfa_fia_box == 1",
                                                 tableOutput("table_alpha_drop"),
                                               ),
                                               
                                               
                                               conditionalPanel(
                                                 condition = " input.split_fia_box == 1",
                                                 h3("Split Half"),
                                               ),
                                               
                                              
                                               
                                               conditionalPanel(
                                                 condition = "input.split_fia_box  == 1",
                                                 tableOutput("table_split_1"),
                                                 tableOutput("table_split_2"),
                                               ),
                                               
                                               conditionalPanel( 
                                                 condition = " input.otros_indic_box == 1",
                                                 h3("Otros Indicadores"),
                                                 
                                               ),
                                               
                                               conditionalPanel( 
                                                 condition = " input.otros_indic_box == 1",

                                                 tableOutput("table_indicadores1"),
                                                 tableOutput("table_indicadores2"),
                                               ),
                                               class = "div_fiabilidad_5" ),
                                               
                                           
                                             #  conditionalPanel( 
                                             #   condition = "input.Bartlett_fia_box == 1 || input.KMO_fia_box == 1",   
                                             #    hr(style="border-color:white; width:100%; height:5px;"),
                                             #  ),
                                                
                                               tags$div(
                                                
                                                 
                                                 
                                                 conditionalPanel( 
                                                   condition = " input.Bartlett_fia_box == 1",
                                                   h3("Test de esfericidad de Bartlett"),
                                                 ),
                                                 
                                                 conditionalPanel( 
                                                   condition = " input.Bartlett_fia_box == 1",
                                                   tableOutput("table_Bartlett"),
                                                 ),
                                                
                                                
                                                conditionalPanel(
                                                  condition = "input.KMO_fia_box == 1",
                                                  
                                                  h3("Índice de KMO"),
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = " input.KMO_fia_box == 1",
                                                  tableOutput("table_KMO"),
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = " input.KMO_fia_box == 1",
                                                
                                                  tableOutput("table_KMO1"),
                                                ),
                                                
                                                
                                                
                                                
                                                
                                                
                                               class = "div_fiabilidad_2" ),
                                                
                                             
                                                # h3("Alfa de Cronbach clásico"),
                                                # tableOutput("table_cronbach"),
                                                
                                               # h3("Omega ordinal"),
                                               # tableOutput("table_omega_ordinal"),
                                                
                                                #h3("Omega normal"),
                                                # tableOutput("table_omega"),
                                                
                                                #plotOutput("Mosaicos"),
                                                #plotOutput("Apiladas"),
                                                #plotOutput("Agrupadas"),
                                              
                                               #  class = "div_fiabilidad" ),
                                               
                                                
                                              )
                                              
                                     ),
                                     tabPanel("Análisis de Dimensionalidad",
                                              
                                              tags$style(".div_separacion  {border-top: 1px solid black;height: 2px;max-width: 200px;padding: 0; margin: 20px auto 0 auto;}"),
                                              
                                              sidebarPanel(
                                                
                                                wellPanel( style=" background-color:#2c3c54",# border:3px inset;border-color:#2c3c54;background-color:#ffffff
                                                  h5("Datos"),
                                                  uiOutput("selectfile_factorial"),
                                                  uiOutput("selecciom_factorial1"),
                                                ),
                                                wellPanel( style=" background-color:#2c3c54",
                                                  h5("Número de factores"),
                                                  checkboxInput("NumFact_box", "Estimar el número de factores", FALSE),
                                                ),
                                              
                                                wellPanel( style=" background-color:#2c3c54",
                                                #  tags$div(class = "div_separacion" ),
                                                    h5("Análisis Factorial"),
                                                    checkboxInput("Fact_box", "Factorización", FALSE),
                                                  
                                                    conditionalPanel(
                                                      condition = "input.Fact_box==1",
                                                      numericInput("rowfactorial", "Seleccione el número de factores:", 1, step = 1),
                                                      uiOutput("seleccion_fm_fa"),
                                                      uiOutput("seleccion_rotacion_fa"),
                                                      uiOutput("seleccion_cor_fa"),
                                                      actionButton("Boton_fact", "Realizar Factorización",
                                                                   style="color: #fff; background-color: #4c5b6b; border-color: #4c5b6b"),
                                                    
                                                    ),
                                                ),
                                                wellPanel( style=" background-color:#2c3c54",
                                                  h5("Predicción"),
                                                  checkboxInput("Predi_box", "Predicciones", FALSE),
                                                  conditionalPanel(
                                                    condition = "input.Predi_box==1",
                                                    checkboxInput("Puntuaciones_box", "Gráfico Puntuaciones Factoriales", FALSE),
                                                   # checkboxInput("Predecir_box", "Predecir", FALSE),
                                                   
                                                    checkboxInput("vector_pre_box", "Añadir nuevo indicador", FALSE),
                                                   
                                                    conditionalPanel(
                                                    condition = "input.vector_pre_box==1",
                                                      textInput("vec_predi", "Introduzca un vector de indicadores"),
                                                      actionButton("Boton_vector", "Generar vector aleatorio",
                                                                   style="color: #fff; background-color: #4c5b6b; border-color: #4c5b6b"),
                                                      HTML("<br><br>"),
                                                      actionButton("Boton_predi", "Realizar Predicciones",
                                                                   style="color: #fff; background-color: #4c5b6b; border-color: #4c5b6b"),
                                                    ),
                                                ),
                                                    
                                                  
                                                  
                                                ),
                                                
                                              ),
                                              
                                              mainPanel(
                                                tags$style(".div_factorial_1   {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_factorial_2   {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_factorial_3   {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_factorial_4   {display: flex; flex-direction: column; align-items: center;}"),
                                                
                                                tags$style(".div_correlacion_policorica  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_residual  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_Factores_1  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_RMSEA  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_factor_seleccionado  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_Sedimentacion  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_Prediccion  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_predis  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_ana_factorial  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_predi_text  {display: flex; flex-direction: column; align-items: right;}"),
                                                
                                                
                                                
                                                
                                                
                                                
                                                #tags$div(
                                             
                                                  
                                                tags$div(
                                                  h1("Análisis de Dimensionalidad"),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.NumFact_box==1",
                                                    h3("Número de factores propuestos"),
                                                  ),
                                                  tableOutput("table_num_factores_as_data"),
                                                  
                                                  
                                                class = "div_correlacion_policorica" ),
                                                
                                                tags$div(
                                                  
                                                  conditionalPanel(
                                                    condition = "input.NumFact_box==1",
                                                    h3("Resumen de número de factores"),
                                                  ),
                                                  
                                                  tableOutput("table_num_factores_summary"),
                                                  
                                                  
                                                 
                                                 
                                                class = "div_factor_seleccionado" ),
                                                
                                                
                                                tags$div(
                                                  conditionalPanel(
                                                    condition = "input.NumFact_box==1",
                                                    h3("Gráfico de Barras"),
                                                  ),
                                                  
                                                  class = "div_Sedimentacion" ),
                                                
                                                conditionalPanel(
                                                  condition = "input.NumFact_box==1",
                                                  plotOutput("Plot_policorica"),
                                                ),
                                                
                                               
                                                  
                                                #################################################################
                                                
                                                conditionalPanel(
                                                  condition = "input.NumFact_box==1",
                                                  hr(style="border-color:white; width:100%; height:5px;"),
                                                ),
                                                
                                                tags$div(
                                                  
                                                  

                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                       h2("Factorización"),
                                                       
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                  
                                                      h3("Modelo Factorial"),
                                                    
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                     tableOutput("table_policorica_Uniquenesses"),
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    h3("Varianza Explicada"),
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    tableOutput("table_policorica_Vaccounted"),
                                                  ),
                                                  
                                                  
                                                 
                                                 conditionalPanel(
                                                   condition = "input.Fact_box==1",
                                                    h3("Solución factorial gráfica"),
                                                 ),
                                                 
                                                 class = "div_Factores_1" ),
                                                
                                              
                                                
                                              conditionalPanel(
                                               condition = "input.Fact_box==1",
                                               plotOutput("Plot_loadings"),
                                              ),
                                              
                                             
                                                
                                                tags$div(
                                                  
                                                  
                                                  
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    h3("Medidas de bondad"),
                                                  ),
                                                  
                                                 
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    tableOutput("table_policorica_RMSEA"),
                                                    
                                                    
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    h3("Test de hipótesis"),
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    tableOutput("tabla_texto_RMSEA"),
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    h3("Medidas de adecuación de la puntuación del factor"),
                                                  ),
                                                  
                                                  
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    tableOutput("table_bloque_3"),
                                                    
                                                    
                                                  ),
                                                  
                                                  tags$div(
                                                    conditionalPanel(
                                                      condition = "input.Fact_box==1",
                                                      h3("Gráfico de Sedimentación"),
                                                    ),
                                                    
                                                    class = "div_Sedimentacion" ),
                                                  
                                                  class = "div_RMSEA" ), 
                                                  
                                              
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    plotOutput("Plot_Sedimentacion"),
                                                    
                                                  ),
                                                  
                                                tags$div(
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    h3("Resultado del análisis factorial"),
                                                  ),
                                                  class = "div_ana_factorial" ),
                                                
                                              
                                                  conditionalPanel(
                                                    condition = "input.Fact_box==1",
                                                    plotOutput("Plot_fadiagram"),
                                                  ),
                                               
                                              
                                              conditionalPanel(
                                                condition = "input.Predi_box==1",
                                                hr(style="border-color:white; width:100%; height:5px;"),
                                              ),
                                              
                                              tags$div(
                                                 
                                                  conditionalPanel(
                                                    condition = "input.Predi_box==1",
                                                    h2("Predicción"),
                                                  ),
                                                  
                                                 
                                                 conditionalPanel(
                                                   condition = "input.Puntuaciones_box==1",
                                                   h3("Coeficientes del modelo"),
                                                 ),
                                                 
                                                 conditionalPanel(
                                                   condition = "input.Puntuaciones_box==1",
                                                   tableOutput("table_policorica_Weights"),
                                                 ),
                                                 
                                                 
                                                 #  conditionalPanel(
                                                   #   condition = "input.Puntuaciones_box==1",
                                                #     h3("Puntuaciones Factoriales"),
                                                #  ),
                                                 
                                                #  conditionalPanel(
                                                   #   condition = "input.Puntuaciones_box==1",
                                                   #     tableOutput("table_policorica_Values"),
                                                #  ),
                                                    
                                                #  conditionalPanel(
                                                #    condition = "input.NumFact_box==1",
                                                #    hr(style="border-color:white; width:100%; height:5px;"),
                                                #  ),
                                                  #h3("Structure"),
                                                 #tableOutput("table_policorica_Structure"),
                                                 
                                                 
                                                 
                                               
                                               # class = "div_correlacion"  ),
                                               
                                                 
                                               
                                               #output$text
                                                
                                               class = "div_Prediccion" ),
                                              
                                              
                                              
                                              tags$div(
                                                conditionalPanel(
                                                  condition = "input.vector_pre_box==1",
                                                  h3("Satisfacción del vector de indicadores introducido"),
                                                ),
                                                
                                                
                                                
                                                conditionalPanel(
                                                  condition = "input.vector_pre_box==1 ",
                                                  tableOutput("table_predicciones_factores"),
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = "input.Puntuaciones_box==1",
                                                  h3("Gráfico de Satisfacciones"),
                                                  
                                                ),
                                                
                                                class = "div_predis" ),
                                               
                                               conditionalPanel(
                                                 condition = "input.Puntuaciones_box==1",
                                                 plotOutput("Plot_Puntuaciones"),
                                               ),
                                              
                                              tags$div(
                                                
                                                conditionalPanel(
                                                  condition = "input.vector_pre_box==1",
                                                  h4("Nota : En rojo se muestra el resultado de la predicción")
                                                  #tableOutput("table_predicciones_text"),
                                                ),
                                              
                                              class = "div_predi_text" ),
                                                
                                              )
                                              
                                     ),
                                     tabPanel("Análisis de Percepción",
                                              
                                              sidebarPanel(
                                                
                                                        #uiOutput("selectfile_percepcion"),
                                                        #uiOutput("selecciom_percepcion"),
                                                wellPanel( style=" background-color:#2c3c54",
                                                  uiOutput("slider"),
                                                ),
                                                
                                                wellPanel( style=" background-color:#2c3c54",
                                                  checkboxInput("Agrupa_percep_box", "Agrupamiento", FALSE),
                                                  conditionalPanel(
                                                    condition = "input.Agrupa_percep_box==1",
                                                    uiOutput("selecciom_comparativa_percep"),
                                                    checkboxInput("boxplot_percep_box", "Gráfica caja y patilla", FALSE),

                                                ),
                                                
                                                
                                                conditionalPanel(
                                                  condition = "input.boxplot_percep_box==1 && input.Agrupa_percep_box==1",
                                                  uiOutput("selecciom_comparativa_boxplot"),

                                                ),
                                                  
                                                ),
                                                actionButton("Boton_describe", "Realizar operación"),
                                                        #checkboxInput("Describe_box", "Pesos", FALSE),
                                                
                                              ),
                                              
                                              mainPanel(
                                                tags$style(".div_percepcion  {display: flex; flex-direction: column; align-items: center;}"),
                                                
                                                tags$div(
                                                  h1("Análisis de percepción del alumnado"),
                                                  uiOutput('output_frecuency'),
                                                  
                                                  uiOutput('output_frecuency1'),
                                                  conditionalPanel(
                                                    condition = "input.Agrupa_percep_box==0",
                                                    tableOutput("table_frecuency"),
                                                  ),
                                                 

                                                  conditionalPanel(
                                                    condition = "input.Agrupa_percep_box==1",
                                                    tableOutput("tabla_frecuency2"),
                                                  ),
                                                  
                                                  hr(),
                                                  uiOutput('output_headline'),
                                                  tableOutput("table_describe"),
                                                  
                                                  
                                                  conditionalPanel(
                                                    condition = "input.boxplot_percep_box==1 && input.Agrupa_percep_box==1",
                                                    conditionalPanel(
                                                      condition = "input.NumFact_box==1",
                                                      hr(style="border-color:white; width:100%; height:5px;"),
                                                    ),
                                                    uiOutput('output_caja_patilla'),
                                                  ),
                                                  
                                                  
                                                  class = "div_percepcion" ),
                                                
                                                conditionalPanel(
                                                  condition = "input.boxplot_percep_box==1 && input.Agrupa_percep_box==1",
                                                  plotOutput("BoxPlot_percepcion"),
                                                ),
                                              )
                                              
                                     ),
                                     
                                     tabPanel("Comparativa entre grupos",
                                              
                                              sidebarPanel(
                                                h5("Comparativa"),
                                                wellPanel( style=" background-color:#2c3c54",
                                                     h6("Datos"),
                                                     #uiOutput("selectfile_factorial"),
                                                     uiOutput("slider1"),
                                                     uiOutput("selecciom_comparativa1"),
                                                ),
                                                
                                                
                                                 wellPanel( style=" background-color:#2c3c54",# border:3px inset;border-color:#2c3c54;background-color:#ffffff
                                                            h6("Análisis de variables de 2 niveles"),
                                                            
                                                            checkboxInput("checkbox_hipo", "T-test", FALSE),
                                                            
                                                                    #  uiOutput("selecciom_factorial1"),
                                                 ),
                                                  
                                                wellPanel( style=" background-color:#2c3c54",
                                                           h6("Análisis de la varianza"), 
                                                           checkboxInput("checkbox_anova", "Anova", FALSE),
                                                           
                                                           conditionalPanel(
                                                             condition = "input.checkbox_anova==1",
                                                             
                                                             # conditionalPanel(
                                                             #  condition = "input.checkbox_anova_noparam==0",
                                                               checkboxInput("checkbox_anova_param", "Paramétrico", FALSE),
                                                               
                                                               wellPanel( style=" background-color:#2c3c54", height = 1,
                                                                          conditionalPanel(
                                                                            condition = " input.checkbox_anova_param==1", # input.checkbox_anova_hetero==0 &&
                                                                   checkboxInput("checkbox_anova_homoce", "Homocedastico", FALSE),
                                                                    ),
                                                                  conditionalPanel(
                                                                   condition = " input.checkbox_anova_param==1", # input.checkbox_anova_homoce==0 &&
                                                                   checkboxInput("checkbox_anova_hetero", "Heterodastico", FALSE),
                                                                  ),
                                                                 # ),
                                                             ),
                                                           #  conditionalPanel(
                                                           #  condition = "input.checkbox_anova_param==0",
                                                                 checkboxInput("checkbox_anova_noparam", "No Paramétrico", FALSE),
                                                           # ),
                                                           ),
                                                           
                                                           
                                                ),
                                                
                                                wellPanel( style=" background-color:#2c3c54",
                                                           h6("Comparaciones"),
                                                           checkboxInput("checkbox_Comparacion", "Comparación", FALSE),
                                                           conditionalPanel(
                                                             condition = "input.checkbox_Comparacion==1",
                                                             uiOutput("seleccion_metodo_PostHoc"),
                                                            #seleccion_fm_fa
                                                           ),
                                                          
                                                ),
                                                 actionButton("Boton_comparativa", "Realizar operación"),
                                               
                                                
                                              ),
                                              
                                              mainPanel(
                                                tags$style(".div_comparativa  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa1  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa2  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa3  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa4  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa5  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa6  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa7  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa8  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa9  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa10  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa11  {display: flex; flex-direction: column; align-items: center;}"),
                                                tags$style(".div_comparativa12  {display: flex; flex-direction: column; align-items: center;}"),
                                                
                                                
                                                tags$div(
                                                    h1("Comparativa de la satisfacción del factor"),
                                                  class = "div_comparativa11" ),
                                                
                                                  conditionalPanel(
                                                    condition = "input.checkbox_hipo==1",
                                                    hr(style="border-color:white; width:75%;  border:1px dashed;"),
                                                  ),
                                                tags$div(
                                                    uiOutput('output_headline_hipotesis_Test'),
                                                  class = "div_comparativa12" ),
                                                
                                                  conditionalPanel(
                                                    condition = "input.checkbox_hipo==1",
                                                    hr(style="border-color:white; width:75%;  border:1px dashed;"),
                                                  ),
                                                
                                                tags$div(
                                                  uiOutput('output_headline_normalidad'),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.checkbox_hipo==1",
                                                    tableOutput("table_normalidad"),
                                                    
                                                  ),
                                                  uiOutput('output_headline_comp_9'),
                                                  conditionalPanel(
                                                    condition = "input.checkbox_hipo==1",
                                                    #tableOutput("table_independencia_freq"),
                                                    tableOutput("table_independencia"), 
                                                  ),
                                                  
                                                  
                                                
                                                  uiOutput('output_headline_comp_0'),
                                                  conditionalPanel(
                                                    condition = "input.checkbox_hipo==1",
                                                    tableOutput("table_varianza"),
                                                  ),
                                                  
                                                  
                                                  class = "div_comparativa" ),
                                                
                                                
                                                
                                                conditionalPanel(
                                                  condition = "input.checkbox_hipo==1",
                                                  hr(style="border-color:white; width:75%;  border:1px dashed;"),
                                                ),
                                                  
                                                tags$div(
                                                  uiOutput('output_headline_comp_1'),
                                                  uiOutput('output_headline_varianza_1'),
                                                  #h3("T-Test"),
                                                  conditionalPanel(
                                                    condition = "input.checkbox_hipo==1",
                                                    tableOutput("table_ttest"),
                                                  ),
                                                  
                                                class = "div_comparativa9" ),
                                                  
                                                conditionalPanel(
                                                  condition = "input.checkbox_hipo==1",
                                                  plotOutput("Box_factor_agrupada"),
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = "input.checkbox_hipo==1",
                                                  hr(style="border-color:white; width:100%; height:5px;"),
                                                ),
                                                
                                                tags$div(
                                                  uiOutput('output_headline_anova_1'),
                                                class = "div_comparativa6" ),
                                                
                                                  
                                                  conditionalPanel(
                                                    condition = "input.checkbox_anova==1",
                                                    hr(style="border-color:white; width:75%;  border:1px dashed;"),
                                                  ),
                                                
                                                tags$div(
                                                  uiOutput('output_headline_anova_2'),
                                                
                                                  conditionalPanel(
                                                    condition = "input.checkbox_anova==1",
                                                    tableOutput("table_normalidad_anova"),
                                                    
                                                  ),
                                                  
                                                  class = "div_comparativa5" ),
                                                
                                                conditionalPanel(
                                                  condition = "input.checkbox_anova==1",
                                                  plotOutput("Box_residuals"),
                                                ),
                                                  
                                                conditionalPanel(
                                                    condition = "input.checkbox_anova==1",
                                                    plotOutput("Plot_hist_norm"),
                                                  ),
                                                
                                               
                                                conditionalPanel(
                                                  condition = "input.checkbox_anova==1",
                                                  hr(style="border-color:white; width:75%; border:1px dashed;"),
                                                ), 
                                                 
                                                tags$div(
                                                  
                                                
                                                  
                                                  uiOutput('output_headline_anova_3'),
                                                
                                                  conditionalPanel(
                                                    condition = "input.checkbox_anova==1",
                                                    tableOutput("table_durbin"),
                                                    
                                                  ),
                                                  #conditionalPanel(
                                                  #  condition = "input.checkbox_anova==1",
                                                  #  uiOutput('output_durbin'),
                                                  # ),
                                                  class = "div_comparativa1" ),
                                                 
                                                 conditionalPanel(
                                                    condition = "input.checkbox_anova==1",
                                                    plotOutput("Plot_residuals"),
                                                  ),
                                                
                                                conditionalPanel(
                                                  condition = "input.checkbox_anova==1",
                                                  hr(style="border-color:white; width:75%; border:1px dashed;"),
                                                ),
                                                
                                                tags$div(
                                                  
                                                  uiOutput('output_headline_anova_4'),
                                                  uiOutput('output_headline_anova_6'),
                                                  conditionalPanel(
                                                    condition = "input.checkbox_anova==1",
                                                    tableOutput("table_homocedasticidad"),
                                                   
                                                  ),
                                                  
                                                  class = "div_comparativa2" ),
                                                  conditionalPanel(
                                                    condition = "input.checkbox_anova==1",
                                                    plotOutput("Box_residuals_agrupada"),
                                                  ),
                                                  
                                                tags$div(
                                                  uiOutput('output_headline_anova_5'),
                                                  
                                                  conditionalPanel(
                                                     condition = "input.checkbox_anova==1",
                                                    tableOutput("table_desviaciones"),
                                                    
                                                  ),
                                                  
                                                  
                                                  class = "div_comparativa8" ),
                                                
                                                  
                                                
                                                conditionalPanel(
                                                  condition = "input.checkbox_anova==1",
                                                  hr(style="border-color:white; width:75%; border:1px dashed;"),
                                                ),
                                                
                                                tags$div(
                                                  uiOutput('output_headline_comp_5'),
                                                class = "div_comparativa7" ),
                                                
                                                conditionalPanel(
                                                  condition = "input.checkbox_anova==1",
                                                  
                                                  splitLayout(cellWidths = c("50%", "50%"), plotOutput("Plot_Anova"), plotOutput("Plot_Anova1")),
                                                  splitLayout(cellWidths = c("50%", "50%"), plotOutput("Plot_Anova2"), plotOutput("Plot_Anova3")),
                                                  #plotOutput("BoxPlot_comparativa"),
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = "input.checkbox_anova==1",
                                                  hr(style="border-color:white; width:75%; height:5px;"),
                                                ),
                                                  
                                                  tags$div(
                                                  uiOutput('output_headline_comp_3'),
                                                  uiOutput('output_headline_comp_8'),
                                                  #h3("Análisis de la Varianza (ANOVA)"),
                                                  conditionalPanel(
                                                    condition = "input.checkbox_anova==1",
                                                    tableOutput("table_Anova"),
                                                  ),
                                                  uiOutput('output_headline_comp_4'),
                                                 # h3("Tamaño Anova"),
                                                 conditionalPanel(
                                                   condition = "input.checkbox_anova==1",
                                                    tableOutput("table_tamano_Anova"),
                                                 ),
                                                  
                                                 
                                                 class = "div_comparativa3" ), 
                                                
                                                conditionalPanel(
                                                  condition = "input.checkbox_Comparacion==1",
                                                  hr(style="border-color:white; width:100%; height:5px;"),
                                                ),
                                                
                                                
                                                tags$div( 
                                                  
                                                  uiOutput('output_headline_comp_7'),
                                                  
                                                  uiOutput('output_headline_comp_6'),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.checkbox_Comparacion==1",
                                                    tableOutput("table_Bonferroni"),
                                                  ),
                                                  
                                                  conditionalPanel(
                                                    condition = "input.checkbox_Comparacion==1",
                                                    tableOutput("table_leyenda"),
                                                  ),
                                                  
                                                class = "div_comparativa4" ), 
                                                
                                                conditionalPanel(
                                                  condition = "input.checkbox_Comparacion==1",
                                                  plotOutput("plot_tuckey"),
                                                ),
                                                
                                                
                                                
                                                
                                                #plotOutput("Plot_Tukey"),
                                                
                                                #    tags$div(
                                                #      h3("Bonferroni"),
                                                #     tableOutput("table_Bonferroni"),
                                                #  class = "div_comparativa1" ),
                                                
                                              )
                                              
                                     )
                                     
                                    
                                     
                                    
                                   )
                                   
                          )
               )
               
               
               
    ) #Close outer tabsetPanel
    
    
    
  })
  
 
  
  observeEvent(input$Boton_vector,{
    
      x <- input$Select_factorial1 
      nombre_num_vector <- sample(1:5, 1)

      for (i in 1:length(x) ) {
        x3 <- sample(1:5, 1)
        if(i != 1){
          nombre_num_vector <-paste(nombre_num_vector, x3, sep=",")
        }
      }
      updateTextInput(session, "vec_predi", value = nombre_num_vector)
    
    
  })
  
  
  
  
 
  output$plot_mostrar <- reactive({
    is.element(input$Select_mer3, variables_disc_cuali)
  })
  
  
  output$PiePlot_mostrar <- reactive({
    is.element(input$Select_mer3, variables_disc_cuali)
  })
  

  
  output$histograma_mostrar <- reactive({
    is.element(input$Select_mer3, variables_continuas)
  })
  
  
  output$BoxPlot_mostrar <- reactive({
    is.element(input$Select_mer3, variables_continuas)
  })
  
  output$Estadistico_mostrar <- reactive({
    is.element(input$Select_mer3, c(variables_continuas,variables_discretas))
  })
  
  output$plot_discretas <- reactive({
    is.element(input$Select_mer3, c(variables_discretas))
  })
  
  output$plot_continuas <- reactive({
    is.element(input$Select_mer3, c(variables_continuas,variables_cualitativas,variables_ordinales))
  })
  
  
  output$boxplot_bidi_continuas1 <- reactive({
    is.element(input$Select_bidi1, c(variables_continuas)) 
  })
  
  output$boxplot_bidi_continuas2 <- reactive({
    is.element(input$Select_bidi2, c(variables_continuas)) 
  })
  
  output$pieplot_bidi_discreta1 <- reactive({
    is.element(input$Select_bidi1, c(variables_discretas,variables_cualitativas)) 
  })
  
  output$pieplot_bidi_discreta2 <- reactive({
    is.element(input$Select_bidi2, c(variables_discretas,variables_cualitativas)) 
  })
  
  output$plot_discretas_bidi1 <- reactive({
    is.element(input$Select_bidi1, c(variables_discretas))
  })
  
  output$plot_discretas_bidi2 <- reactive({
    is.element(input$Select_bidi2, c(variables_discretas))
  })
  
  output$plot_continua_bidi1 <- reactive({
    is.element(input$Select_bidi1, c(variables_cualitativas))
  })
  
  output$plot_continua_bidi2 <- reactive({
    is.element(input$Select_bidi2, c(variables_cualitativas))
  })
  
  outputOptions(output, "plot_discretas_bidi1", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_discretas_bidi2", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_continua_bidi1", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_continua_bidi2", suspendWhenHidden = FALSE)
  
  
  outputOptions(output, "boxplot_bidi_continuas1", suspendWhenHidden = FALSE)
  outputOptions(output, "boxplot_bidi_continuas2", suspendWhenHidden = FALSE)
  outputOptions(output, "pieplot_bidi_discreta1", suspendWhenHidden = FALSE)
  outputOptions(output, "pieplot_bidi_discreta2", suspendWhenHidden = FALSE)
  
  outputOptions(output, "plot_mostrar", suspendWhenHidden = FALSE)
  outputOptions(output, "PiePlot_mostrar", suspendWhenHidden = FALSE)
  outputOptions(output, "histograma_mostrar", suspendWhenHidden = FALSE)
  outputOptions(output, "BoxPlot_mostrar", suspendWhenHidden = FALSE)
  outputOptions(output, "Estadistico_mostrar", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_discretas", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_continuas", suspendWhenHidden = FALSE)
  
  
  
  
  
  output$Multi_Variables <- renderUI({
    
    
    
    if(input$Select_var_1 == "Atributos_Nominales"){
      
      unique_cualitativa <- unique(variables_cualitativas)
      return(multiInput(
        
        inputId = "Var_id",
        label = "", 
        width = "100%",
        choices = NULL,
        choiceNames = lapply(seq_along(unique_cualitativa),function(i) unique_cualitativa[i]),
        choiceValues = unique_cualitativa
      ))
  }
    
  if(input$Select_var_1 == "Continuas"){
    
   unique_continuas <- unique(variables_continuas)  
   return( multiInput(
      
      inputId = "Var_id",
      label = "", 
      width = "100%",
      choices = NULL,
      choiceNames = lapply(seq_along(unique_continuas),function(i) unique_continuas[i]),
      choiceValues = unique_continuas
    ))
    
  }
    
    if(input$Select_var_1 == "Discretas"){
      
      unique_discretas <- unique(variables_discretas) 
      return( multiInput(
        
        inputId = "Var_id",
        label = "", 
        width = "100%",
        choices = NULL,
        choiceNames = lapply(seq_along(unique_discretas),function(i) unique_discretas[i]),
        choiceValues = unique_discretas
      ))
    
    }
    
    if(input$Select_var_1 == "Atributos_Ordinales"){
      
      unique_ordinales <- unique(variables_ordinales)
      return(multiInput(
        
        inputId = "Var_id",
        label = "", 
        width = "100%",
        choices = NULL,
        choiceNames = lapply(seq_along(unique_ordinales),function(i) unique_ordinales[i]),
        choiceValues = unique_ordinales
      ))
    }
    
    
    
  })
  
  
  
  
  
  output$seleccion_variable_1 <- renderUI({
    
    req(input$files)
    lista <- list(variables_continuas,variables_discretas,variables_cualitativas)
    selectInput("Select_var_1", "Selecciona el tipo de variable", 
                choices  = c("Continuas", "Discretas", "Atributos_Nominales", "Atributos_Ordinales"),selected = NULL)
    
    
  })
  
  output$seleccion_variable_2 <- renderUI({
    
    req(input$files)
    lista <- list(variables_continuas,variables_discretas,variables_cualitativas)
    selectInput("Select_var_2", "Selecciona el tipo de variable", 
                choices  = c("Continuas", "Discretas", "Atributos_Nominales", "Atributos_Ordinales"),selected = NULL)
    
    
  })
  
  output$seleccion_rotacion_fa <- renderUI({
    
    req(input$files)
    selectInput("Select_rotacion_fa", "Selecciona el tipo de rotacion", 
                choices  = c("Ortogonal varimax" = "varimax", "Ortogonal quartimax" = "quartimax", "Ortogonal bentlerT" = "bentlerT",
                             "Ortogonal equamax" = "equamax", "Ortogonal varimin" = "varimin", "Ortogonal geominT" = "geominT",
                             "Ortogonal bifactor" = "bifactor", "Oblicua promax" = "promax", "Oblicua oblimin" = "oblimin",
                             "Oblicua simplimax" = "simplimax", "Oblicua bentlerQ," = "bentlerQ,", "Oblicua geominQ" = "geominQ",
                             "Oblicua biquartimin" = "biquartimin", "Oblicua cluster" = "cluster"),selected = NULL)
    
    #"Promax", "promax", "oblimin", "simplimax", "bentlerQ, "geominQ" and "biquartimin" and "cluster" 
    #"varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" and "bifactor" 
   # "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT", "bifactor"
  })
  
  
  output$seleccion_metodo_PostHoc  <- renderUI({
    
    req(input$files)
    
    if(input$checkbox_anova_noparam == TRUE){
      selectInput("Select_metodo_PostHoc", "Selecciona el método de comparación", 
                  choices  = c("Pairwill-wilcox"),selected = NULL)
    }else{
      selectInput("Select_metodo_PostHoc", "Selecciona el método de comparación", 
                  choices  = c("hsd", "bonferroni", "scheffe", "duncan", "Tukey"),selected = NULL)
    }
    
  })
  
  
  output$seleccion_fm_fa <- renderUI({
    
    req(input$files)
    selectInput("Select_fm_fa", "Selecciona el método de factorización", 
                choices  = c("ml","minres", "uls", "ols", "wls", "gls", "pa", "mle", "minchi", "minrank"),selected = NULL)
    
    
  })
  
  output$seleccion_cor_fa <- renderUI({
    
    req(input$files)
    selectInput("Select_cor_fa", "Selecciona el tipo de correlación", 
                choices  = c( "poly", "cor", "cov", "tet", "mixed"),selected = NULL)
    
    
  })
  
  
  observeEvent(input$intercambio_var,{
    

    input$Select_var_1
    input$Select_var_2
    
    variables_continuas <<- variables_continuas[variables_continuas != input$Var_id];
    variables_discretas <<- variables_discretas[variables_discretas != input$Var_id];
    variables_cualitativas <<- variables_cualitativas[variables_cualitativas != input$Var_id];
    variables_ordinales <<- variables_ordinales[variables_ordinales != input$Var_id];
    variables_disc_cont <<- variables_disc_cont[variables_continuas != input$Var_id];
    variables_disc_cont <<- variables_disc_cont[variables_discretas != input$Var_id];
    

   
    if(input$Select_var_2 == "Continuas" ){
      variables_continuas <<- c(variables_continuas,input$Var_id)
      variables_disc_cont <<- c(variables_disc_cont,input$Var_id)
    }
    
    else if(input$Select_var_2 == "Discretas" ){
      variables_discretas <<- c(variables_discretas,input$Var_id)
      variables_disc_cont <<- c(variables_disc_cont,input$Var_id)
    }
    
    else if(input$Select_var_2 == "Atributos_Nominales" ){
      variables_cualitativas <<- c(variables_cualitativas,input$Var_id)
    }
    
    else if(input$Select_var_2 == "Atributos_Ordinales" ){
      
    
      variables_ordinales <<- c(variables_ordinales,input$Var_id)
      
    }
   
  })
  
  
  
  
  variables_posibles <- c("NOTA10_SS","NOTA10_SUP", "OpciÃ³n", "CrÃ.d..FB.", "CrÃ.d..FT", "CrÃ.d..EP",
                          "CrÃ.d..Itin.", "Requisitos", "Itinerario", "NÃºm..Cred..faltan.req..Req.",
                          "CRED_SUPERADOS", "CRED_FALTAN_TOT.", "Sexo")
  
  
  variables_continuas <- c("NOTA10_SS","NOTA10_SUP","CRED_FALTAN_TOT.",
                           "CRED_SUPERADOS")
  
  
  variables_discretas <- c("OpciÃ³n", "CrÃ.d..FB.", "CrÃ.d..FT", "CrÃ.d..EP",
                           "CrÃ.d..Itin.", "Itinerario", "NÃºm..Cred..faltan.req..Req.")
  
  
  variables_cualitativas <- c("DescripciÃ³n.del.Ã.rea","NOTA10_SS_CAT", "NOTA10_SUP_CAT", "Requisitos", "Empresa","Sexo")
  
  variables_disc_cuali <-  c("OpciÃ³n", "CrÃ.d..FB.", "CrÃ.d..FT", "CrÃ.d..EP",
                             "CrÃ.d..Itin.", "Itinerario", "NÃºm..Cred..faltan.req..Req.",
                             "DescripciÃ³n.del.Ã.rea","NOTA10_SS_CAT", "NOTA10_SUP_CAT", "Requisitos", "Empresa","Sexo")
  
  variables_disc_cont <- c("NOTA10_SS","NOTA10_SUP")
  
  variables_ordinales <- c("Plaza")  # Como la Plaza que va de 1 a 80 
  
  #variable_nominal <- c("")   Estas son las cualitativas
 

  

  
  guardar_datos <- reactive ({
    
    
    if(is.null(input$files)){return()}
      prueba_text <- 0
      prueba_texto <- 0
    for(nr in 1:length(input$files[, 1])){
      file_name = input$files[[nr, 'name']]
      data <- read.csv(file = input$files[[nr, 'datapath']])
      
      imprimir <- paste(ruta,file_name,sep="")
  
      prueba_text <- data
      nombre_columnas <- colnames(data)
      
      for(nh in 1:length(nombre_columnas)){
        
        
        prueba_text[,nombre_columnas[nh]] <- iconv(prueba_text[,nombre_columnas[nh]], from="UTF-8", to="LATIN1")
      
        #prueba_text[,nombre_columnas[nh]] <- toupper(stri_trans_general(data[,nombre_columnas[nh]],"Latin-ASCII"))
        
      }

      write.csv(prueba_text, file = imprimir, row.names = F, fileEncoding = "LATIN1" )
      
      data1 <- read.csv(imprimir)
      
      prueba_texto <- data1
      
      for(nh in 1:length(nombre_columnas)){
        
        prueba_texto[,nombre_columnas[nh]] <- stri_trans_general(prueba_text[,nombre_columnas[nh]],"Latin-ASCII")
        
      }
      
      write.csv(prueba_texto, file = imprimir, row.names = F, fileEncoding = "UTF-8" )
      
    }
      
     
    
  })
  
  
  
  arreglar_header <- reactive ({
    
 
    
    List_of_file <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)

    
    for(nr in 1:length(List_of_file)){
      
      Fichero_csv <- paste(ruta,List_of_file[nr],sep="")
      
      data <- read.csv(Fichero_csv, encoding = "UTF-8")
      
      columnas <- colnames(data)
      
      j = 1
      
      
      for(i in 1:length(columnas)){
        selec_col <- columnas[i]
        
        if(class(data[, selec_col]) == "integer"){
          
          if(max(data[, selec_col]) > 10000000){
            
            if( (str_detect(selec_col, "Acad.c3..mico") == TRUE) | (str_detect(selec_col , "Tutores") == TRUE) | 
                (str_detect(selec_col, "academico") == TRUE) | (str_detect(selec_col, "Academico") == TRUE) ){
              columnas[i] <- "Dni Tutor Academico"
            }
            
            else if( (str_detect(selec_col, "externo") == TRUE) | (str_detect(selec_col, "Externo") == TRUE) ){
              columnas[i] <- "Dni Tutor Externo"
            }
            
            else{
              columnas[i] <- "Dni Alumno"
            }
          }
          
          if( (str_detect(selec_col, "CRED") == TRUE) | str_detect(selec_col, "FB.$") |
              str_detect(selec_col, "FT$") | str_detect(selec_col, "EP$") | str_detect(selec_col, "Itin.$") | 
              (str_detect(selec_col, "Cred") == TRUE)){
            
            if(str_detect(selec_col, "FB.$") == TRUE){
              columnas[i] <- "Creditos_FB"
            }
            
            else if(str_detect(selec_col, "FT$") == TRUE){
              columnas[i] <- "Creditos_FT"
            }
            
            else if(str_detect(selec_col, "EP$") == TRUE){
              columnas[i] <- "Creditos_EP"
            }
            
            else if(str_detect(selec_col, "Itin.$") == TRUE){
              columnas[i] <- "Creditos_Itin"
            }
            
            if(str_detect(selec_col, "Req.$") == TRUE){
              columnas[i] <- "Creditos_Req"
            }
            
            
          }
          
          
          if( (str_detect(selec_col, "^Valore") == TRUE | str_detect(selec_col, "Valora")  == TRUE) ){
            if(str_detect(selec_col, "externo") == TRUE | str_detect(selec_col, "Externo")  == TRUE
               | str_detect(selec_col, "acad" ) == TRUE | str_detect(selec_col, "Academico")  == TRUE){
              
              columnas[i] <- "Item"
              f <- toString(j)
              columnas[i] <- paste(columnas[i],f,sep="_")
              j = j + 1
              
            }
            
           # else if(str_detect(selec_col, "acad" ) == TRUE | str_detect(selec_col, "Academico")  == TRUE){
            #   columnas[i] <- "Item_TA"
              
            # }
            
            
            
            else if(str_detect(selec_col, "información") == TRUE){
              columnas[i] <- "Valoracion_Información"
              
            }
            
            else if(str_detect(selec_col, "charla") == TRUE){
              columnas[i] <- "Valoracion_Charla"
              
            }
            
            else if(str_detect(selec_col, "satisfacción") == TRUE){
              columnas[i] <- "Valoracion_Satisfacción"
              
            }
            
            else{
              columnas[i] <- "Valoracion"
            }
            
            if(str_detect(selec_col, "coordinación") == TRUE){
              columnas[i] <- "Valoracion_Coordinacion_Tutores"
              
            }
            
          }
          
        }
        
        else if(class(selec_col) == "character"){
          
        }
        
        if( columnas[i] == "Identificador.de.oferta."){
          columnas[i] <- 'Plaza'
        }
        
        
        if( columnas[i] == "Ã.rea") {
          columnas[i] <- "Area"
        }
        
        if( columnas[i] == "DescripciÃ³n.del.Ã.rea") {
          columnas[i] <- "Descripcion del area"
        }
        
        if(str_detect(selec_col, "Curso.Acadï") == TRUE) {
          columnas[i] <- "Curso Academico"
        }
        
        if(str_detect(selec_col, "Tï...tulo.de.la.oferta.") == TRUE) {
          columnas[i] <- "Titulo de la oferta"
        }
        
        if(str_detect(selec_col, "TÃ.tulo.de.la.oferta") == TRUE) {
          columnas[i] <- "Titulo de la oferta"
        }
        
        if(str_detect(selec_col, "Privada.") == TRUE) {
          columnas[i] <- "Privada/Publica"
        }
     
        if(str_detect(selec_col, "empresa.con.las.TIC") == TRUE) {
          columnas[i] <- "Relacion de la empresa con las TIC"
        }
        
      
       
      }
      
  
      
      colnames(data) <- columnas
      
      for(nh in 1:length(colnames(data))){
        
        colnames(data) <- iconv(colnames(data), from="UTF-8", to="LATIN1")
        colnames(data) <- stri_trans_general(colnames(data),"Latin-ASCII")
        
      }
    
     
       columnas <- colnames(data)

       
      for(i in 1:length(columnas)){
        selec_col <- columnas[i]
       
        
        if(str_detect(selec_col, "NOTA") == TRUE){
          for (i in 1:nrow(data)){
            data[i, selec_col] <- gsub(",", ".",data[i, selec_col])
            data[i, selec_col] <- round(as.numeric(data[i, selec_col]),2)
          }
          
          data[,selec_col] <- as.factor(data[,selec_col])
          
          #data[, selec_col] <- round(as.numeric(data[, selec_col]),2) 
        }
        
        else if((str_detect(selec_col, "Itinerario") == TRUE) | (str_detect(selec_col, "itinerario") == TRUE)){
          for (i in 1:nrow(data)){
            if(data[i, selec_col] == "1"){
              data[i, selec_col] <- "Computacion"
            }
            else if(data[i, selec_col] == "2"){
              data[i, selec_col] <- "Ingenieria de Computadores"
            }
            else if(data[i, selec_col] == "3"){
              data[i, selec_col] <- "Ingenieria del Software"
            }
            else if(data[i, selec_col] == "4"){
              data[i, selec_col] <- "Sistemas de Informacion"
            }
            else if(data[i, selec_col] == "5"){
              data[i, selec_col] <- "Tecnologias de la Informacion"
            }
            
          }
        }
        
        
      }
      
     
      write.csv(data, file = Fichero_csv, row.names = F)
      
     
    }
    
  })
  
  añadir_variables <- reactive ({
    
    List_of_file <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    var_discretas <- vector()
    var_continuas <- vector()
    var_cualitativas <- vector()
    var_ordinal <- vector()
    
    for(nr in 1:length(List_of_file)){
      Fichero_csv <- paste(ruta,List_of_file[nr],sep="")
      data <- read.csv(Fichero_csv)
      columnas <- colnames(data)
      for(i in 1:length(columnas)){
        selec_col <- columnas[i]
        if(class(data[, selec_col]) == "integer"){
          vals <- (data[, selec_col])
          a <- unique(vals)
          a <- length(a)
          
          if((str_detect(selec_col, "Itinerario") == TRUE) | (str_detect(selec_col, "itinerario") == TRUE)){
          
            var_cualitativas <-  c(var_cualitativas, selec_col)
            
          }
          
          
          
          else if(a < 12){
            
            
            var_discretas <-  c(var_discretas, selec_col)
            variables_disc_cont <- c(variables_disc_cont, selec_col)
            
            
          }
          
          else if(a >= 12){

            if(data[1, selec_col] >= 10000000){
              var_ordinal <- c(var_ordinal, selec_col)
            }
            else{
              
              
              var_continuas <-  c(var_continuas, selec_col)
              variables_disc_cont <- c(variables_disc_cont, selec_col)
            }
          }
         # data[, selec_col] <- round(data[, selec_col],2)
          
        }
        
        else if(class(selec_col) == "character"){
          
          if((str_detect(selec_col, "identifica") == TRUE) | (str_detect(selec_col, "Identifica") == TRUE)){
            var_ordinal <- c(var_ordinal, selec_col)
          }
          
          else if(str_detect(selec_col, "Plaza") == TRUE){
            var_ordinal <- c(var_ordinal, selec_col)
          }
          
          else if(str_detect(selec_col, "Curso.Aca") == TRUE){
            var_ordinal <- c(var_ordinal, selec_col)
          }
          
          else if((str_detect(selec_col, "NOTA") == TRUE) ){
            

            var_continuas <- c(var_continuas, selec_col)
            variables_disc_cont <- c(variables_disc_cont, selec_col)
          }
          
          else{
            var_cualitativas <-  c(var_cualitativas, selec_col)
          }
          
        }
        
      }
      
    }

    
   
    
    
    variables_disc_cuali <<- rbind(var_discretas,var_cualitativas)
    variables_discretas <<- var_discretas
    variables_continuas <<- var_continuas
    variables_cualitativas <<- var_cualitativas
    variables_ordinales <<- var_ordinal
    #assign("variables_discretas", "var_discretas", envir = .GlobalEnv)
    #assign("variables_continuas", "var_continuas", envir = .GlobalEnv)
    #assign("variables_cualitativas", "var_cualitativas", envir = .GlobalEnv)
    
  })
  
  
  
  values <- reactiveValues(df = NULL)
  
  output$filedf2 <- renderTable({
    if(is.null(input$files)){validate(need(input$files!= "", ""))}
    input$files$datapath # the file input data frame object that contains the file attributes
  })
  
  output$plot <- renderPlot({
    
    
    if(is.element(input$Select_mer3, variables_disc_cuali) == TRUE){
    
   
    for(nr in 1:length(variables_disc_cuali)){
      
      
      if((input$Graficos_box == TRUE) | (input$Todos_box == TRUE) ){
        
        if( input$Select_mer3 == variables_disc_cuali[nr]){
          Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
          a <- read.csv(Fichero_csv)
          
          
          vals <- (a[, input$Select_mer3])

          nuevo_df <- data.frame(Leyenda = c(vals)) 
          datos <- (nuevo_df %>% group_by(Leyenda) %>% tally())
          #datos$Leyenda <- as.character(datos$Leyenda )
          
          # Barras
          
          valores_eje_x <- as.numeric(datos$Leyenda)

          
          p <- ggplot(datos, aes(x= Leyenda, y=n)) +
                     geom_bar(stat="identity", fill="orangered")+
                     geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
                     scale_fill_brewer() +
                     theme(text = element_text(size=14),
                           panel.grid = element_line(color = "#4c5b6b",
                                                     size = 0.75,
                                                    linetype = 2),

                           panel.background = element_rect(fill = "#2b3e50"),
                           plot.background = element_rect(fill = "#2b3e50"),
                           axis.text.x = element_text(size = 14),
                           axis.title = element_text(size = 14),
                           axis.text = element_text(size = 14),
                           axis.text.y = element_text(size = 14),legend.text = element_text(size = 14),
                           legend.background = element_rect(fill = "#2b3e50"),
                           plot.title = element_text(
                             size=rel(1.5), 
                             vjust=2, 
                             face="bold", 
                             color="black", 
                             hjust = 0.5,
                             lineheight=1.5)) +
                     xlab(input$Select_mer3) + ylab("Cantidades")    
          
          
          if(is.numeric(datos$Leyenda)){
            return (p + scale_x_continuous(breaks = datos$Leyenda))
          }else{
            return (p + scale_x_discrete(breaks = datos$Leyenda))
          }
        }
      }
      
    }
    return()
      
    }
  })
  
  output$PiePlot <- renderPlot({
    
    if(is.element(input$Select_mer3, variables_disc_cuali) == TRUE){
  
    for(nr in 1:length(variables_disc_cuali)){
      
      if((input$Graficos_box == TRUE) | (input$Todos_box == TRUE)){
        
        if( input$Select_mer3 == variables_disc_cuali[nr]){
          Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
        
          a <- read.csv(Fichero_csv) #, stringsAsFactors=FALSE
      
          
          #b = a[, input$Select_mer3]
          #table(cut(b, break = 6))
          
          
          vals <- (a[, input$Select_mer3])
          nuevo_df <- data.frame(Leyenda = c(vals))
          datos <- (nuevo_df %>% group_by(Leyenda) %>% tally())
          
          
         
          
         
          return( ggplot(datos, aes(x = "", y = n, fill = as.factor(Leyenda))) +
                   geom_col(color = "black") +
                   geom_text(aes(label = n),
                             position = position_stack(vjust = 0.5), color = "black",size = 5) +
                   coord_polar(theta = "y") +
                   scale_fill_brewer(palette = "Set1") +
                   theme(text = element_text(size=14),
                       axis.text.x = element_text(size = 14),
                       axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.text.y = element_text(size = 14),legend.text = element_text(size = 14),
                       axis.ticks = element_blank(),
                       panel.grid = element_blank(),
                       panel.background = element_rect(fill = "#2b3e50"),
                       plot.background = element_rect(fill = "#2b3e50"),
                       legend.background = element_rect(fill = "#2b3e50"),
                       plot.title = element_text(
                         size=rel(1.5), 
                         vjust=2, 
                         face="bold", 
                         color="black", 
                         hjust = 0.5,
                         lineheight=1.5)) + guides(fill = guide_legend(title = input$Select_mer3)) )
        
                    
          
          
          #return (ggplot(a, aes(x = "",y = a[, input$Select_mer3] , fill = a[, input$Select_mer3] )) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0) ) 
          
        }
      }
    }
    return()
    }
  })
  
  output$HistogramPlot <- renderPlot({
    
    if(is.element(input$Select_mer3, variables_continuas) == TRUE){

    for(nr in 1:length(variables_continuas)){
      if((input$Graficos_box == TRUE) | (input$Todos_box == TRUE)){
        
        if( input$Select_mer3 == variables_continuas[nr]){
          
          Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
          a <- read.csv(Fichero_csv)
          
          
          k <- nclass.Sturges(a[[input$Select_mer3]])
          
          vals <- (a[, input$Select_mer3])
          
          nuevo_df <- data.frame(Leyenda = c(vals)) 
          
          datos <- (nuevo_df %>% group_by(Leyenda) %>% tally())
          datos$Leyenda <- as.character(datos$Leyenda )
          
          A = diff(range(a[[input$Select_mer3]])) / k
          i = min(a[[input$Select_mer3]])
          L <- i - 0.05 + A *(0:k)
          L_inferior = L[0:k]
         
          
          #############################################################3 
          
          
          
          p<- ggplot(a, aes(x=vals))+ geom_histogram(color="darkblue", fill="orangered", alpha=0.6, breaks = L_inferior) +
            xlab(input$Select_mer3) + ylab("Cantidades") + theme(panel.grid = element_line(color = "#4c5b6b",
                                                                                   size = 0.75,linetype = 2),
                                                                 panel.background = element_rect(fill = "#2b3e50"),
                                                                 plot.background = element_rect(fill = "#2b3e50"),
                                                                 axis.text.x = element_text(size = 14),
                                                                 axis.title = element_text(size = 14),
                                                                 axis.text = element_text(size = 14),
                                                                 axis.text.y = element_text(size = 14),legend.text = element_text(size = 14),
                                                                 legend.background = element_rect(fill = "#2b3e50")) +  scale_x_continuous(breaks = round(L_inferior,digits = 2))
          return(p)
        }
        
        
      }
      
    }
    return()
    }
  })
  
  
  output$BoxPlot_percepcion <- renderPlot({
    
    matrices_ml <- describe_ML()
    data1 <- matrices_ml[str_detect(matrices_ml[,as.numeric(input$inSlider)], "Item"), ]
    
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    archivo <- read.csv(Fichero_csv)
    
    pruebas_ml <- subset(archivo, select = data1[,as.numeric(input$inSlider)] )
    
    c <- archivo[,input$Select_compa_percep]
    
    
    pruebas_ml <- cbind(c,pruebas_ml)

    ggplot(pruebas_ml, aes(x=pruebas_ml$c, y=pruebas_ml[, input$Select_compa_boxplot])) + 
      geom_boxplot() + labs(x = input$Select_compa_percep, y = input$Select_compa_boxplot) + 
      theme (plot.title = element_text(
        size=rel(1.5), 
        vjust=2, 
        face="bold", 
        color="black", 
        hjust = 0.5,
        lineheight=1.5),
        panel.grid = element_line(color = "#4c5b6b",
                                  size = 0.75,linetype = 2),
        panel.background = element_rect(fill = "#2b3e50"),
        plot.background = element_rect(fill = "#2b3e50"),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.text.y = element_text(size = 14),legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "#2b3e50"),
      )
    
  })
  
  output$BoxPlot_comparativa <- renderPlot({
    
    matrices_ml <- describe_ML()
    data1 <- matrices_ml[str_detect(matrices_ml[,as.numeric(input$inSlider)], "Item"), ]
    
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    archivo <- read.csv(Fichero_csv)
    
    pruebas_ml <- subset(archivo, select = data1[,as.numeric(input$inSlider)] )
    
    c <- archivo[,input$Select_compa_percep]
    
    
    pruebas_ml <- cbind(c,pruebas_ml)
    
    ggplot(pruebas_ml, aes(x=pruebas_ml$c, y=pruebas_ml[, input$Select_compa_boxplot])) + 
      geom_boxplot() + labs(x = input$Select_compa_percep, y = input$Select_compa_boxplot) + 
      theme (plot.title = element_text(
        size=rel(1.5), 
        vjust=2, 
        face="bold", 
        color="black", 
        hjust = 0.5,
        lineheight=1.5),
        panel.grid = element_line(color = "#4c5b6b",
                                  size = 0.75,linetype = 2),
        panel.background = element_rect(fill = "#2b3e50"),
        plot.background = element_rect(fill = "#2b3e50"),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.text.y = element_text(size = 14),legend.text = element_text(size = 14),
        legend.background = element_rect(fill = "#2b3e50"),
      )
    
  })
  
  
  
  output$BoxPlot <- renderPlot({
    
    if(is.element(input$Select_mer3, variables_continuas) == TRUE){
    

    for(nr in 1:length(variables_continuas)){
      if((input$Graficos_box == TRUE) | (input$Todos_box == TRUE)){ 
        if( (input$Select_mer3 == variables_continuas[nr])){
          
          
          Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")

          a <- read.csv(Fichero_csv)
          vals <- (a[, input$Select_mer3])
          nuevo_df <- data.frame(Leyenda = c(vals)) 
          datos <- (nuevo_df %>% group_by(Leyenda) %>% tally())
          datos$Leyenda <- as.character(datos$Leyenda )
          
          len <- length(vals)
          
          p <- ggplot(data = a, aes(x = a[, input$Select_mer3] )) + 
                   geom_boxplot() +
                   labs(x = input$Select_mer3,y = "") + 
                   theme (plot.title = element_text(
                                                    size=rel(1.5), 
                                                    vjust=2, 
                                                    face="bold", 
                                                    color="black", 
                                                    hjust = 0.5,
                                                    lineheight=1.5),
                          panel.grid = element_line(color = "#4c5b6b",
                                                    size = 0.75,linetype = 2),
                          panel.background = element_rect(fill = "#2b3e50"),
                          plot.background = element_rect(fill = "#2b3e50"),
                          axis.text.x = element_text(size = 14),
                          axis.title = element_text(size = 14),
                          axis.text = element_text(size = 14),
                          axis.text.y = element_text(size = 14),legend.text = element_text(size = 14),
                          legend.background = element_rect(fill = "#2b3e50"),
                          )
                    
          
          return(p)
                 
          
          #return ( ggplot(a, aes(x = a[, input$Select_mer3], y = a[, input$Select_mer4])) + geom_boxplot() + geom_point(data = a, x = 0, colour='red', size=2.5) +
          #  labs(x = input$Select_mer3, y = input$Select_mer4) )
        }
      }
      
    }
    return()
    }
  })
  
  
  
  output$selectfile <- renderUI({
    if(is.null(input$files)){return()}
    
    
    guardar_datos()
    
    
    arreglar_header()
    

    añadir_variables()
    

    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)

  
    
    selectInput("Select_visualizar", "Selecciona la variable", 
                choices  = List_of_file_paths ,selected = NULL)
    

    
  })
  
  output$selectfile_3 <- renderUI({
    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_3", "Archivo a unir", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  
  output$selectfile_5 <- renderUI({
    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_5", "Archivo a unir", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  output$selectfile_4 <- renderUI({
    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_4", "Archivo a unir", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  output$selectfile_1 <- renderUI({
    if(is.null(input$files)) {return()}

    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_1", "Archivo a unir", 
                choices  = List_of_file_paths ,selected = NULL,multiple = TRUE)
    
  })
  
  
  output$selectfile_2 <- renderUI({

    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_2", "Archivo a unir", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  output$selectfile_arreglo <- renderUI({

    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_arreglo", "Archivo a unir", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  output$selectfile_correcion <- renderUI({
    
    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_correcion", "Archivo a unir", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  output$selectfile_categorizar <- renderUI({
    
    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_categorizar", "Archivo a unir", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  output$selectfile_bidi <- renderUI({
    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_bidi", "Archivo a unir", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  
  
  output$selectfile_factorial <- renderUI({
    
    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_factorial", "Seleccione el archivo", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  output$selectfile_percepcion <- renderUI({
    
    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path = ruta, pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_percepcion", "Archivo a factorizar", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  output$selecciom_factorial1 <- renderUI({
    
    
    req(input$files)
    
    
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    
    #selectInput("Select_factorial1", "Selecciona la variable", 
    # choices  = colnames(read.csv(Fichero_csv)),selected = NULL, multiple = TRUE)
    
    pickerInput("Select_factorial1","Selecciona la variable", 
                choices= colnames(read.csv(Fichero_csv)), options = list(`actions-box` = TRUE, style = "btn-link"),multiple = T)
    
    
  })
  
  output$selecciom_comparativa1 <- renderUI({

    req(input$files)
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    
    pickerInput("Select_compa1","Seleccionar variable de agrupamiento", 
                choices= c("Sexo","Cuatrimestre","Opcion","Itinerario",
                           "Privada.Publica", "Formato","Var_Escala","Curso.Academico"), 
                options = list(`actions-box` = TRUE, style = "btn-link"),multiple = T)
    

    #selectInput("Select_compa1", "Seleccionar variable de agrupamiento", 
    #            choices  = c("Sexo","Cuatrimestre","Opcion","Itinerario",
    #                         "Privada Publica", "Formato"),selected = NULL)
    
  })
  

  
  output$selecciom_comparativa_percep <- renderUI({
    
    req(input$files)
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    
    selectInput("Select_compa_percep", "Seleccionar variable de agrupamiento", 
                choices  = c("Sexo","Cuatrimestre","Opcion","Itinerario",
                             "Privada.Publica", "Formato", "Var_Escala","Curso.Academico"),selected = NULL)
    
  })
  
  
  output$selecciom_comparativa_boxplot <- renderUI({
    
    matrices_ml <- describe_ML()
    data1 <- matrices_ml[str_detect(matrices_ml[,as.numeric(input$inSlider)], "Item"), ]
    
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    archivo <- read.csv(Fichero_csv)
    
    pruebas_ml <- subset(archivo, select = data1[,as.numeric(input$inSlider)] )
    
    selectInput("Select_compa_boxplot", "Selector de item de ese factor para la caja y patilla", 
                choices  = colnames(pruebas_ml),selected = NULL)
    
  })

  

  
  output$selecciom_percepcion <- renderUI({
    
    
    req(input$files)
    
    
    Fichero_csv <- paste(ruta,input$Select_Arc_percepcion,sep="")

    pickerInput("Select_percepcion","Selecciona la variable", 
                choices= colnames(read.csv(Fichero_csv)), options = list(`actions-box` = TRUE, style = "btn-link"),multiple = T)
    
    
  })
  
  output$selecciom_factorial2  <- renderUI({
    
    req(input$files)
    
    
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    
    selectInput("Select_factorial1", "Selecciona la variable", 
                choices  = colnames(read.csv(Fichero_csv)),selected = NULL)
    
    
  })
  
  
  
  
  
  funcion_tabla_policorica <- function() {

    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    
    daton <- data.frame()
    
    for (i in 1:length(input$Select_factorial1)){
      algo <- data.frame("cambio" = data1[, input$Select_factorial1[i]] )
      names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_factorial1[i] ) 
      if(i == 1){
        daton <- algo
      }else{
        daton <- bind_cols(daton, algo)
      }
 
    }

    result_n_factors <- fa.parallel(daton,n.obs= 200,fa="fa",fm="minres")


    ASIFactor <- fa(daton,nfactors = input$rowfactorial ,fm = input$Select_fm_fa, rotate = input$Select_rotacion_fa, cor = input$Select_cor_fa)

    return(ASIFactor) 
  }
  
  funcion_datos_fiabilidad <- function() {
    
    Fichero_csv <- paste(ruta,input$Select_Arc_graph_cat,sep="")
    data1 <- read.csv(Fichero_csv)
    
    daton <- data.frame()
    
    for (i in 1:length(input$Select_graph_cat1)){
      algo <- data.frame("cambio" = data1[, input$Select_graph_cat1[i]] )
      names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_graph_cat1[i] ) 
      if(i == 1){
        daton <- algo
      }else{
        daton <- bind_cols(daton, algo)
      }
      
    }
    return(daton)
  }
  
  output$Plot_fadiagram <- renderPlot({
    
    ASIfactor <- funcion_tabla_policorica()
    
    a <- c()
    for(i in 1:length(colnames(ASIfactor$loadings))){
      b <-paste("FA",i,sep = "")
      a <- c(a,b)
      
    }
    colnames(ASIfactor$loadings) <- a
    fa.diagram(ASIfactor, main = " ",rsize = 2, marg = c(3,3,3,3))
    
  })
  
  funcion_independencia <- function() {
    if(length(input$Select_compa1) == 1){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      
      data1 <- read.csv(Fichero_csv)
      
      
      
      a <- ASIfactor[["scores"]][,as.numeric(input$inSlider1)]
      b <- data1[,input$Select_compa1[1]]
      c <- data.frame("var1" = b , "var2" = a )
      unico <- unique(b)
      d <- c[c$var1==unico[1],]
      e <- c[c$var1==unico[2],]
      
      e$var1 <- NULL
      d$var1 <- NULL
      

      f <- cut(as.numeric(unlist(e)),breaks = 5)

      x <- data.frame( "kek" = h)
      
      
      data_frec <- data.frame("1" = c(0,0,0,0,0) )
      for(i in 1:length(unlist(e))){
        
        for(j in 1:5){
          separador <- str_split(x$kek.f[j],",", simplify=T)
          menor <- as.numeric(substr(separador[1], 2, nchar(separador[1])) )
          mayor <- as.numeric(substr(separador[2], 1, nchar(separador[2])-1) )
          
          if(d[i,] > menor & d[i,] <= mayor){

            data_frec[j,1] <- ( data_frec[j,1] + 1 )
          }
        }

      }
      
      data <- cbind(x,data_frec)
      
      
      #data$X1[i] <- as.character(data$X1[i])


     
      colnames(data) <- c(" ",unico[1],unico[2])

      return(data)
      
      
    }
    
  }
  
  funcion_aov_ <- function() {
    
    if(length(input$Select_compa1) == 1){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      
      data1 <- read.csv(Fichero_csv)
      
      
        
      if(length(input$Select_compa1) == 1){
        fm = aov( lm(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] ) )
        
        
      }else if(length(input$Select_compa1) == 2){
        fm = aov( lm(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]] ) )
        
        
      }else if(length(input$Select_compa1) == 3){
        fm = aov( lm(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]]
                     + data1[,input$Select_compa1[3]] ) )
        
        
      }else if(length(input$Select_compa1) == 4){
        fm = aov( lm(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]]
                     + data1[,input$Select_compa1[3]] + data1[,input$Select_compa1[4]] ) )
        
        
      }else if(length(input$Select_compa1) == 5){
        fm = aov( lm(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]]
                     + data1[,input$Select_compa1[3]] + data1[,input$Select_compa1[4]] + data1[,input$Select_compa1[5]] ) )
        
      }else if(length(input$Select_compa1) == 6){
        fm = aov( lm(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]]
                     + data1[,input$Select_compa1[3]] + data1[,input$Select_compa1[4]] + data1[,input$Select_compa1[5]] + data1[,input$Select_compa1[6]] ) )
        
      }
      
      
      return(fm)
    }
    
    
    
  }
  
  funcion_krustal_ <- function() {
    
    if(length(input$Select_compa1) == 1){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      
      data1 <- read.csv(Fichero_csv)
      
      
      if(length(input$Select_compa1) == 1){
        fm = kruskal.test( ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]]  )
        
        
      }else if(length(input$Select_compa1) == 2){
        fm = kruskal.test( ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]]  )
        
        
      }else if(length(input$Select_compa1) == 3){
        fm = kruskal.test( ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]]
                     + data1[,input$Select_compa1[3]]  )
        
        
      }else if(length(input$Select_compa1) == 4){
        fm = kruskal.test( ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]]
                     + data1[,input$Select_compa1[3]] + data1[,input$Select_compa1[4]]  )
        
        
      }else if(length(input$Select_compa1) == 5){
        fm = kruskal.test( ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]]
                     + data1[,input$Select_compa1[3]] + data1[,input$Select_compa1[4]] + data1[,input$Select_compa1[5]]  )
        
      }else if(length(input$Select_compa1) == 6){
        fm = kruskal.test( ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1[1]] + data1[,input$Select_compa1[2]]
                     + data1[,input$Select_compa1[3]] + data1[,input$Select_compa1[4]] + data1[,input$Select_compa1[5]] + data1[,input$Select_compa1[6]]  )
        
      }
      
      return(fm)
    }
    
  }

  output$Plot_hist_norm <- renderPlot({

    if(length(input$Select_compa1) == 1){
    
      fm <- funcion_aov_()
       hist(fm$residuals, xlab = "Residuos", ylab = "Densidad", main = "Histograma",freq = FALSE,ylim=c(0,1), cex.main = 2,font.main = 1)
       lines(density(fm$residuals, bw = 0.75), col="red", lwd=2) 
      
      
      #residuos <- data.frame("resi" = ASIfactor$residual)
      #return(hist(ASIfactor$residual,xlab="Residuos",col = "blue4" ,main = "Histograma") )
    }
    
  })
  
  
  output$Plot_residuals <- renderPlot({

    if(length(input$Select_compa1) == 1){
      
      fm <- funcion_aov_()
      

      return(plot(fm$residuals, ylab = "Residuos", xlab = "Sujeto", main = "Gráfico de residuales", cex.main = 2,font.main = 1) )
    }
  
    
  })
  
  output$Box_residuals <- renderPlot({

    if(length(input$Select_compa1) == 1){

      fm <- funcion_aov_()
      
      return( boxplot(fm$residuals, main = "Gráfico de caja y patilla", ylab = "Residuos", cex.main = 2,font.main = 1) )
      
    }
    
  })
  
  output$Box_factor_agrupada <- renderPlot({
    
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    num = nrow(distinct(data1[input$Select_compa1])) 
    if(num <= 2){
     
      if(length(input$Select_compa1) == 1){
        
        ASIfactor <- funcion_tabla_policorica()
        Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
        data1 <- read.csv(Fichero_csv)
        fm <- funcion_aov_()
        a <- paste("Satisfacción  FA",input$inSlider1)
        return( boxplot(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1], main = "Gráfico de caja y patilla", ylab = a, xlab = input$Select_compa1, cex.main = 2,font.main = 1) )
      }
    }
    
  })
  
  Tabla_Leyenda <- eventReactive(input$Boton_comparativa,{
    
    if(length(input$Select_compa1) == 1){
      
      if(input$Select_metodo_PostHoc !="Pairwill-wilcox"){
      
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
  
      daton <- data.frame( "score" = ASIfactor[["scores"]][,as.numeric(input$inSlider1)], "agrupa" = data1[,input$Select_compa1[1]])

      
      unico <- unique(daton$agrupa)

      for(i in 1: length(unico) ){
        if(i == 1){
          data <- data.frame("Niveles" = i, "Etiquetas" = unico[i])
        }else{
          data_aux <- data.frame("Niveles" = i, "Etiquetas" = unico[i])
          data <- rbind(data,data_aux)
        }
      }
      
 
      return(data)
      }
    }
    
  })
  
  output$plot_tuckey <- renderPlot({
    if(length(input$Select_compa1) == 1){
      if(input$Select_metodo_PostHoc !="Pairwill-wilcox"){
   
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      

      if(length(input$Select_compa1) == 1){
        daton <- data.frame( "score" = ASIfactor[["scores"]][,as.numeric(input$inSlider1)], "agrupa" = data1[,input$Select_compa1[1]])
      
        unico <- unique(daton$agrupa)
        for(i in 1: length(unico) ){
          daton$agrupa[daton$agrupa == unico[i]] <- i
        }
        
        fm = aov( lm(daton$score ~ daton$agrupa)  )
        if( input$Select_metodo_PostHoc == "Tukey"){
          a <- TukeyHSD(fm)
        }else{
          a <- PostHocTest(fm, which = NULL,
                            method = input$Select_metodo_PostHoc,
                            conf.level = 0.95, ordered = FALSE)
        }
        max_tuck <- round(max(abs(unlist(a))),0)

        plot(a, col = "red", cex.axis=0.75,xlim = c(-max_tuck, max_tuck),ann = FALSE ) # , col = rgb(0.36, 0.35, 0.35), cex.axis=0.75
       #plot(archivo_csv[, input$Select_bidi1], archivo_csv[, input$Select_bidi2], pch = 19, col = "black",
        #     xlab = input$Select_bidi1, ylab = input$Select_bidi2 )
        # https://rpubs.com/Edbbio/885399
        
      }
      }
    }
  })
  
  output$Box_residuals_agrupada <- renderPlot({
    

    
    if(length(input$Select_compa1) == 1){
      
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      fm <- funcion_aov_()
      
      return( boxplot(fm$residuals ~ data1[,input$Select_compa1], main = "Gráfico de caja y patilla", ylab = "Residuos", xlab = input$Select_compa1, cex.main = 2,font.main = 1) )
      
    }
    
  })
  
  output$Plot_Anova <- renderPlot({
    
    
    if(length(input$Select_compa1) == 1){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      
      data1 <- read.csv(Fichero_csv)
      
      fm <- funcion_aov_()
      return(plot(fm, 1, sub = ""))
      
    }
    
  })
  
  output$Plot_Anova1 <- renderPlot({
    
    if(length(input$Select_compa1) == 1){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      
      data1 <- read.csv(Fichero_csv)

      fm <- funcion_aov_()
      return(plot(fm, 2, sub = ""))
    
    }
    
  })
  
  output$Plot_Anova2 <- renderPlot({
    
    if(length(input$Select_compa1) == 1){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      
      data1 <- read.csv(Fichero_csv)
      

      
      fm <- funcion_aov_()
      return(plot(fm, 3, sub = ""))
      
    }
    
  })
  
  output$Plot_Anova3 <- renderPlot({
    
    if(length(input$Select_compa1) == 1){
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      
      data1 <- read.csv(Fichero_csv)
      

      
      fm <- funcion_aov_()
      return(plot(fm, 5, sub = ""))
      
    }
    
  })
  
  output$Plot_loadings <- renderPlot({
    
    
    if(input$Fact_box == 1){
      ASIfactor <- funcion_tabla_policorica()
      
      cadena_texto <- row.names(ASIfactor[["loadings"]])
      cadena_texto <- sub('Item_','',cadena_texto) 
      
      
      Carga <- ASIfactor[["loadings"]]
      nombre <-colnames(Carga)
      tamano <- length(colnames(Carga))
      
      for (i in 1:tamano) {
        nombre[i] <- "Saturación_FA"
        f <- toString(i)
        nombre[i] <- paste(nombre[i],f,sep="")
      }
      colnames(Carga) <- nombre
      
      # Revisar el ylim del plot
      BEBE <-plot(Carga,xlim = c(-1,1),ylim = c(-1,1))
      text(ASIfactor[["loadings"]], cadena_texto, cex=0.8, pos=4, col="white") 
      abline(h=0, v=0, col="white")
      return( BEBE )
    }
    
  })
  
  
  output$Plot_Sedimentacion <- renderPlot({
    
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    
    daton <- data.frame()
    
    for (i in 1:length(input$Select_factorial1)){
      algo <- data.frame("cambio" = data1[, input$Select_factorial1[i]] )
      names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_factorial1[i] ) 
      if(i == 1){
        daton <- algo
      }else{
        daton <- bind_cols(daton, algo)
      }
      
    }
    
   #return(scree(daton,main = " ") )
    
    results <- prcomp(daton, scale = TRUE)
    var_explained = results$sdev^2 / sum(results$sdev^2)
    
    par(cex.axis=4.0)
    screeplot <- qplot(c(1:length(daton)), var_explained) + 
      geom_line() + 
      xlab("Número de factores") +  
      ylab("Autovalores de factores") +
      ggtitle(" ") +
      ylim(0, 1) +
      geom_hline(yintercept=0.10) +
      theme(axis.title = element_text(size = 14)) +
      scale_x_continuous(breaks=(1:length(input$Select_factorial1)) )
    

    return(screeplot)
    
  })
  
  output$Plot_Puntuaciones <- renderPlot({
    
    
    if(input$Puntuaciones_box == 1 && input$vec_predi == ""){

        ASIfactor <- funcion_tabla_policorica()
        
        a <- c()
        for(i in 1:length(colnames(ASIfactor$loadings))){
          b <-paste("Satisfacción_FA",i,sep = "")
          a <- c(a,b)
        }
        colnames(ASIfactor[["scores"]]) <- a
        core_plot <- plot(ASIfactor[["scores"]],xlim = c(-3,3), ylim = c(-3,3))
        abline(h=0, v=0, col="white")
        
        vector_prueba <- str_split(input$vec_predi, ",")
        
        
        
        
        return(core_plot)
        

    }else{
        
        Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
        data1 <- read.csv(Fichero_csv)
        
        daton <- data.frame()
        
        for (i in 1:length(input$Select_factorial1)){
          algo <- data.frame("cambio" = data1[, input$Select_factorial1[i]] )
          names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_factorial1[i] ) 
          if(i == 1){
            daton <- algo
          }else{
            daton <- bind_cols(daton, algo)
          }
          
        }
        
        ASIfactor <- funcion_tabla_policorica()
        
        a <- c()
        for(i in 1:length(colnames(ASIfactor$loadings))){
          b <-paste("Satisfacción_FA",i,sep = "")
          a <- c(a,b)
        }
        colnames(ASIfactor[["scores"]]) <- a
        
        vector_prueba <- str_split(input$vec_predi, ",")
        
        prediccion <- predict(ASIfactor,as.numeric(unlist(vector_prueba)),daton)
      
        core_plot <- plot(ASIfactor[["scores"]],xlim = c(-3,3), ylim = c(-3,3))
        abline(h=0, v=0, col="white")

        points(prediccion, col='red', pch=19) # algo[nrow(daton1),]
        return(core_plot)
        
      }
      
    
    
  })


  
  output$Plot_Matriz_Corr_fiabilidad <- renderPlot({
    
    Datos_Matriz_Corr <- list()
    items <- list()
    
    if(input$Select_fiabilid_matriz == "Policórica"){
      
      Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
      items = round(Datos_Matriz_Corr$rho,2)
      
    } else if(input$Select_fiabilid_matriz == "Pearson"){
      
      Datos_Matriz_Corr <- cor(funcion_datos_fiabilidad(),method = "pearson")
      items = round(Datos_Matriz_Corr,2)
      
    } else if(input$Select_fiabilid_matriz == "Tetracorica"){
      
      Datos_Matriz_Corr <- tetrachoric(funcion_datos_fiabilidad())
      items = round(Datos_Matriz_Corr$rho,2)
      
    } else if(input$Select_fiabilid_matriz == "Biserial"){
      
      algo <- funcion_datos_fiabilidad()
      algo <- apply(algo, 1, function(x)unique(x[!is.na(x)]))
      var_dicotomica <- vector()
      var_likert <- vector()
      
      for( i in 1:length(algo)){
        h <- unique(algo[i])
        if(length(h) == 2 || length(h) == 1){
          cbind(var_dicotomica,algo[i])
        }
        
        if(length(h) > 2){
          cbind(var_likert,algo[i])
        }
      }
      
      vector.is.empty <- function(x) return(length(x) == 0 )
      
      if(vector.is.empty(var_dicotomica) == TRUE){
        return("Seleccione variables tipo dicotomica")
      }
      
      if(vector.is.empty(var_likert) == TRUE){
        return("Seleccione variables tipo likert")
      }
      
      Datos_Matriz_Corr <- biserial(var_dicotomica,var_likert)
      items = round(Datos_Matriz_Corr$rho,2)
      
    } else if(input$Select_fiabilid_matriz == "Poliserial") {
      
      algo <- funcion_datos_fiabilidad()
      algo <- apply(algo, 1, function(x)unique(x[!is.na(x)]))
      var_continua <- vector()
      var_likert <- vector()
      
      for( i in 1:length(algo)){
        
        h <- unique(algo[i])
        if(any(round(algo[i]) != algo[i]) == TRUE ){
          cbind(var_continua,algo[i])
        }
        
        else{
          cbind(var_likert,algo[i])
        }
      }
      
      vector.is.empty <- function(x) return(length(x) == 0 )
      
      if(vector.is.empty(var_continua) == TRUE){
        return("Seleccione variables tipo continua")
      }
      
      if(vector.is.empty(var_likert) == TRUE){
        return("Seleccione variables tipo likert")
      }
      
      Datos_Matriz_Corr <- polyserial(var_likert,var_continua)
      items = round(Datos_Matriz_Corr$rho,2)
    }

    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    
    PLOTCOR <- corrplot(items,method = "shade", sade.col = NA, tl.col = "white",
                        tl.srt = 45, col = col(200), addCoef.col = "black",
                        addcolorlabel = "no", order = "AOE", number.cex = 0.75)
  })
  
  
  

  
  output$Plot_policorica <- renderPlot({
    

    if(input$NumFact_box == 1){
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      daton <- data.frame()
      
      for (i in 1:length(input$Select_factorial1)){
        algo <- data.frame("cambio" = data1[, input$Select_factorial1[i]] )
        names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_factorial1[i] ) 
        if(i == 1){
          daton <- algo
        }else{
          daton <- bind_cols(daton, algo)
        }
        
      }
      #result_n_factors <- fa.parallel(daton,n.obs=200,fa="fa",fm="minres")
      result_n_factors <- n_factors(daton,rotation = "varimax",n = NULL, algorithm = "mle")
      summ <- summary(result_n_factors)
      colnames(summ) <- c("Num_Factores","Num_Metodos")

    
      #,cex.axis = 1 
      # + scale_y_continuous(labels = scales::percent, limits=c(0,0.30), n.breaks = 8)
      plot_policorica <- plot(x = result_n_factors,cex.axis = 1)  + 
        theme(axis.title = element_text(size = 14) )  + 
        scale_y_continuous(labels = scales::percent, limits=c(0,1), n.breaks = 10)
      
      plot_policorica[["labels"]][["title"]] <- " "
      plot_policorica[["labels"]][["x"]] <- "Número de factores"
      plot_policorica[["labels"]][["y"]] <- "Número de métodos"
      
  
      return(plot_policorica)
    }
    
  })
  

  
  Policorica_RMSEA <- eventReactive(input$Boton_fact,{


    ASIfactor <- funcion_tabla_policorica()
    #   RMSEA <- data.frame(I_Complejidad_Media = mean(ASIfactor[["complexity"]]), RMSR = round(ASIfactor[["rms"]],2),
                        #                     RMSR_Correjido = round(ASIfactor[["crms"]],2),
                        #                    RMSEA = ASIfactor[["RMSEA"]][["RMSEA"]], 
                        #                    Inferior = ASIfactor[["RMSEA"]][["lower"]], Superior = ASIfactor[["RMSEA"]][["upper"]],
                        #                    Confianza = ASIfactor[["RMSEA"]][["confidence"]], I_Tucker_Lewis = round(ASIfactor[["TLI"]],2),
                        #                     BIC = ASIfactor[["BIC"]], FIT = round(ASIfactor[["fit"]],2) )
    # algo <- c("I_Complejidad_Media","RMSR","D.free","RMSEA","Inferior","Superior","Confianza","I_Tucker_Lewis","BIC","FIT")
    #  juntar_datos <- rbind(algo,RMSEA)
   # colnames(juntar_datos) <- c("","RMSR","","","","RMSEA","","","","")
    
    c <- "["
    d <- round(ASIfactor[["RMSEA"]][["lower"]],2)
    e <- ","
    f <- round(ASIfactor[["RMSEA"]][["upper"]],2)
    g <- "]"
    
    h <- paste(c,d,e,f,g)
    
    Indice <- c("Ind_complejidad_media", round(mean(ASIfactor[["complexity"]]),2),"","")
    RMSR <- c( "RMS", round(ASIfactor[["rms"]],2),"","" )
    RMSEA <- c("RMSEA",round(ASIfactor[["RMSEA"]][["RMSEA"]],2), "intervalo de confianza (90%)", h)
    tucker <- c("I_Tucker_Lewis",round(ASIfactor[["TLI"]],2),"","" )
    Bic <- c("BIC", round(ASIfactor[["BIC"]],2),"","")
    Fit <- c("FIT", round(ASIfactor[["fit"]],2),"","")
    
    # espacio1 <- c("","","","")
    # espacio2 <- c("","","","")
    # espacio3 <- c("","","","")
    # espacio4 <- c("","","","")
    # espacio5 <- c("","","","")

    #colnames(EPVAL) <- c("p-valor","","","")
    juntar_datos <- rbind.data.frame(Indice,RMSR,RMSEA,tucker,Bic,Fit)
    colnames(juntar_datos) <- c("","","","")
    
    
    return(juntar_datos)
    
  })
  
  Bloque_3 <- eventReactive(input$Boton_fact,{
    
    
    ASIfactor <- funcion_tabla_policorica()

    a <- round(sqrt(ASIfactor[["R2"]]),2)
    b <- round(ASIfactor[["R2"]],2)
    c <- round( 2*ASIfactor[["R2"]]-1,2)
    
    
    d <- rbind(a,b,c)
    
    rownames(d) <- c("Correlación de puntuaciones factoriales",
                     "R2 múltiple de puntuaciones factoriales",
                     "Correlación mínima de puntuaciones factoriales")
    
    a <- c()
    for(i in 1:length(colnames(ASIfactor$loadings))){
      b <-paste("FA",i,sep = "")
      a <- c(a,b)
      
    }
    
    colnames(d) <- a
     
    return(cbind(Factores = rownames(d), round(d,2) ))
    
  })

  
 texto_RMSEA <- eventReactive(input$Boton_fact,{
    
    ASIfactor <- funcion_tabla_policorica()

    total <- mean(ASIfactor[["complexity"]])
      
    #HTML(paste("<br> El test de hipotesis indica que  ", ASIfactor[["factors"]], " factores son suficientes <br> <br/>",
    #     "<ul> <li>",
    #     "Grados de libertad para el modelo nulo: ", ASIfactor[["null.dof"]],
    #     " y objetivo de funcion: ", round(ASIfactor[["null.model"]],2),
    #      " con Chi Cuadrado de: ", round(ASIfactor[["null.chisq"]],2), " </li> <br/>",
    #      "<li> Grados de libertad para el modelo: ", round(ASIfactor[["dof"]],2),
    #      " y objetivo de funcion: ", round(ASIfactor[["objective"]],2),
          #      "</li> </ul>",
    #      "<br>", "<br>", "<ul> <li>",
    #      "El número armónico de observaciones es ",ASIfactor[["n.obs"]] , "con el chi cuadrado empirico ",round(ASIfactor[["chi"]],2),
          #      " con probabilidad < 1 </li> <br>",
    #     "<li> El número armónico de observaciones es ",ASIfactor[["nh"]] , "con probabilidad Chi cuadrado  = ",round(ASIfactor[["STATISTIC"]],2),
    #      " con probabilidad < ", round(ASIfactor[["PVAL"]],2), " </li> <br> </ul>" ))
    

    
    Indice1 <- c(paste("Num_fact_suf :",ASIfactor[["factors"]], sep = "    "),"","","")
    Indice2 <- c("Modelo nulo", paste("g.l. :",ASIfactor[["null.dof"]], sep = "    "),
                 paste("F.obj :",round(ASIfactor[["null.model"]],2), sep = "    "),
                 paste("Chi-cuadrado :",round(ASIfactor[["null.chisq"]],2), sep = "    "))
    Indice3 <- c("Modelo",paste("g.l. :",ASIfactor[["dof"]], sep = "    "),
                 paste("F.obj :",round(ASIfactor[["objective"]],2), sep = "    "),"")
    IndiceVacio <- c("","","","")
    Indice4 <- c(paste("Num_armónico :",ASIfactor[["n.obs"]], sep = "    "),
                 paste("Chi-cuadrado_empírico :",round(ASIfactor[["chi"]],2), sep = "    "), " Probabilidad < 1","")
    Indice5 <- c(paste("Num_total_obs :",ASIfactor[["nh"]], sep = "    "),
                 paste("Chi-cuadrado_verosimilitud :",round(ASIfactor[["STATISTIC"]],2), sep = "    "),"Probabilidad < 0","")
  
    
    juntar_datos <- rbind.data.frame(Indice1,Indice2,Indice3,IndiceVacio,Indice4,Indice5)
    colnames(juntar_datos) <- c("","","","")
    
    
    return(juntar_datos)
    
    })
  
  Policorica_Uniquenesses <- eventReactive(input$Boton_fact,{
    
    
    if(input$Fact_box == 1){
    
      ASIfactor <- funcion_tabla_policorica()
  

      Comunalidad <- round(ASIfactor[["communality"]],2)
      Unicidad <- round(ASIfactor[["uniquenesses"]],2)
      I.Complejidad <- round(ASIfactor[["complexity"]],2)
  
      Cargas <- ASIfactor[["loadings"]]
      nombre <-colnames(Cargas)
      tamano <- length(colnames(Cargas))
      
      for (i in 1:tamano) {
          nombre[i] <- "Saturación_FA"
        f <- toString(i)
        nombre[i] <- paste(nombre[i],f,sep="")
      }
      colnames(Cargas) <- nombre
      confiamos <- cbind(Cargas,Comunalidad, Unicidad, I.Complejidad)
  
      return(cbind(Indicadores = rownames(confiamos), round(confiamos,2) ))
      
    }
    
  })
  
  
  describe_daton <- function() {
    
    Fichero_csv <- paste(ruta,input$Select_Arc_percepcion,sep="")
    data1 <- read.csv(Fichero_csv)
    daton <- data.frame()
    
    for (i in 1:length(input$Select_percepcion)){
      algo <- data.frame("cambio" = data1[, input$Select_percepcion[i]] )
      names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_percepcion[i] ) 
      if(i == 1){
        daton <- algo
      }else{
        daton <- bind_cols(daton, algo)
      }
    }
    return(daton)
  }
  
  
  
  describe_ML  <- function() {
    
    ASIfactorcito <- funcion_tabla_policorica()
    
    Cargas <- ASIfactorcito[["loadings"]]
    nombre <-colnames(Cargas)
    tamano <- length(colnames(Cargas))
    
    for (i in 1:tamano) {
      nombre[i] <- "Saturación_FA"
      f <- toString(i)
      nombre[i] <- paste(nombre[i],f,sep="")
    }
    colnames(Cargas) <- nombre
    confiamos <- cbind(Cargas,Comunalidad)
    
    maxima <- rowMaxs(confiamos)
    tamanio <- length(input$Select_factorial1)
    matrices_ml <- matrix( 1:tamanio,nrow = tamanio, ncol = input$rowfactorial )
    
    for(i in 1:tamanio){
      for(j in 1:input$rowfactorial){
        matrices_ml[i,j] <- "a"
        if(confiamos[i,j] == maxima[i] ){
          matrices_ml[i,j] <- rownames(confiamos)[i]   #maxima[i]
          
        }
      }
    }
    data1 <- matrices_ml[str_detect(matrices_ml[,1], "Valoracion"), ]
    return(matrices_ml)
    
  }
  
  Tabla_describe  <- eventReactive(input$Boton_describe,{ # input$Boton_describe,
    
    #daton <- describe_daton()
    
    if(input$Agrupa_percep_box == FALSE){
     
      matrices_ml <- describe_ML()
      data1 <- matrices_ml[str_detect(matrices_ml[,as.numeric(input$inSlider)], "Item"), ]
  
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      archivo <- read.csv(Fichero_csv)
      
      pruebas_ml <- subset(archivo, select = data1[,as.numeric(input$inSlider)] )
      describe_result <- describe.by(pruebas_ml)
      
      describe_result$vars <- NULL
      describe_result$n <- NULL 
      
      return(cbind(Items = rownames(describe_result), describe_result ))
    
    }
    
    else if(input$Agrupa_percep_box == TRUE){
      
      matrices_ml <- describe_ML()
      data1 <- matrices_ml[str_detect(matrices_ml[,as.numeric(input$inSlider)], "Item"), ]
      
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      archivo <- read.csv(Fichero_csv)
      c <- archivo[,input$Select_compa_percep]
      
      pruebas_ml <- subset(archivo, select = data1[,as.numeric(input$inSlider)] )
      
      

      a <- archivo[,input$Select_compa_percep]
      
      a <- unique(a)
      
      
      
      pruebita <- data.frame()
      
      for(i in 1:length(a)){
        Vacio <- data.frame(a[i],"-","-","-","-","-","-","-","-","-","-","-","-") # ,""
        Vacio1 <- data.frame("-","-","-","-","-","-","-","-","-","-","-","-","-")
        d <- cbind(c,pruebas_ml)
        d <- d[ d$c == a[i],]
        d$c <- NULL

        describe_result <- describe.by(d)
        describe_result$vars <- NULL
        colnames(Vacio) <- colnames(describe_result)
        
        filas <- rownames(describe_result)
        pruebona <- data.frame(as.character(round(describe_result[[1]],2)) )
        
        columnas <- colnames(describe_result)

        for(j in 2:length(colnames(describe_result))){
         
          pruebona <- cbind(pruebona,as.character(round(describe_result[[j]],2)) )
         
        }
        pruebona <- cbind(filas,pruebona)
        
        columnas <- c("",columnas)
        
        colnames(pruebona) <- columnas
        rownames(pruebona) <- rownames(describe_result)

        colnames(Vacio) <- columnas
        colnames(Vacio1) <- columnas

        
        if(i == 1){
          pruebita <- rbind(Vacio,pruebona)
        }else{
          pruebita <- rbind(pruebita,Vacio1,Vacio,pruebona)
        }
        

        # d <- cbind(c,pruebas_ml)
        #################### # d <- d[ d$c == input$Select_compa_percep1,]
        # d <- d[ d$c == a[i],]
       
        # d$c <- NULL
        # pruebas_ml <- d
        # describe_result <- describe.by(pruebas_ml)
        # describe_result$vars <- NULL 
       
      }
       
      return(pruebita) 
      
      #describe_result$n <- NULL 
      
      return(cbind(Items = rownames(describe_result), describe_result ))
      
    }
    
     
  
    
  })
  
  
    

  

  

  
  Tabla_frecuency <- eventReactive(input$Boton_describe,{ 
    
    matrices_ml <- describe_ML()
    data1 <- matrices_ml[str_detect(matrices_ml[,as.numeric(input$inSlider)], "Item"), ]
    
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    archivo <- read.csv(Fichero_csv)
    
    if(input$Agrupa_percep_box == FALSE){
     
      
      General <- subset(archivo, select = data1[,as.numeric(input$inSlider)] )
    
    }else{
      
      
      c <- archivo[,input$Select_compa_percep]
      
      General <- subset(archivo, select = data1[,as.numeric(input$inSlider)] )
      d <- cbind(c,General)
      d <- d[ d$c == "Mujer",]
      
      d$c <- NULL
      General <- d
      
    }
    
    max_row <- 0
    
    for(i in 1:length(General)){
      if(max_row < length(distinct(General[i]))){
        max_row <- distinct(General[i])
        max_row <- length(as.numeric(rownames(max_row)))
      }
    }
    
    aux_dato <- General[1]
    
    length(row_number(aux_dato))
    length(aux_dato)
    

    
    dato <- NULL
    tabla_aux <- 0
    for(i in 1:length(General)){
      aux_dato <- General[i]
      cambio <- list()
      num_lista <- 1
      tamaño_original <- length(row_number(aux_dato))
      for(j in 1:max_row){
        if(as.numeric(rownames(table(aux_dato)))[j] == j ){
          
        }else{

          aux_dato[length(rownames(aux_dato))+1,] <- j
          cambio[num_lista] <- j
          num_lista <- num_lista + 1
          
        }
      }
      
      
      if(tamaño_original != length(row_number(aux_dato))){
        num_lista <- num_lista - 1
        if(length(dato) == 0){

          dato <- aux_dato
          dato <- table(aux_dato)
          for(i in 1:num_lista){
            num <- as.numeric(cambio[i])
            dato[num] <- 0
          }
          
        }else{

          aux_dato <- table(aux_dato)
          for(i in 1:num_lista){
            num <- as.numeric(cambio[i])
            aux_dato[num] <- 0
          }

          dato <- cbind(dato,aux_dato)
        }
        
      }else{
        
        if(length(dato) == 0){
          
          dato <- aux_dato
          dato <- table(aux_dato)
        }else{
          aux_dato <- table(aux_dato)
          dato <- cbind(dato,aux_dato)
        }
        
      }
      
    }
    
    
    colnames(dato) <- colnames(General)
    
    return(cbind(Valoracion = rownames(dato), dato))
  })
  
  Tabla_frecuency2 <- eventReactive(input$Boton_describe,{ 

    if(input$Agrupa_percep_box == TRUE){
      matrices_ml <- describe_ML()
      data1 <- matrices_ml[str_detect(matrices_ml[,as.numeric(input$inSlider)], "Item"), ]
      
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      archivo <- read.csv(Fichero_csv)
      

      Generali <- subset(archivo, select = data1[,as.numeric(input$inSlider)] )
      
      
      c <- archivo[,input$Select_compa_percep]
      
      
      a <- archivo[,input$Select_compa_percep]
      
      a <- unique(a)
      
      data_final <- data.frame()
      for(k in 1:length(a)){

      
        d <- cbind(c,Generali)
        d <- d[ d$c == a[k],]
        
        d$c <- NULL
        General <- d
          
        
        
        max_row <- 0
        
        for(i in 1:length(General)){
          if(max_row < length(distinct(General[i]))){
            max_row <- distinct(General[i])
            max_row <- length(as.numeric(rownames(max_row)))
          }
        }
        
        aux_dato <- General[1]
        
        length(row_number(aux_dato))
        length(aux_dato)
        

        
        dato <- NULL
        tabla_aux <- 0
        for(i in 1:length(General)){
          aux_dato <- General[i]
          cambio <- list()
          num_lista <- 1
          tamaño_original <- length(row_number(aux_dato))
          for(j in 1:max_row){
            if(as.numeric(rownames(table(aux_dato)))[j] == j ){
              
            }else{
              
              aux_dato[length(rownames(aux_dato))+1,] <- j
              cambio[num_lista] <- j
              num_lista <- num_lista + 1
              
            }
          }
          
          
          if(tamaño_original != length(row_number(aux_dato))){
            num_lista <- num_lista - 1
            if(length(dato) == 0){
              
              dato <- aux_dato
              dato <- table(aux_dato)
              for(i in 1:num_lista){
                num <- as.numeric(cambio[i])
                dato[num] <- 0
              }
              
            }else{
              
              aux_dato <- table(aux_dato)
              for(i in 1:num_lista){
                num <- as.numeric(cambio[i])
                aux_dato[num] <- 0
              }
              
              dato <- cbind(dato,aux_dato)
            }
            
          }else{
            
            if(length(dato) == 0){
              
              dato <- aux_dato
              dato <- table(aux_dato)
            }else{
              aux_dato <- table(aux_dato)
              dato <- cbind(dato,aux_dato)
            }
            
          }
        
        }
        

        Vacio <- c(a[k])
        Vacio1 <- c("-")
        for(l in 1:length(colnames(General)) ){
          Vacio <- c(Vacio,"-")
          Vacio1 <- c(Vacio1,"-")
        }

        
        filas <- rownames(dato)
        total <- data.frame(as.character(round(dato[,1],2)) )
        for(h in 2:length(colnames(dato))){
          total <- cbind(total,as.character(round(dato[,h],2)) )
        }  
      
        total <- cbind(filas,total)
        
        columna <- c("",colnames(General))
        colnames(total) <- columna
        
        
        if(k == 1){
          data_final <- rbind(Vacio,total)
        }else{

          data_final <- rbind(data_final,Vacio1,Vacio,total)

        }

       # colnames(dato) <- colnames(General)
      }
      
      return(data_final)
      
      return(cbind(Valoracion = rownames(dato), dato))
    }
  })
  
  Tabla_independencia <- eventReactive(input$Boton_comparativa,{
    
    data_sin_intervalo <- funcion_independencia()
    data_sin_intervalo[1] <- NULL

   
    chicuadrado1 <- chisq.test(data_sin_intervalo )
    fisher1 <- fisher.test(data_sin_intervalo )

    if(min(data_sin_intervalo) <= 5){
      aviso <- "Tamaño muestral de los grupos < 5"
      data <- data.frame("Test" = c("Chi-cuadrado","Exacto de Fisher"), "Estadistico" = c(round(chicuadrado1$statistic,2)," "),
                         "g.l." = c(chicuadrado1$parameter," "), "p-valor" = c(chicuadrado1$p.value,fisher1$p.value),
                         "Aviso" = c(aviso, " "))
      colnames(data) <- c("Test", "Estadistico", "g.l.", "p-valor", "Aviso")
    }else{
      data <- data.frame("Test" = c("Chi-cuadrado","Exacto de Fisher"), "Estadistico" = c(round(chicuadrado1$statistic,2)," "),
                         "g.l." = c(chicuadrado1$parameter," "), "p-valor" = c(chicuadrado1$p.value,fisher1$p.value) )
      colnames(data) <- c("Test", "Estadistico", "g.l.", "p-valor")
    }
    
    return(data)
    
  })
  
  Tabla_independencia_freq <- eventReactive(input$Boton_comparativa,{
    return(funcion_independencia())
    
  })
  
  
  Tabla_varianza <- eventReactive(input$Boton_comparativa,{
    
    ASIfactor <- funcion_tabla_policorica()
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    


      b <- unique(data1[,input$Select_compa1])
      b[1]
      
      if(b[1] != 2 ){
        data1[,input$Select_compa1][data1[,input$Select_compa1] == b[1]] <- 1
        data1[,input$Select_compa1][data1[,input$Select_compa1] == b[2]] <- 2
      }
      

  
  
  
      #varianza_test <- var.test(ASIfactorcito[["scores"]][,1],ASI[,1])
       varianza <- var.test(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ as.numeric(data1[,input$Select_compa1], alternative = "two.sided" ) )

      
       Vari <- data.frame(f = varianza[["statistic"]],"g.l.num" = as.character(varianza[["parameter"]][["num df"]]),
                          "g.l.denom" = as.character(varianza[["parameter"]][["denom df"]]),
                          p_value = as.character(formatC(varianza[["p.value"]], format = "e", digits = 2)),
                          coeficiente_intervalo_1 = varianza[["conf.int"]][1],
                          coeficiente_intervalo_2 = varianza[["conf.int"]][2] )
       
     
       colnames(Vari) <- c("F","g.l. num","g.l. denom","p-valor","límite inferior (95%)","límite superior (95%)")
      
       return(Vari)
     
       
    
    
   # T_TEST <- data.frame(t = ttest[["statistic"]],df = ttest[["parameter"]][["df"]],
    #                     p.value = ttest[["p.value"]],media_de_x = ttest[["estimate"]][["mean of x"]] )
    
  })
  
  Tabla_ttest <- eventReactive(input$Boton_comparativa,{
    ASIfactor <- funcion_tabla_policorica()
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    


  
    b <- unique(data1[,input$Select_compa1])
    b[1]
    
    if(b[1] != 2 ){
      data1[,input$Select_compa1][data1[,input$Select_compa1] == b[1]] <- 1
      data1[,input$Select_compa1][data1[,input$Select_compa1] == b[2]] <- 2
    }
    

    #varianza_test <- var.test(ASIfactorcito[["scores"]][,1],ASI[,1])
    varianza <- var.test(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ as.numeric(data1[,input$Select_compa1]) )

    variable_equal <- FALSE
    if(varianza[["p.value"]] > 0.05){
      variable_equal <- TRUE
    }else{
      variable_equal <- FALSE
    }
    
   
# as.numeric(data1[,input$Select_compa1])
    ttest <- t.test(ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ as.numeric(data1[,input$Select_compa1]) ,var.equal = variable_equal)
    
    
    T_TEST <- data.frame(t = ttest[["statistic"]],df = as.character(round(ttest[["parameter"]][["df"]],0)),
                        p.value = as.character(formatC(ttest[["p.value"]], format = "e", digits = 2)) )
    
    colnames(T_TEST) <- c("t","g.l.","p-valor")
    
    return(T_TEST)

  })
  
  
  
  
  
  
  
  
  Tabla_Durbin <- eventReactive(input$Boton_comparativa,{
    
    fm <- funcion_aov_()

    durbin <- data.frame("Durbin" = durbinWatsonTest(fm$residuals) )
    
    if(round(durbin,0) == 2){
      a <- ("Independientes") 
    }else{
      a <- ("No Independientes") 
    }
    durbin <- data.frame("Durbin" = durbinWatsonTest(fm$residuals), "rest" = a )
    
    colnames(durbin) <- c("Durbin-Watson","Result (Próx 2)")
    
    return(durbin)
    
  })
  
  Tabla_Desviaciones <- eventReactive(input$Boton_comparativa,{
    
    
      ASIfactor <- funcion_tabla_policorica()
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      

      fm <- funcion_aov_()
        
      tapplyy <- tapply(fm$residuals,data1[,input$Select_compa1],sd)
      valor <- max(tapplyy, na.rm = TRUE) / min(tapplyy, na.rm = TRUE) # Si es mayor que 2 varianzas distintas si no iguales

      if(valor > 2){
        h <- "Varianzas distintas"
      }else{
        h <- "Varianzas iguales"
      }

      data <- data.frame( "Min" = min(tapplyy, na.rm = TRUE),"Max" = max(tapplyy, na.rm = TRUE), "Ratio" = valor, "(<o>2)" = h)
      colnames(data) <- c("Min","Max","Ratio","Ratio (< ó > 2)")

      return(data)
        
    
  })
  
  Tabla_Homocedasticidad <- eventReactive(input$Boton_comparativa,{
    
    ASIfactor <- funcion_tabla_policorica() 
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    daton = data.frame(weight = ASIfactor[["scores"]][,as.numeric(input$inSlider1)] , group = as.character(data1[,input$Select_compa1]) )
    
    fm <- funcion_aov_()
    
    if( length(rownames(data1)) < 50){
      test_norma <-  shapiro.test(fm$residuals)
    }else{
      test_norma <- lillie.test(fm$residuals)
    }
    
  
    if(test_norma$statistic < 0.05){
      bartlesito <- bartlett.test(fm$residuals ~ data1[,input$Select_compa1])
      a <- "Bartlett"
      data <- data.frame("Test" = a,"Estadistico" = bartlesito[["statistic"]], "df" = as.character(bartlesito[["parameter"]][["df"]]),
                         "P-valor" = as.character(formatC(bartlesito[["p.value"]], format = "e", digits = 2)) )
      colnames(data) <- c("Test","Estadístico", "g.l.", "p-valor")  
    }else{
      bartlesito <- fligner.test(fm$residuals ~ data1[,input$Select_compa1])
      
      if(is.numeric(data1[,input$Select_compa1]) == TRUE ){
        for(i in 1:length(unique(data1[,input$Select_compa1]))){
          b <- unique(data1[,input$Select_compa1])
          text <- paste( "a_",i)
          data1[,input$Select_compa1][data1[,input$Select_compa1] == b[i]] <- text
        }
      }
      
      levene <- leveneTest(fm$residuals ~ data1[,input$Select_compa1])
      a <- "Fligner-Killeen"
      b <- "Levene"
      data <- data.frame("Test" = c(a, b) ,"Estadistico" = c(bartlesito[["statistic"]], levene$`F value`[1]) , "df" = c(as.character(bartlesito[["parameter"]][["df"]]), as.character(levene$Df[1]) ) ,
                         "df2" =c(" ", as.character(levene$Df[2]) ) ,
                         "P-valor" = c( as.character(formatC(bartlesito[["p.value"]], format = "e", digits = 2)), as.character(formatC(levene$`Pr(>F)`[1], format = "e", digits = 2)) ) )
      
      colnames(data) <- c("Test","Estadístico", "g.l.num","g.l.denom", "p-valor")
    }
    
    return(data)
  })
  
 
  
  Tabla_normalidad <- eventReactive(input$Boton_comparativa,{

    ASIfactor <- funcion_tabla_policorica()
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    daton = data.frame(weight = ASIfactor[["scores"]][,as.numeric(input$inSlider1)] , group = as.character(data1[,input$Select_compa1]) )
    
    
    if( length(rownames(data1)) < 50){
      test_norma <- by(data = daton,INDICES = daton$group,FUN = function(x){ shapiro.test( x$weight ) } )
      b <- "Shapiro-Wilk"
    }else{
      test_norma <- by(data = daton,INDICES = daton$group,FUN = function(x){ lillie.test( x$weight )})
      b <- "Kolmogorov-Smirnov"
    }
    
    
    variable_1 <- unique(daton$group)
    UFF <- data.frame()
    
    for(i in 1:length(unique(daton$group))){
      if(i == 1){
        a <- data.frame("Test" = b,"Variable" = variable_1[i], "w" = test_norma[[variable_1[i]]][["statistic"]],"pvalor" = as.character(formatC(test_norma[[variable_1[i]]][["p.value"]], format = "e", digits = 2)) )
      }else{
        a <- data.frame("Test" = " ","Variable" = variable_1[i], "w" = test_norma[[variable_1[i]]][["statistic"]],"pvalor" = as.character(formatC(test_norma[[variable_1[i]]][["p.value"]], format = "e", digits = 2)) )
      }
      UFF <- rbind(UFF,a)
    }
    
    colnames(UFF) <- c("Test",input$Select_compa1,"Estadístico","p-valor")
    return(UFF)
 
  })
  
  Tabla_normalidad_anova  <- eventReactive(input$Boton_comparativa,{
    
    ASIfactor <- funcion_tabla_policorica()
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    daton = data.frame(weight = ASIfactor[["scores"]][,as.numeric(input$inSlider1)] , group = as.character(data1[,input$Select_compa1]) )
    
    fm <- funcion_aov_()
    
    if( length(rownames(data1)) < 50){
      test_norma <-  shapiro.test(fm$residuals)
      b <- "Shapiro-Wilk"
    }else{
      test_norma <- lillie.test(fm$residuals)
      b <- "Kolmogorov-Smirnov"
    }
    
    
    a <- data.frame("Test" = b,"w" = test_norma$statistic,"pvalor" = as.character(formatC(test_norma$p.value, format = "e", digits = 2)) )
    

    colnames(a) <- c("Test","Estadístico","p-valor")
    return(a)
    
  })
  
  Tabla_Anova <- eventReactive(input$Boton_comparativa,{
    
    ASIfactor <- funcion_tabla_policorica()
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    
    fm <- funcion_aov_()
    kurstal <- funcion_krustal_()

    if( length(rownames(data1) ) < 50){
      test_norma <-  shapiro.test(fm$residuals)
    }else{
      test_norma <- lillie.test(fm$residuals)
      
      
    }

    tapplyy <- tapply(fm$residuals,data1[,input$Select_compa1],sd)

    valor <- max(tapplyy) / min(tapplyy) # Si es mayor que 2 varianzas distintas si no iguales
    

    
    
   # checkbox_anova_hetero
   # checkbox_anova_homoce
   # checkbox_anova_noparam
    
    # test_norma$statistic >= 0.05 && h == 0)
    if(input$checkbox_anova_homoce == 1){
    
      
      
      summary_fm <- summary(fm)
      
      summary_fm[[1]][["Pr(>F)"]] <- formatC(summary_fm[[1]][["Pr(>F)"]], format = "e", digits = 2)
      summary_fm[[1]][["Pr(>F)"]][length(summary_fm[[1]][["Pr(>F)"]])] <- " "
      
      data <- data.frame( "Sum_Sq" = round(summary_fm[[1]][["Sum Sq"]],2), "Df" = as.character(summary_fm[[1]][["Df"]]),  
                          "Mean_Sq" = round(summary_fm[[1]][["Mean Sq"]],2), "F_value" = round(summary_fm[[1]][["F value"]],2),
                          "Pr(>F)" = summary_fm[[1]][["Pr(>F)"]]  ) 
      
      colnames(data) <- c("Suma de cuadrados", "g.l.", "Media cuadrática", "F","p-valor")
      
      data[is.na(data)] <- " "
      data[data == "NA"] <- " "
      
      

      
      rownames(data) <- c( as.character(input$Select_compa1),"Residuos")
      
      data <- cbind(Indicadores = rownames(data), data )
      colnames(data)[1] <- ""
      
      
      return( data )
      # test_norma$statistic < 0.05 && h == 0
    }else if(input$checkbox_anova_noparam == 1){
      
      
      
      krusti <- kruskal.test( ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1] )
      
      data <- data.frame( "Test" = "Kruskal-Wallis", "estadistico" = krusti[["statistic"]], "pvalue" = as.character(formatC(krusti[["p.value"]], format = "e", digits = 2)) )
      
      colnames(data) <- c("Test", "Estadístico","p-valor")
      return(data)
    }else{
      
      
      oneway <- oneway.test( ASIfactor[["scores"]][,as.numeric(input$inSlider1)] ~ data1[,input$Select_compa1] )
      
      data <- data.frame( "Test" = "One-way", "estadistico" = oneway[["statistic"]], "pvalue" = as.character(formatC(oneway[["p.value"]], format = "e", digits = 2)) )
      
      colnames(data) <- c("Test", "Estadístico","p-valor")
      return(data)
      
    }
    
 
  })
  
  Tabla_Tamano_Anova <- eventReactive(input$Boton_comparativa,{
    # checkbox_anova_hetero
    # checkbox_anova_homoce
    # checkbox_anova_noparam
    
    if(input$checkbox_anova_homoce == 1){  
      fm <- funcion_aov_()
      summary_fm <- summary(fm)
      tamano <- summary_fm[[1]][["Sum Sq"]][1] / summary_fm[[1]][["Sum Sq"]][2]
      
      data <- etaSquared(fm)
      rownames(data) <- input$Select_compa1
      
      if(data[1] <= 0.01 ){
        valor <- "Pequeño"
      }else if(data[1] > 0.01 && data[1] <= 0.06){
        valor <- "Mediano"
      }else if(data[1] > 0.06 && data[1] <= 0.14){
        valor <- "Grande"
      }else if(data[1] > 0.14 ){
        valor <- "Muy grande"
      }
      
      
      data <- cbind(Indicadores = rownames(data), round(data,2) )
      
      data <-cbind(data, texto = valor)
      
      
      
      colnames(data) <- c(" ","eta cuadrado","eta cuadrado parcial","Tamaño efecto")
      
      

      return( data )
      
    }
    
  })
  
  
  
  Tabla_Bonferroni <- eventReactive(input$Boton_comparativa,{
    
    ASIfactor <- funcion_tabla_policorica()
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    
    #input$Select_metodo_PostHoc
    
    if(input$Select_metodo_PostHoc =="Pairwill-wilcox"){
    
      data2 <- data.frame( valor = c(data1[1,input$Select_compa1]) )
      
      for(i in 2:length(data1[,input$Select_compa1])){
        data2 <- cbind(data2, data1[i,input$Select_compa1])
        
      }
      
      agrupa <- distinct(data1[input$Select_compa1])
      colnames(agrupa)[1] <- "Agrupamiento"
  
   
      data_Bonfe <- data.frame(group = as.character(agrupa),
                            values = ASIfactor[["scores"]][,as.numeric(input$inSlider1)] )
      
  
      Bonferroni_data <- pairwise.wilcox.test (data_Bonfe$values, data2)
      
      Bonferroni_data[["p.value"]]
      
      data <- data.frame( valor = Bonferroni_data[["p.value"]] )
      data <- cbind(Indicadores = rownames(data), round(data,4) )
      

      colnames(data) <- c(" ",colnames(Bonferroni_data[["p.value"]]))
      return(data)
    
    }else if( input$Select_metodo_PostHoc == "Tukey"){
      
      fm <- funcion_aov_()
      aa <- TukeyHSD(fm)
      #a <- cbind(Indicadores = rownames(aa[["data1[, input$Select_compa1[1]]"]]), round(aa[["data1[, input$Select_compa1[1]]"]],4) )
      #colnames(a) <- c(" ","dif","inferior","superior","p-valor")

      
      che <- rownames(aa$`data1[, input$Select_compa1[1]]`)
      eee <- unlist(aa["data1[, input$Select_compa1[1]]"])
      
      for(i in 1:4){
        datin_aux <- data.frame()
        datin <- data.frame()
        for(j in 1:(length(eee)/4) ){
          
          if(i != 1){
            h <- (length(eee)/4) * (i-1)
          }else{
            h <- 0
          }
          
          if(j == 1){
            datin <- data.frame("1"= round(eee[j+h],4) )
            datin_aux <- datin
          }else{
            if(j != 4){
              datin <- data.frame("1"= round(eee[j+h],4) )
              datin_aux <- rbind(datin_aux,datin)
            }else{
              datin <- data.frame("1"= round(eee[j+h],4) )
              datin_aux <- rbind(datin_aux,datin)
            }
            
          }
        }
        if(i == 1){
          datin_total <- data.frame()
          datin_total <- datin_aux
        }else{
          datin_total <- cbind(datin_total,datin_aux)
        }
      }
      
      
      colnames(datin_total) <- c("dif","inferior","superior","pvalor")
      rownames(datin_total) <- che
  
      
      kek <- data.frame("1" = as.character(round(datin_total$dif,2)), "2" = as.character(round(datin_total$inferior,2)),
                        "3" = as.character(round(datin_total$superior,2)), "4" = as.character(round(datin_total$pvalor,4))  )
      
      colnames(kek) <- c("dif","inferior","superior","pvalor")
      rownames(kek) <- che
     
      
      kek <- cbind(Indicadores = rownames(aa[["data1[, input$Select_compa1[1]]"]]), kek )
      return(kek) 
      
    }else{
    
      fm <- funcion_aov_()
      aa <- PostHocTest(fm, which = NULL,
                  method = input$Select_metodo_PostHoc,
                  conf.level = 0.95, ordered = FALSE)
      

      che <- rownames(aa$`data1[, input$Select_compa1[1]]`)
      eee <- unlist(aa["data1[, input$Select_compa1[1]]"])
      
      for(i in 1:4){
        datin_aux <- data.frame()
        datin <- data.frame()
        for(j in 1:(length(eee)/4) ){
          
          if(i != 1){
            h <- (length(eee)/4) * (i-1)
          }else{
            h <- 0
          }
          
          if(j == 1){
            datin <- data.frame("1"= round(eee[j+h],4) )
            datin_aux <- datin
          }else{
            if(j != 4){
              datin <- data.frame("1"= round(eee[j+h],4) )
              datin_aux <- rbind(datin_aux,datin)
            }else{
              datin <- data.frame("1"= round(eee[j+h],4) )
              datin_aux <- rbind(datin_aux,datin)
            }
            
          }
        }
        if(i == 1){
          datin_total <- data.frame()
          datin_total <- datin_aux
        }else{
          datin_total <- cbind(datin_total,datin_aux)
        }
      }
      
      
      colnames(datin_total) <- c("dif","inferior","superior","pvalor")
      rownames(datin_total) <- che
      
      kek <- data.frame("1" = as.character(round(datin_total$dif,2)), "2" = as.character(round(datin_total$inferior,2)),
                        "3" = as.character(round(datin_total$superior,2)), "4" = as.character(round(datin_total$pvalor,4))  )
      
      colnames(kek) <- c("dif","inferior","superior","pvalor")
      rownames(kek) <- che

      
      kek <- cbind(Indicadores = rownames(aa[["data1[, input$Select_compa1[1]]"]]), kek )
      return(kek)  
      

      #  a <- cbind(Indicadores = rownames(aa[["data1[, input$Select_compa1[1]]"]]), round(aa[["data1[, input$Select_compa1[1]]"]],4) )
      #  colnames(a) <- c(" ","dif","inferior","superior","p-valor")
      #  return(a) # aa[["data1[, input$Select_compa1[1]]"]]
      
    }
  })
  
  output$slider <- renderUI({
    matrices_ml <- describe_ML()
    
    vector = c()
    datos <- data.frame()
    for (i in 1:ncol(matrices_ml)){
      vector <- c(vector, i)
    }
    
    selectInput("inSlider", "Seleccione el factor a analizar:", vector)
    
    
    #sliderInput("inSlider", "Seleccione el factor a analizar", min=1, max=ncol(matrices_ml), value=2000, step = 1)
  })
    
  output$slider1 <- renderUI({
    matrices_ml <- describe_ML()
    
    vector = c()
    datos <- data.frame()
    for (i in 1:ncol(matrices_ml)){
      vector <- c(vector, i)
    }
    
    selectInput("inSlider1", "Seleccione el factor a analizar:", vector)
    
    #sliderInput("inSlider1", "Seleccione el factor a analizar", min=1, max=ncol(matrices_ml), value=2000, step = 1)
  })
  
  
  Policorica_Predicciones <- eventReactive(input$Boton_predi,{
    
    
    if(input$Predi_box == 1){
      
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      daton <- data.frame()
      
      for (i in 1:length(input$Select_factorial1)){
        algo <- data.frame("cambio" = data1[, input$Select_factorial1[i]] )
        names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_factorial1[i] ) 
        if(i == 1){
          daton <- algo
        }else{
          daton <- bind_cols(daton, algo)
        }
        
      }
    
      
      ASIfactor <- funcion_tabla_policorica()
      
      vector_prueba <- str_split(input$vec_predi, ",")
      a <- c()
      for(i in 1:length(colnames(ASIfactor$loadings))){
        b <-paste("FA",i,sep = "")
        a <- c(a,b)
        
      }

      c <- predict(ASIfactor,as.numeric(unlist(vector_prueba)),daton)
      
      colnames(c) <- a
      
      return(c)
    }
    
  })
  
  Text_Predicciones <- eventReactive(input$Boton_predi,{
    
    texto <- data.frame(Result = c(""))
    colnames(texto) <- "En rojo se muestra el resultado de la predicción"
    return(texto)
  })
  
  Policorica_Values <- eventReactive(input$Boton_fact,{
    

    if(input$Puntuaciones_box == 1){
      ASIfactor <- funcion_tabla_policorica()
      puntuaciones <- ASIfactor[["scores"]]
      
      nombre <-colnames(puntuaciones)
      tamano <- length(colnames(puntuaciones))
      
      for (i in 1:tamano) {
        nombre[i] <- "FA"
        f <- toString(i)
        nombre[i] <- paste(nombre[i],f,sep="")
      }
      colnames(puntuaciones) <- nombre
      
      return(cbind(Indicadores = rownames(puntuaciones), round(puntuaciones,2) ))
      
    }
    
  })
  
 

  Tabla_num_factores <- eventReactive(input$NumFact_box,{
    
    if(input$NumFact_box == 1){
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      daton <- data.frame()
      
      for (i in 1:length(input$Select_factorial1)){
        algo <- data.frame("cambio" = data1[, input$Select_factorial1[i]] )
        names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_factorial1[i] ) 
        if(i == 1){
          daton <- algo
        }else{
          daton <- bind_cols(daton, algo)
        }
        
      }
      
  
      #result_n_factors <- fa.parallel(daton,n.obs=200,fa="fa",fm="minres")
      result_n_factors <- n_factors(daton,rotation = "varimax",n = NULL, algorithm = "mle")
      
      data_tratar <- as.data.frame(result_n_factors)
      names(data_tratar) = c("Num_Factores", "Metodo", "Familia")
      


     # data_tratar$Num_Factores <- trunc(data_tratar$Num_Factores)
      return(data_tratar)
    }
    
  })
  
  Tabla_num_factores_summary <- eventReactive(input$NumFact_box,{
    
    if(input$NumFact_box == 1){
      Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
      data1 <- read.csv(Fichero_csv)
      
      daton <- data.frame()
      
      for (i in 1:length(input$Select_factorial1) ){
        algo <- data.frame("cambio" = data1[, input$Select_factorial1[i]] )
        names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_factorial1[i] ) 
        if(i == 1){
          daton <- algo
        }else{
          daton <- bind_cols(daton, algo)
        }
        
      }
      
      #result_n_factors <- fa.parallel(daton,n.obs=200,fa="fa",fm="minres")
      result_n_factors <- n_factors(daton,rotation = "varimax",n = NULL, algorithm = "mle")
      
      summ <- summary(result_n_factors)
      colnames(summ) <- c("Num_Factores","Num_Metodos")

      return(summ)
    }
    
  })
  
  Policorica_Weights <- eventReactive(input$Boton_fact,{
    
    
    if(input$Fact_box == 1){
      ASIfactor <- funcion_tabla_policorica()
      
      Pesos <- ASIfactor[["weights"]]
      nombre <-colnames(Pesos)
      tamano <- length(colnames(Pesos))
      
      for (i in 1:tamano) {
        nombre[i] <- "FA"
        f <- toString(i)
        nombre[i] <- paste(nombre[i],f,sep="")
      }
      colnames(Pesos) <- nombre
    
      
      return(cbind(Indicadores = rownames(Pesos), round(Pesos,2) ))
      
    }
    
  })
  
  
  
  Policorica_Vaccounted <- eventReactive(input$Boton_fact,{
   
    if(input$Fact_box == 1){
    
      ASIfactor <- funcion_tabla_policorica()
      
      Vacc <- ASIfactor[["Vaccounted"]]
      nombre <-colnames(Vacc)
      tamano <- length(colnames(Vacc))
      
      for (i in 1:tamano) {
        nombre[i] <- "FA"
        f <- toString(i)
        nombre[i] <- paste(nombre[i],f,sep="")
      }
      colnames(Vacc) <- nombre
      
      nombre_filas <- c("SS Saturaciones","Proporción Varianza","Varianza acumulada","Proporción explicada","Proporción acumulada")
      
      return(cbind(Factores = nombre_filas, round(Vacc,2) ))

    }
    
  })
  

  Fiabilidad_daton <- function() {
    
    Fichero_csv <- paste(ruta,input$Select_Arc_graph_cat,sep="")
    data1 <- read.csv(Fichero_csv)
    daton <- data.frame()
    
    for (i in 1:length(input$Select_graph_cat1)){
      algo <- data.frame("cambio" = data1[, input$Select_graph_cat1[i]] )
      names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_graph_cat1[i] ) 
      if(i == 1){
        daton <- algo
      }else{
        daton <- bind_cols(daton, algo)
      }
    }
    return(daton)
  }

  
  KMO_table <- eventReactive(input$Boton_Adecuacion,{
    
      #kmo_value <- KMO(Fiabilidad_daton())
      
      
      if(input$Select_fiabilid_matriz == "Pearson"){
        
        Datos_Matriz_Corr <- cor(funcion_datos_fiabilidad(),method = "pearson")
        kmo_value <- KMO(Datos_Matriz_Corr)
        
        
      }else if(input$Select_fiabilid_matriz == "Policórica"){
        
        Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
        kmo_value <- KMO(Datos_Matriz_Corr$rho)
        
        
      }else if(input$Select_fiabilid_matriz == "Tetracorica"){
        
        Datos_Matriz_Corr <- tetrachoric(funcion_datos_fiabilidad())
        kmo_value <- KMO(Datos_Matriz_Corr$rho)
        
        
      }else if(input$Select_fiabilid_matriz == "Biserial"){
        
        Datos_Matriz_Corr <- biserial(funcion_datos_fiabilidad())
        kmo_value <- KMO(Datos_Matriz_Corr$rho)
        
        
      }else if(input$Select_fiabilid_matriz == "Poliserial"){
        
        Datos_Matriz_Corr <- polyserial(funcion_datos_fiabilidad())
        kmo_value <-KMO(Datos_Matriz_Corr$rho)
      }
      data <- data.frame( "MSA" = kmo_value[["MSA"]])
      return(round(data,2))

  })
  
  KMO_table1 <- eventReactive(input$Boton_Adecuacion,{
    
    #kmo_value <- KMO(Fiabilidad_daton())
    
    if(input$Select_fiabilid_matriz == "Pearson"){
      
      Datos_Matriz_Corr <- cor(funcion_datos_fiabilidad(),method = "pearson")
      kmo_value <- KMO(Datos_Matriz_Corr)
      
      
    }else if(input$Select_fiabilid_matriz == "Policórica"){
      
      Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
      kmo_value <- KMO(Datos_Matriz_Corr$rho)
      
      
    }else if(input$Select_fiabilid_matriz == "Tetracorica"){
      
      Datos_Matriz_Corr <- tetrachoric(funcion_datos_fiabilidad())
      kmo_value <- KMO(Datos_Matriz_Corr$rho)
      
      
    }else if(input$Select_fiabilid_matriz == "Biserial"){
      
      Datos_Matriz_Corr <- biserial(funcion_datos_fiabilidad())
      kmo_value <- KMO(Datos_Matriz_Corr$rho)
      
      
    }else if(input$Select_fiabilid_matriz == "Poliserial"){
      
      Datos_Matriz_Corr <- polyserial(funcion_datos_fiabilidad())
      kmo_value <-KMO(Datos_Matriz_Corr$rho)
    }
    
    Valoracion <- list()
    

    
    for(i in 1:length(kmo_value[["MSAi"]])){
      if(round(as.numeric(kmo_value[["MSAi"]][i]),2) < 0.5){
        Valoracion[i] <- "Inaceptable"
        
      }else if(round(as.numeric(kmo_value[["MSAi"]][i]),2) >= 0.5 & round(as.numeric(kmo_value[["MSAi"]][i]),2) < 0.59 ){
        Valoracion[i] <- "Insuficiente"
        
      }else if(round(as.numeric(kmo_value[["MSAi"]][i]),2) >= 0.6 & round(as.numeric(kmo_value[["MSAi"]][i]),2) < 0.69 ){
        Valoracion[i] <- "Mediocre"
        
      }else if(round(as.numeric(kmo_value[["MSAi"]][i]),2) >= 0.7 & round(as.numeric(kmo_value[["MSAi"]][i]),2) < 0.79 ){
        Valoracion[i] <- "Aceptable"
        
      }else if(round(as.numeric(kmo_value[["MSAi"]][i]),2) >= 0.8 & round(as.numeric(kmo_value[["MSAi"]][i]),2) < 0.89 ){
        Valoracion[i] <- "Meritorio"
        
      }else {
        Valoracion[i] <- "Maravilloso"
      }

    }
    MSA <- kmo_value[["MSAi"]]
    Datos <- cbind(Items = names(kmo_value[["MSAi"]]),MSA,Valoracion)
    return(Datos)


    
  })
  
  Indicadores1_table <- eventReactive(input$Boton_fiabilidad,{
    data <- Fiabilidad_daton()
    Rel_val <- reliability(data)
    O_indicador <- data.frame(Rel_val[["result.df"]][1], Rel_val[["result.df"]][3], Rel_val[["result.df"]][4], Rel_val[["result.df"]][5], Rel_val[["result.df"]][6] )
    colnames(O_indicador) <- c("Omega_h","Omega_tot","Uni","R_fit","Fa.fit")
    return(O_indicador)
  })
  
  Indicadores2_table <- eventReactive(input$Boton_fiabilidad,{
    data <- Fiabilidad_daton()
    Rel_val <- reliability(data)
    O_indicador <- data.frame( Rel_val[["result.df"]][7], Rel_val[["result.df"]][8], Rel_val[["result.df"]][9], Rel_val[["result.df"]][10] )
    colnames(O_indicador) <- c("Max.split","Min.split","Mean.r","Med.r")
    return(O_indicador)
  })
  

  
  Bartlett_table <- eventReactive(input$Boton_Adecuacion,{
    
    
    
    if(input$Select_fiabilid_matriz == "Pearson"){
      
      Datos_Matriz_Corr <- cor(funcion_datos_fiabilidad(),method = "pearson")
      a <- Datos_Matriz_Corr
      
    }else if(input$Select_fiabilid_matriz == "Policórica"){
      
      Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
      a <- Datos_Matriz_Corr$rho
      
    }else if(input$Select_fiabilid_matriz == "Tetracorica"){
      
      Datos_Matriz_Corr <- tetrachoric(funcion_datos_fiabilidad())
      a <- Datos_Matriz_Corr$rho
      
    }else if(input$Select_fiabilid_matriz == "Biserial"){
      
      Datos_Matriz_Corr <- biserial(funcion_datos_fiabilidad())
      a <- Datos_Matriz_Corr$rho
      
    }else if(input$Select_fiabilid_matriz == "Poliserial"){
      
      Datos_Matriz_Corr <- polyserial(funcion_datos_fiabilidad())
      a <- Datos_Matriz_Corr$rho
      
    }
    
    daton <- Fiabilidad_daton()
    
    bartlett <- cortest.bartlett( a, n = length(rownames(daton) ))  # cortest.bartlett()  matriz policorica/pearson 

    data <- data.frame( "Estadistico(Chi-Cuadrado)" = round(bartlett[["chisq"]],2), "G.L." = as.character(round(bartlett[["df"]],2)),
                        "p-Valor" = as.character(formatC(bartlett[["p.value"]], format = "e", digits = 2)) )
    
    colnames(data) <- c("Estadístico (Chi-Cuadrado)","g.l.","p-valor")
    
   
    return(data)
    
    
  })
  
  
  Alpha_table_drop <- eventReactive(input$Boton_fiabilidad,{
    
    Datos_Matriz_Corr <- list()
    items <- list()
    
    if(input$alfa_fia_box == 1){
    
      if(input$Select_fiabilid_matriz == "Pearson"){
        
        Datos_Matriz_Corr <- cor(funcion_datos_fiabilidad(),method = "pearson")
        a <- psych::alpha(Datos_Matriz_Corr)
        
        
        
      }else if(input$Select_fiabilid_matriz == "Policórica"){
        
        Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
        a <- psych::alpha(Datos_Matriz_Corr$rho)

        
      }else if(input$Select_fiabilid_matriz == "Tetracorica"){
        
        Datos_Matriz_Corr <- tetrachoric(funcion_datos_fiabilidad())
        a <- psych::alpha(Datos_Matriz_Corr$rho)

        
      }else if(input$Select_fiabilid_matriz == "Biserial"){

        Datos_Matriz_Corr <- biserial(funcion_datos_fiabilidad())
        a <- psych::alpha(Datos_Matriz_Corr$rho)

        
      }else if(input$Select_fiabilid_matriz == "Poliserial"){
        
        
        Datos_Matriz_Corr <- polyserial(funcion_datos_fiabilidad())
        a <- psych::alpha(Datos_Matriz_Corr$rho)

        
      }
      
      return(cbind(Items = rownames(a[["alpha.drop"]]), round(a[["alpha.drop"]],2) ))
      
      
    }
    
    else if(input$Select_fiabilid_alfa == "SplitHalf"){
      
      Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
      a <- splitHalf(Datos_Matriz_Corr$rho)
      
      data <- data.frame( "Max_SplitHalf_lambda_4" = a[["maxrb"]], "Guttman_lambda_6"  = a[["lambda6"]], "AVG_SplitHalf"  = a[["meanr"]],
                          "Guttman_lambda_3_alpha"  = a[["alpha"]], "Guttman_lambda_2"  = a[["lambda2"]], "Min_SplitHalf"  = a[["minrb"]], 
                          "AVG_interitem_r"  = a[["av.r"]], "AVG_interitem_r_median"  = a[["med.r"]] )
      
      
      return(round(data,2))
      
    }else if(input$Select_fiabilid_alfa == "Omega Ordinal"){
      

      Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
      a <- omega(Datos_Matriz_Corr$rho)
      return(round(a[["schmid"]][["sl"]],2))
      
    }
    
    
    
  })
  
  Alpha_table_total <- eventReactive(input$Boton_fiabilidad,{
    
    
    Datos_Matriz_Corr <- list()
    items <- list()
  
    if(input$alfa_fia_box == 1){

      if(input$Select_fiabilid_matriz == "Pearson"){
       
        Datos_Matriz_Corr <- cor(funcion_datos_fiabilidad(),method = "pearson")
        a <- psych::alpha(Datos_Matriz_Corr)
        return(round(a[["total"]],2))
        
      }else if(input$Select_fiabilid_matriz == "Policórica"){
        
        Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
        a <- psych::alpha(Datos_Matriz_Corr$rho)
        return(round(a[["total"]],2))
        
      }else if(input$Select_fiabilid_matriz == "Tetracorica"){
  
        Datos_Matriz_Corr <- tetrachoric(funcion_datos_fiabilidad())
        a <- psych::alpha(Datos_Matriz_Corr$rho)
        return(round(a[["total"]],2))
        
      }else if(input$Select_fiabilid_matriz == "Biserial"){
        
        Datos_Matriz_Corr <- biserial(funcion_datos_fiabilidad())
        a <- psych::alpha(Datos_Matriz_Corr$rho)
        return(round(a[["total"]],2))
        
      }else if(input$Select_fiabilid_matriz == "Poliserial"){
        
        Datos_Matriz_Corr <- polyserial(funcion_datos_fiabilidad())
        a <- psych::alpha(Datos_Matriz_Corr$rho)
        return(round(a[["total"]],2))
      }
    
    }
    
  })
  
  split_table_1 <- eventReactive(input$Boton_fiabilidad,{
    
    Datos_Matriz_Corr <- list()
    items <- list()
    
    Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
    a <- splitHalf(Datos_Matriz_Corr$rho)
    
    data <- data.frame( "Max_SplitHalf_lambda_4" = a[["maxrb"]], "Guttman_lambda_6"  = a[["lambda6"]],
                        "AVG_SplitHalf"  = a[["meanr"]], "Guttman_lambda_3_alpha"  = a[["alpha"]])
    
    return(round(data,2))
    
    
  })
  
  split_table_2 <- eventReactive(input$Boton_fiabilidad,{
    
    Datos_Matriz_Corr <- list()
    items <- list()
    
    Datos_Matriz_Corr <- polychoric(funcion_datos_fiabilidad())
    a <- splitHalf(Datos_Matriz_Corr$rho)
    
    data <- data.frame(  "Guttman_lambda_2"  = a[["lambda2"]], "Min_SplitHalf"  = a[["minrb"]],
                         "AVG_interitem_r"  = a[["av.r"]], "AVG_interitem_r_median"  = a[["med.r"]])
    
    return(round(data,2))
    
    
  })
  

  

  
  Cronbach_table <- eventReactive(input$Boton_fiabilidad,{
    
    Fichero_csv <- paste(ruta,input$Select_Arc_graph_cat,sep="")
    data1 <- read.csv(Fichero_csv)
    
    daton <- data.frame()
    
    for (i in 1:length(input$Select_graph_cat1)){
      algo <- data.frame("cambio" = data1[, input$Select_graph_cat1[i]] )
      names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_graph_cat1[i] ) 
      if(i == 1){
        daton <- algo
      }else{
        daton <- bind_cols(daton, algo)
      }
      
    }
   
     datos_alpha <- alpha(daton)
     return(round(datos_alpha[["alpha.drop"]],2))
  })
  
  Omega_Ordinal_table <- eventReactive(input$Boton_fiabilidad,{
    
    Fichero_csv <- paste(ruta,input$Select_Arc_graph_cat,sep="")
    data1 <- read.csv(Fichero_csv)
    
    daton <- data.frame()
    
    for (i in 1:length(input$Select_graph_cat1)){
      algo <- data.frame("cambio" = data1[, input$Select_graph_cat1[i]] )
      names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_graph_cat1[i] ) 
      if(i == 1){
        daton <- algo
      }else{
        daton <- bind_cols(daton, algo)
      }
      
    }
    
    
    General_poly<-polychoric(daton)
    
    omeguita <- omega(General_tetra$rho)
    
    return(round(omeguita[["omega.tot"]],2))
  })
  
  Omega_table <- eventReactive(input$Boton_fiabilidad,{
    
    Fichero_csv <- paste(ruta,input$Select_Arc_graph_cat,sep="")
    data1 <- read.csv(Fichero_csv)
    
    daton <- data.frame()
    
    for (i in 1:length(input$Select_graph_cat1)){
      algo <- data.frame("cambio" = data1[, input$Select_graph_cat1[i]] )
      names(algo) <- gsub(x = names(algo), pattern = "cambio", replacement = input$Select_graph_cat1[i] ) 
      if(i == 1){
        daton <- algo
      }else{
        daton <- bind_cols(daton, algo)
      }
      
    }
    
    datos_omega <- Omega(daton)
    return(round(datos_omega[["schmid"]][["RMSEA"]],2))
  })
  
  
  
  
  output$table_contingency <- renderTable({
    return(Contingency())
  })
  
  output$table_num_factores_as_data <- renderTable({
    return(Tabla_num_factores())
    }, digits=0)
  
  output$table_num_factores_summary <- renderTable({
    return(Tabla_num_factores_summary())
  }, digits=0)
  
  output$table_policorica_Weights <- renderTable({
    return(Policorica_Weights())
  })
  
  
  
  
  output$table_predicciones_factores <- renderTable({
    return(Policorica_Predicciones())
  })
  
  output$table_predicciones_text <- renderTable({
    return(Text_Predicciones())
  })
  
  
  output$table_describe <- renderTable({
    return(Tabla_describe())
  })
  

  
  output$table_frecuency <- renderTable({
    return(Tabla_frecuency())
  })
  
  output$tabla_frecuency2 <- renderTable({
    return(Tabla_frecuency2())
  })
  

  
  output$table_ttest <- renderTable({
    
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    num = nrow(distinct(data1[input$Select_compa1])) 
    
    if(num <= 2){
      return(Tabla_ttest())
    }
    
  })
  
  output$table_independencia <- renderTable({
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    num = nrow(distinct(data1[input$Select_compa1])) 
    if(num <= 2){
      return(Tabla_independencia()) 
    }
  })
  
  output$table_independencia_freq <- renderTable({
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    num = nrow(distinct(data1[input$Select_compa1])) 
    if(num <= 2){
      return(Tabla_independencia_freq()) 
    }
  })
  
  
  output$table_varianza <- renderTable({
    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    num = nrow(distinct(data1[input$Select_compa1])) 
    if(num <= 2){
      return(Tabla_varianza())
    }
  })
  
 
  
  
  output$table_normalidad <- renderTable({

    Fichero_csv <- paste(ruta,input$Select_Arc_factorial,sep="")
    data1 <- read.csv(Fichero_csv)
    num = nrow(distinct(data1[input$Select_compa1])) 
    if(num <= 2){
     return(Tabla_normalidad())
    }
    
  })
  
  output$table_normalidad_anova <- renderTable({
    
    return(Tabla_normalidad_anova())
    
  })
  
  output$table_durbin <- renderTable({
    
    return(Tabla_Durbin())
    
  })
  
  output$table_homocedasticidad <- renderTable({
    
    return(Tabla_Homocedasticidad())
    
  })
  
  
  output$table_desviaciones <- renderTable({
    
    return(Tabla_Desviaciones())
    
  })

  
  output$table_Anova <- renderTable(na = " ",{
    
      return(Tabla_Anova())
  })
  
  output$table_tamano_Anova <- renderTable({
    
      return(Tabla_Tamano_Anova())
  })
  
  output$table_Bonferroni <- renderTable(na = " ",{
    return(Tabla_Bonferroni())
  })
  
  output$table_leyenda  <- renderTable({
    return(Tabla_Leyenda())
  })
  
  output$table_policorica_Values <- renderTable({
    return(Policorica_Values())
  })
  
  output$table_policorica_Uniquenesses <- renderTable({
    return(Policorica_Uniquenesses())
  })
  
  output$table_policorica_Communalities <- renderTable({
    return(Policorica_Communalities())
  })
  
  output$table_policorica_Communality <- renderTable({
    return(Policorica_Communality())
  })
  
  output$table_policorica_R2 <- renderTable({
    return(Policorica_R2())
  })
  
  output$table_policorica_RMSEA <- renderTable({
    return(Policorica_RMSEA())
  })
  
  
  output$table_bloque_3 <- renderTable({
    return(Bloque_3())
  })
  
  
  
  output$tabla_texto_RMSEA <- renderTable({
    return(texto_RMSEA())
  })
  
  output$table_policorica_Complexity <- renderTable({
    return(Policorica_Complexity())
  })

  
  output$table_policorica_Vaccounted <- renderTable({
    return(Policorica_Vaccounted())
  })
  


    
  output$table_policorica_Structure <- renderTable({
    return(Policorica_Structure())
  })
  
  
  output$table_alpha_total <- renderTable({
    return(Alpha_table_total())
  })
  
  output$table_alpha_drop <- renderTable({
    return(Alpha_table_drop())
  })
  
  
  output$table_split_1 <- renderTable({
    return(split_table_1())
  })
  
  output$table_split_2 <- renderTable({
    return(split_table_2())
  })
  

  
  
  output$table_KMO <- renderTable({
    return(KMO_table())
  })
  
  output$table_KMO1 <- renderTable({
    return(KMO_table1())
  })
  
  output$table_Bartlett <- renderTable({
    return(Bartlett_table())
  })
  
  output$table_indicadores1 <- renderTable({
    return(Indicadores1_table())
  })
  
  output$table_indicadores2 <- renderTable({
    return(Indicadores2_table())
  })
  

  
  output$table_cronbach <- renderTable({
    return(Cronbach_table())
  })
  
  output$table_omega_ordinal <- renderTable({
    return(Omega_Ordinal_table())
  })
  
  output$table_omega <- renderTable({
    return(Omega_table())
  })
  
  
  
  
  output$selectfile_graph_cat <- renderUI({
    
    
    if(is.null(input$files)) {return()}
    
    List_of_file_paths <- list.files(path ="E:/Pruebas_R_CSV_WRITE/", pattern = ".csv", all.files = TRUE, full.names = FALSE)
    
    selectInput("Select_Arc_graph_cat", "Seleccione el archivo", 
                choices  = List_of_file_paths ,selected = NULL)
    
  })
  
  output$selecciom_graph_cat1 <- renderUI({
    
    
    req(input$files)
    
    
    Fichero_csv <- paste(ruta,input$Select_Arc_graph_cat,sep="")
    
    #selectInput("Select_graph_cat1", "Selecciona la variable", 
    #            choices  = colnames(read.csv(Fichero_csv)),selected = NULL, multiple = TRUE)
    
    tags$div(
     # style=" background-color:#ffffff",# border:3px inset;border-color:#2c3c54;background-color:#ffffff
      
    pickerInput("Select_graph_cat1","Selecciona la variable", 
                choices= colnames(read.csv(Fichero_csv)), options = list(`actions-box` = TRUE, style = "btn-link"),multiple = T)
    
                #choicesOpt = list(
                  #style = rep(("color: black; background: lightgrey; font-weight: bold;"),10)))
    )
    
  })
  
  output$selecciom_fiabilid_matriz <- renderUI({
    
    selectInput("Select_fiabilid_matriz", "Seleccionar tipo de matriz", 
            choices  = c("Pearson", "Policórica", "Tetracorica", "Biserial", "Poliserial"),selected = NULL)
                
  })
  

  

  
  output$selecciom_graph_cat2  <- renderUI({
    
    req(input$files)
    
    selectInput("Select_graph_cat2", "Selecciona la variable", 
                choices  = colnames(read.csv(input$Select_Arc_graph_cat)),selected = NULL)
    
    
  })
  
  
  output$Mosaicos <- renderPlot({
    
    Fichero_csv <- paste(ruta,input$Select_Arc_graph_cat,sep="")
    Mosaicos <- read.csv(Fichero_csv)
    
    x <- Mosaicos[,input$Select_graph_cat1]
    y <- Mosaicos[,input$Select_graph_cat2]
    return ( mosaicplot(~ x + y, 
                        data = Mosaicos, 
                        color = 2:7, 
                        las = 1) ) 
  })
  
  
  output$Apiladas <- renderPlot({
    
    Fichero_csv <- paste(ruta,input$Select_Arc_graph_cat,sep="")
    Barras_apiladas <- read.csv(Fichero_csv)
    
    x <- Barras_apiladas[,input$Select_graph_cat1]
    y <- Barras_apiladas[,input$Select_graph_cat2]

     p <- ggplot(data=Barras_apiladas, mapping = aes(x = factor(x), fill = factor(y))) 

     return ( p + geom_bar(position = 'stack', stat = 'count')  +
                ggtitle ("Gráfico de barras apilados") + theme (plot.title = element_text(family="Comic Sans MS",
                                                                                          size=rel(1.5), #Tamaño relativo de la letra del título
                                                                                          vjust=2, #Justificación vertical, para separarlo del gráfico
                                                                                          face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                                                                          color="black", #Color del texto
                                                                                          hjust = 0.5,
                                                                                          lineheight=1.5)))
                 
  })
  
  output$Agrupadas <- renderPlot({
    
    Fichero_csv <- paste(ruta,input$Select_Arc_graph_cat,sep="")
    Barras_Agrupadas <- read.csv(Fichero_csv)
    
    x <- Barras_Agrupadas[,input$Select_graph_cat1]
    y <- Barras_Agrupadas[,input$Select_graph_cat2]
    
    
    ggplot(Barras_Agrupadas, aes(fill = condition, y = y, x = x)) + 
      geom_bar(position="dodge", stat="identity")
  })
  

  

  
  output$selecciom_columna_bidi_1 <- renderUI({
    
    req(input$files)
    
    # df[, !(colnames(read.csv(Fichero_csv)) %in% c("x","bar","foo"))]
    
    Fichero_csv <- paste(ruta,input$Select_Arc_bidi,sep="")
    
    algo <- read.csv(Fichero_csv)
    
    algo <- algo[, !(colnames(algo) %in% c("Dni.Alumno","Dni.Tutor.Academico","Dni.Tutor.Externo"))]
    
    selectInput("Select_bidi1", "Selecciona la primera variable", 
                choices  = colnames(algo),selected = NULL)
    
    
  })
  
  output$selecciom_columna_bidi_2 <- renderUI({
    
    req(input$files)
    Fichero_csv <- paste(ruta,input$Select_Arc_bidi,sep="")
    
    algo <- read.csv(Fichero_csv)
    
    algo <- algo[, !(colnames(algo) %in% c("Dni.Alumno","Dni.Tutor.Academico","Dni.Tutor.Externo"))]
    
    selectInput("Select_bidi2", "Selecciona la segunda variable", 
                choices  = colnames(algo) ,selected = NULL)
    
    
  })
  
  

  
  output$BoxPlot_bidi <- renderPlot({
    
    if((input$Graficos_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){
      Fichero_csv <- paste(ruta,input$Select_Arc_bidi,sep="")
      a <- read.csv(Fichero_csv)
      
      if( ( (input$Select_bidi1 %in% variables_continuas )== TRUE)  ){ 
        vals1 <- (a[, input$Select_bidi1])
        vals2 <- ( a[, input$Select_bidi2] )
        name_x <- input$Select_bidi1
        name_fill <- input$Select_bidi2
        
      }else{
        vals1 <- (a[, input$Select_bidi2])
        vals2 <- (a[, input$Select_bidi1] )
        name_x <- input$Select_bidi2
        name_fill <- input$Select_bidi1
        
      }

    

      
      
      redondeo <- unique(round(vals1, digits = 1) )
      
      redondeo <- sub(".1", "", redondeo)
      redondeo <- sub(".2", "", redondeo)
      redondeo <- sub(".3", "", redondeo)
      redondeo <- sub(".4", "", redondeo)
      redondeo <- sub(".6", "", redondeo)
      redondeo <- sub(".7", "", redondeo)
      redondeo <- sub(".8", "", redondeo)
      redondeo <- sub(".9", "", redondeo)
      
      redondeo <- as.numeric(unique(redondeo))
    

      
      p <- ggplot(a, aes(x = vals1, y = vals2)) + geom_boxplot( ) + geom_point(data = a, x = 0, colour='red', size=2.5) +
        labs(x = name_x, y = name_fill) + theme(axis.text.x = element_text(size = 14),
                                                axis.text.y = element_text(size = 14),
                                                axis.title = element_text(size = 14),
                                                axis.text = element_text(size = 14),legend.text = element_text(size = 14)) + 
        scale_x_discrete(limits=redondeo) 
                                           
                                               
      
      return(p)
    }
    return()
  })
  
  output$Barras_bidi <- renderPlot({
    
    if((input$Graficos_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){
      
      Fichero_csv <- paste(ruta,input$Select_Arc_bidi,sep="")
      a <- read.csv(Fichero_csv)
      
      #ok <- table(a[, input$Select_bidi1],a[, input$Select_bidi2])
      
     
      

      ok <- data.frame( col1 = a[, input$Select_bidi1], col2 = a[, input$Select_bidi2])
      
      ok <- ok %>% group_by(col1,col2) %>% tally()

   
      
      return( ggplot(ok, aes(x=as.factor(col2), y=n, fill=as.factor(col1) )) +
                geom_bar(stat='identity', position = position_dodge(preserve = "single"))+
                scale_fill_brewer() +
               # geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
                theme(text = element_text(size=14),
                      axis.text.x = element_text(size = 14),
                      axis.text.y = element_text(size = 14),
                      axis.title = element_text(size = 14),
                      axis.text = element_text(size = 14),
                      legend.text = element_text(size = 14),
                      panel.grid = element_line(color = "#4c5b6b",
                                                size = 0.75,
                                                linetype = 2),
                      
                      panel.background = element_rect(fill = "#2b3e50"),
                      plot.background = element_rect(fill = "#2b3e50"),
                      legend.background = element_rect(fill = "#2b3e50"),
                      plot.title = element_text(
                        size=rel(1.5), 
                        vjust=2, 
                        face="bold", 
                        color="black", 
                        hjust = 0.5,
                        lineheight=1.5)) +
                xlab(input$Select_bidi2) + ylab("Cantidades") + scale_fill_discrete(name = input$Select_bidi1)
                )
      
      
      
      
    }
    return()
  })
  
  output$Histogram_bidi <- renderPlot({
    Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
    a <- read.csv(Fichero_csv)
    
    k <- nclass.Sturges(a[[input$Select_bidi1]])
    
    vals1 <- (a[, input$Select_bidi1])
    vals2 <- (a[, input$Select_bidi2])
    name_x <- input$Select_bidi1
    name_fill <- input$Select_bidi2
    
    if( ( (input$Select_bidi1 %in% variables_continuas )== TRUE)  ){ 
      vals1 <- (a[, input$Select_bidi1])
      vals2 <- ( a[, input$Select_bidi2] )
      name_x <- input$Select_bidi1
      name_fill <- input$Select_bidi2
      
    }else{
      vals1 <- (a[, input$Select_bidi2])
      vals2 <- (a[, input$Select_bidi1])
      name_x <- input$Select_bidi2
      name_fill <- input$Select_bidi1
      
    }
      

    
    A = diff(range(vals1)) / k
    i = min(vals1)
    L <- i - 0.05 + A *(0:k)
    L_inferior = L[0:k]
    
    
    c <- data.frame("col1" = vals1, "col2" = vals2)
    
    
    #############################################################3 
    
    # return( ggplot(c, aes(x = col1)) +
    #  geom_histogram(fill = "white", colour = "black") +
    #  facet_grid(col2 ~ ., scales = "free") )
    
    
    
    p <- ggplot(c, aes(x=col1) )+ geom_histogram(color="#e9ecef",alpha=0.6, breaks = round(L_inferior,digits = 1) ,binwidth = 8) +
      facet_grid(col2 ~ ., scales = "free") +
      xlab(name_x) + ylab("Cantidades")  + 
        theme(panel.grid = element_line(color = "#4c5b6b",size = 0.75,linetype = 2),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              legend.position="none",
              panel.spacing = unit(1, "lines"),
              strip.text.x = element_text(size = 14),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 14),legend.text = element_text(size = 14),
                       panel.background = element_rect(fill = "#2b3e50"),
                       plot.background = element_rect(fill = "#2b3e50"), 
                       legend.background = element_rect(fill = "#2b3e50")) +
      scale_x_continuous(breaks = round(L_inferior,digits = 1)   ) +
      facet_wrap(~col2)
return(p + labs(fill = name_fill))
    
    
    
    
  })
  
  
  
  
  
  
  output$Dispersion_puntos_bidi <- renderPlot({
    
    if((input$Graficos_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){
      
      Fichero_csv <- paste(ruta,input$Select_Arc_bidi,sep="")
      archivo_csv <- read.csv(Fichero_csv)
      
      p <-  plot(archivo_csv[, input$Select_bidi1], archivo_csv[, input$Select_bidi2], pch = 19, col = "black",
                 xlab = input$Select_bidi1, ylab = input$Select_bidi2 )
      return(p)
    }
    
  })
  
  output$Pieplot_bidi <- renderPlot({
    
    if((input$Graficos_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){
      
      Fichero_csv <- paste(ruta,input$Select_Arc_bidi,sep="")
      a <- read.csv(Fichero_csv)
      
      
      
      
      ok <- table(a[, input$Select_bidi1],a[, input$Select_bidi2])
      ok <- data.frame( col1 = a[, input$Select_bidi1], col2 = a[, input$Select_bidi2])
      ok <- ok %>% group_by(col1,col2) %>% tally()
      
      return( ggplot(data = ok, aes(x = "",y = n, fill = as.factor(col1) )) + 
                geom_bar(stat = "identity", position = position_fill()) +
                geom_text(aes(label = n), position = position_fill(vjust = 0.5)) +
                coord_polar(theta = "y") +
                scale_fill_brewer(palette = "Set1") +
                facet_grid(. ~ as.factor(col2) ) +
                theme(text = element_text(size=14),
                      
                      
                      axis.title = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid = element_blank(),
                      panel.background = element_rect(fill = "#2b3e50"),
                      plot.background = element_rect(fill = "#2b3e50"),
                      legend.background = element_rect(fill = "#2b3e50"),
                      plot.title = element_text(
                        size=rel(1.5), 
                        vjust=2, 
                        face="bold", 
                        color="black", 
                        hjust = 0.5,
                        lineheight=1.5)) + scale_fill_discrete(name = input$Select_bidi1) 

      )
      
      
      
      
    }
    return()
  })
  
  
  observeEvent(input$Escalar,{
    
    Fichero_csv <- paste(ruta,input$Select_Arc_categorizar ,sep="")
    data <- read.csv(Fichero_csv)
    Var_Escala <- round(scale(data[, input$Select_col_categorizar]),2)
    data <- cbind(data,escala)
    write.csv(data, file = Fichero_csv, row.names = F)

  })
  
  
  

  
  observeEvent(input$categorizacion,{
    
    
     
     Fichero_csv <- paste(ruta,input$Select_Arc_categorizar ,sep="")

     data <- read.csv(Fichero_csv)
     array <- input$marcas_cat
     

     
     array <-  str_split(array," < ",simplify = TRUE ) 
     array <-  str_split(array," > ",simplify = TRUE )
     array <-  str_split(array,"<",simplify = TRUE )
     array <-  str_split(array,">",simplify = TRUE )
     array <-  str_split(array,",",simplify = TRUE )

     
     array <- as.numeric(array)
     

     array <- sort(array,method = "shell")
     

     for (i in 1:length(array)-1){
     
      
       for (j in 1:nrow(data)){
     
         dato <- data[j, input$Select_col_categorizar]

         if( ( dato > array[i] ) && ( dato <= array[i+1])){
           
           data[j,input$nombres_cat] <- i 
           
         }
         
       }
       
     }
       
     write.csv(data, file = Fichero_csv, row.names = F)
     añadir_variables()
     return(write.csv(data, file = Fichero_csv, row.names = F))
     
    
  })
  
  
  observeEvent(input$delete,{
    
    #validate(need(input$Select_Arc_arreglo != "", ""))
    Fichero_csv <- paste(ruta,input$Select_Arc_arreglo,sep="")
    data <- read.csv(Fichero_csv)
    go <- input$Select_col_arreglo
    data <- data[-input$rowno,] #input$rowno
    
    values$df <- data
    return(write.csv(data, file = Fichero_csv, row.names = F))
    
  })
  
  observeEvent(input$cambiar_elemento,{
    
    Fichero_csv <- paste(ruta,input$Select_Arc_correcion,sep="")
    
    data <- read.csv(Fichero_csv)
    

    for (i in 1:nrow(data)){
      
      data[i, input$Select_col_correcion] <- gsub(input$caption, input$substituto,data[i, input$Select_col_correcion])
      
    }
    write.csv(data, file = Fichero_csv, row.names = F)
    hz <- 1
    hz <- data[1, input$Select_col_correcion]
    if( class(as.integer(hz)) == "integer") {   # (class(hz) == "numeric") |
      
     
      a <- unique(data[,input$Select_col_correcion])
     
      a <- length(a)
      
      
      if(a < 12){
        variables_discretas <<- c(variables_discretas, input$Select_col_correcion) 
      }
      else if(a >= 12){
        variables_continuas <<- c(variables_continuas, input$Select_col_correcion) 
      }
     
    }
    
   
    return(data)
  })
  
  
  observeEvent(input$redondear,{
    
    Fichero_csv <- paste(ruta,input$Select_Arc_correcion,sep="")
    
    data <- read.csv(Fichero_csv)
    
    
    test <- 1
    size <- 1
    comprobacion <- 1
    
    texto <- paste(input$Select_col_redondear,"redondeo",sep="_")

    
    
    if(class(data[, input$Select_col_redondear]) == "character"){

      
      for (i in 1:nrow(data)){
        
        test[i] <- gsub(",", ".",data[i, input$Select_col_redondear])
        test[i] <- gsub('"', "",test[i])
        
      }
      
      comprobacion <- round(as.integer(test[1]),0)

      if(class(comprobacion) == "numeric"){
        
        for (i in 1:nrow(data)){
          
          data[i,texto] <- round(as.integer(test[i]),0)
          
        }
        
        write.csv(data, file = Fichero_csv, row.names = F)
        
        a <- unique(data[,texto])
        a <- length(a)
        
        if(a < 12){
          variables_discretas <<- c(variables_discretas, texto) 
        }
        else if(a >= 12){
          variables_continuas <<- c(variables_continuas, texto) 
        }
        
        
      }
      
    }
    
    if(class(data[, input$Select_col_redondear]) == "numeric"){
      for (i in 1:nrow(data)){
        
        data[i,texto] <- round(data[i, input$Select_col_redondear],0)
        
      }
      
      write.csv(data, file = Fichero_csv, row.names = F)
      
      
      a <- unique(data[,texto])
      a <- length(a)
      
      if(a < 12){
        variables_discretas <<- c(variables_discretas, texto) 
      }
      else if(a >= 12){
        variables_continuas <<- c(variables_continuas, texto) 
      }
     
      #write.csv(data, file = Fichero_csv, row.names = F)
      
      #añadir_variables()
    }
    

   
  })
  
  observeEvent(input$Cambiar_nombre,{
    
    Fichero_csv <- paste(ruta,input$Select_Arc_correcion,sep="")
    data <- read.csv(Fichero_csv)
    names(data)[names(data) == input$Select_col_correcion] <- input$Nombre_Columna_Cambio
    
    return(write.csv(data, file = Fichero_csv, row.names = F))
    
  })
    
  
  observeEvent(input$arreglo_do,{
    
    
    Fichero_csv <- paste(ruta,input$Select_Arc_arreglo,sep="")
    data <- read.csv(Fichero_csv)
    #k <- nclass.Sturges(input$Select_col_arreglo)
    
    go <- input$Select_col_arreglo
 
    data <- data[, !(names(data)  %in% as.character(go)), drop = F ]
    #data <- subset( data, select = -c(as.character(go))  )   #Borrar Columnas
    #data <- data[-2,]                                    Borrar Filas
    
    
    
    #data <- data[ !(names(data)  %in% c(43836375)), ]
    
    #for (i in 1:nrow(data)){
    
    #data$Notas_kek[i] <- data$NOTA10_SS[i] > 6  # añade columnas
    #data$NOTA10_SS[i] <- substring(data$NOTA10_SS[i],1,1) #Trunca el valor
    #data$NOTA10_SUP[i] <- substring(data$NOTA10_SUP[i],1,1)
    #data$CrÃ.d..FB.[i] <-  data$CrÃ.d..FB.[i]/6
    #data$CrÃ.d..FT[i] <-  data$CrÃ.d..FT[i]/6
    #data$CrÃ.d..EP[i] <-  data$CrÃ.d..EP[i]/6
    #data$CrÃ.d..Itin.[i] <-  data$CrÃ.d..Itin.[i]/6
    #data$NÃºm..Cred..faltan.req..Req.[i] <-  data$NÃºm..Cred..faltan.req..Req.[i]/6
    #data$CRED_SUPERADOS[i] <-  data$CRED_SUPERADOS[i]/6
    #data$CRED_FALTAN_TOT.[i] <-  data$CRED_FALTAN_TOT.[i]/6
    
    #}
    
    values$df <- data
    return(write.csv(data, file = Fichero_csv, row.names = F))
    
    
  })
  
  
  
  
  output$selecciom_columna_arreglo <- renderUI({
    
    validate(need(input$Select_Arc_arreglo != "", ""))
    req(input$files)
    
    Fichero_csv <- paste(ruta,input$Select_Arc_arreglo,sep="")
    data <- read.csv(Fichero_csv)
    
    values$df <- data
    
    selectInput("Select_col_arreglo", "Selecciona la columna", 
                choices  = colnames(values$df),selected = NULL)
    
    
  })
  
  
  
  
  
  output$selecciom_columna_correcion <- renderUI({
    
    validate(need(input$Select_Arc_correcion != "", ""))
    req(input$files)
    
    Fichero_csv <- paste(ruta,input$Select_Arc_correcion,sep="")
    data <- read.csv(Fichero_csv)
    
    values$df <- data
    
    selectInput("Select_col_correcion", "Selecciona la columna", 
                choices  = colnames(values$df),selected = NULL)
    
    
  })
  
  
  output$selecciom_columna_categorizar <- renderUI({
    
    validate(need(input$Select_Arc_categorizar != "", ""))
    req(input$files)
    
    Fichero_csv <- paste(ruta,input$Select_Arc_categorizar,sep="")
    data <- read.csv(Fichero_csv)
    
    values$df <- data
    
    selectInput("Select_col_categorizar", "Selecciona la columna", 
                choices  = colnames(values$df),selected = NULL)
    
    
  })
  
  output$selecciom_columna_redondear <- renderUI({
    
    validate(need(input$Select_Arc_correcion != "", ""))
    req(input$files)
    
    Fichero_csv <- paste(ruta,input$Select_Arc_correcion,sep="")
    data <- read.csv(Fichero_csv)
    
    values$df <- data
    
    selectInput("Select_col_redondear", "Selecciona la columna a redondear", 
                choices  = colnames(values$df),selected = NULL)
    
    
  })
  
  output$selecciom_columna_1 <- renderUI({
    
    validate(need(input$Select_Arc_1 != "", ""))
    req(input$files)
    
   
    
    Fichero_csv <- paste(ruta,input$Select_Arc_1,sep="")
  
    texto_split = str_split_fixed(Fichero_csv," ", 1)
    columnas_union <- ""
    for(nr in 1:length(texto_split)){
      
      
      
      columnas_union <- c(columnas_union,colnames(read.csv(texto_split[nr,])))
      
    }
    
    #texto_columnas = data.frame(unlist(texto_split))

    
    selectInput("Select_mer1", "Selecciona la columna", 
                choices  = columnas_union,selected = NULL,multiple = TRUE)
    
    
  
    
    #colnames(get(dataMerge())))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(fusion(), file)
    }
  )
  
  
  
  fusion <- eventReactive(input$do,{
    
    runif(input$do)


    if(length(input$Select_Arc_1) == 2){
      arch_1 <- read.csv(paste(ruta,input$Select_Arc_1[1],sep=""))
      arch_2 <- read.csv(paste(ruta,input$Select_Arc_1[2],sep=""))
      finale <- Reduce(merge, list(arch_1,arch_2) )
    }
    
    if(length(input$Select_Arc_1) == 3){
      arch_1 <- read.csv(paste(ruta,input$Select_Arc_1[1],sep=""))
      arch_2 <- read.csv(paste(ruta,input$Select_Arc_1[2],sep=""))
      arch_3 <- read.csv(paste(ruta,input$Select_Arc_1[3],sep=""))
      finale <- Reduce(merge, list(arch_1,arch_2,arch_3) )
    }
    
    if(length(input$Select_Arc_1) == 4){
      
      arch_1 <- read.csv(paste(ruta,input$Select_Arc_1[1],sep=""))
      arch_2 <- read.csv(paste(ruta,input$Select_Arc_1[2],sep=""))
      arch_3 <- read.csv(paste(ruta,input$Select_Arc_1[3],sep=""))
      arch_4 <- read.csv(paste(ruta,input$Select_Arc_1[4],sep=""))
      finale <- Reduce(merge, list(arch_1,arch_2,arch_3,arch_4) )
    }
    
    if(length(input$Select_Arc_1) == 5){
      arch_1 <- read.csv(paste(ruta,input$Select_Arc_1[1],sep=""))
      arch_2 <- read.csv(paste(ruta,input$Select_Arc_1[2],sep=""))
      arch_3 <- read.csv(paste(ruta,input$Select_Arc_1[3],sep=""))
      arch_4 <- read.csv(paste(ruta,input$Select_Arc_1[4],sep=""))
      arch_5 <- read.csv(paste(ruta,input$Select_Arc_1[5],sep=""))
      finale <- Reduce(merge, list(arch_1,arch_2,arch_3,arch_4,arch_5) )
    }

    arch_1 <- paste(ruta,"Archivo_Fusionado.csv",sep="")
    
    
    #for (i in 1:nrow(finale)){
      
      #if("Lugar" %in% colnames(finale)){
        
      
        #if(finale[i,"Lugar"] == ""){
          #finale <- finale[!(i),]
          #}
        # }
      
    #}
    
    write.csv(finale, file = arch_1, row.names = F)
    finale

  })
  
  
  output$table_fusion <- renderDT({
    
    
    
    return (datatable(fusion(), style ="bootstrap",options = list(lengthMenu = c(5,10),
                                                                  columnDefs = list(list(
                                                                    targets = "_all",
                                                                    render = JS(
                                                                      "function(data, type, row, meta) {",
                                                                      "return type === 'display' && data != null && data.length > 30 ?",
                                                                      "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                                      "}")
                                                                    
                                                                  ))),class = "display") )
    
  })
  
  output$selecciom_columna_2 <- renderUI({
    
    req(input$files)
    
    Fichero_csv <- paste(ruta,input$Select_Arc_2,sep="")
    
    selectInput("Select_mer2", "Selecciona la columna", 
                choices  = colnames(read.csv(Fichero_csv)),selected = NULL)
    
    #colnames(get(dataMerge())))
  })
  
  
  output$selecciom_columna_3 <- renderUI({
    
    req(input$files)
    
    Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
    
    selectInput("Select_mer3", "Seleccionar variable a analizar", 
                choices  = colnames(read.csv(Fichero_csv)),selected = NULL)
    
    #colnames(get(dataMerge())))
  })
  
  output$selecciom_columna_4 <- renderUI({
    
    req(input$files)
    
    Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
    
    selectInput("Select_mer4", "Selecciona la columna", 
                choices  = colnames(read.csv(Fichero_csv)),selected = NULL)
    
    #colnames(get(dataMerge())))
  })
  
  
  output$selecciom_columna_5 <- renderUI({
    
    req(input$files)
    
    Fichero_csv <- paste(ruta,input$Select_Arc_5,sep="")
    
    selectInput("Select_mer5", "Selecciona la columna", 
                choices  = colnames(read.csv(Fichero_csv)),selected = NULL)
    
    #colnames(get(dataMerge())))
  })
  
  output$selecciom_columna_6 <- renderUI({
    
    req(input$files)
    
    Fichero_csv <- paste(ruta,input$Select_Arc_3,sep="")
    
    selectInput("Select_mer6", "Selecciona la columna", 
                choices  = colnames(read.csv(Fichero_csv)),selected = NULL)
    
    #colnames(get(dataMerge())))
  })
  
  output$selecciom_columna_7 <- renderUI({
    
    req(input$files)
    
    Fichero_csv_1 <- paste(ruta,input$Select_Arc_1,sep="")
    Fichero_csv_2 <- paste(ruta,input$Select_Arc_2,sep="")
    
    a <- colnames(read.csv(Fichero_csv_1))
    b <- colnames(read.csv(Fichero_csv_2))
    
    c = append(a,b)
    selectInput("Select_mer7", "Selecciona la columna", 
                choices  = c,selected = NULL)
    
    #colnames(get(dataMerge())))
  })
  
  output$selecciom_columna_8 <- renderUI({
    
    req(input$files)
    
    Fichero_csv_1 <- paste(ruta,input$Select_Arc_1,sep="")
    Fichero_csv_2 <- paste(ruta,input$Select_Arc_2,sep="")
    Fichero_csv_3 <- paste(ruta,input$Select_Arc_3,sep="")
    
    a <- colnames(read.csv(Fichero_csv_1))
    b <- colnames(read.csv(Fichero_csv_2))
    c <- colnames(read.csv(Fichero_csv_3))
    
    d = append(a,b)
    e = append(d,c)
    selectInput("Select_mer5", "Selecciona la columna", 
                choices  = d,selected = NULL)
    
    #colnames(get(dataMerge())))
  })
  
  
  
  output$table4 <- renderTable({
    
    return(dataMerge()) 
    
  })
  
  output$graph <- renderTable ({
    
    Fichero_csv <- paste(ruta,input$Select_Arc_1,sep="")
    a <- read.csv(Fichero_csv)
    hist(a[,input$Select_mer1])
  })
  
  
  Tablas_frecuencias <- eventReactive(input$Vamos,{
    
  
    
    
    if((input$Tablas_box == TRUE) | (input$Todos_box == TRUE) ){
      aux <- 0
      
      
      
      for(nr in 1:length(variables_continuas)){
        
        if(variables_continuas[nr] == input$Select_mer3)
          aux <- 1
        
      }
      
      if( aux != 1){
        for(nr in 1:length(variables_discretas)){
          
          if(variables_discretas[nr] == input$Select_mer3)
            aux <- 2
          
        }
        
      }
      
      if( (aux != 1) & (aux != 2) ){
        aux <- 3
      }
      
      Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
      algo <- read.csv(Fichero_csv, stringsAsFactors=FALSE)
      Variable <- as.data.frame(table(algo[, input$Select_mer3]))
      colnames(Variable)[colnames(Variable) == 'Var1'] <- input$Select_mer3
        
      
      if(aux == 1){
      
        k <- nclass.Sturges(algo[[input$Select_mer3]])
        
        minimo <- min(algo[[input$Select_mer3]])
        maximo <- max(algo[[input$Select_mer3]])
        
        
        c = sum(as.numeric(minimo),as.numeric(maximo))
        
        
        A = diff(range(algo[[input$Select_mer3]])) / k
        i = min(algo[[input$Select_mer3]])
        L <- i - 0.05 + A *(0:k)
        L.inferior = L[0:k]
        L.superior = L[1:k+1]
        Marcas<-(L[0:k]+L[1:k+1]) /2
        vals_agrupada <- cut(algo[[input$Select_mer3]],breaks=L, right=TRUE)
        
        Absoluta <- as.vector(table(vals_agrupada))
        Absoluta.AC <-cumsum(Absoluta)
        Relativa <- as.vector(prop.table(table(vals_agrupada)))
        Relativa.AC <-cumsum(Relativa)
        Porcentaje <- Relativa*100
        tabla_frec = data.frame(L.inferior,L.superior,Marcas,Absoluta,
                                Absoluta.AC,Relativa,Relativa.AC,Porcentaje)
        
        
        return (tabla_frec)
      }
      
      else{
        if(aux == 2){
          colnames(Variable)[which(colnames(Variable) == 'Freq')] <- 'Absoluta'
         
            return(transform(Variable,
                      Absolutas.AC = cumsum(Absoluta),
                      Relativa = round(prop.table(Absoluta),3),
                      Relativa.AC = round(cumsum(prop.table(Absoluta)),3),
                      Porcentaje = ceiling(round(prop.table(Absoluta),3) *100)
                      
            ) )
          
        }
        
        else{
          colnames(Variable)[which(colnames(Variable) == 'Freq')] <- 'Absoluta'
          
          return(transform(Variable,
                           Relativa = round(prop.table(Absoluta),3),
                           Porcentaje = ceiling(round(prop.table(Absoluta),3) *100)
                           
                   ) 
          )
          
        }
      }
      
    }
    
    
    
  })
  
  Tabla_chi_cuadrado <- eventReactive(input$Boton_agrup,{ 
    
   
    cuadrado <- chisq.test(funcion_tabla_bidi())
                                                                           
    chi <- data.frame(XSquare = cuadrado[1],df = cuadrado[2],p = cuadrado[3] ) 
    colnames(chi)[colnames(chi) == 'statistic'] <- 'Chi.Cuadrado'
    colnames(chi)[colnames(chi) == 'parameter'] <- 'Grados.de.Libertad'
    colnames(chi)[colnames(chi) == 'p.value'] <- 'P-Valor'
    
    colnames(chi) <- c("Estadístico (Chi-Cuadrado)","g.l.","p-valor")
   
    return(chi)
    
  })
  
  Tabla_Fisher <- eventReactive(input$Boton_agrup,{ 
    
    temp <-  is.error(fish <- fisher.test(funcion_tabla_bidi()))
    if(temp == TRUE){
      error_salida <- data.frame("Información" = "Debido a la naturaleza de los datos no se pueden mostrar los resultados del test de Fisher")
      return(error_salida)
    }
    fish <- fisher.test(funcion_tabla_bidi())
    
   
    fisher <- data.frame(pvalue = fish[1] )
    colnames(fisher)[colnames(fisher) == 'p.value'] <- 'p-valor'
    
    colnames(fisher) <- c("p-valor")
    return(fisher)
    
  })
  
  
  Tabla_Cramer <- eventReactive(input$Boton_agrup,{ 
    
    #https://r-coder.com/tabla-contingencia-r/
    #https://www.youtube.com/watch?v=V0KOVwoU9gk
    
    if( ( (input$Select_bidi1 %in% variables_cualitativas ) == TRUE) & ( (input$Select_bidi2 %in% variables_cualitativas ) == TRUE) ){ 
      
      
      Tau_B <- KendallTauB(funcion_tabla_bidi(), conf.level=0.95)
      Tau_C <- StuartTauC(funcion_tabla_bidi(), conf.level=0.95)
      D_Somers <- SomersDelta (funcion_tabla_bidi())
      
      CramerV <- data.frame(Tau_B = Tau_B[1],Tau_C = Tau_C[1],D_Somers = D_Somers[1])
      return(CramerV)
      
    } else if( ( (input$Select_bidi1 %in% variables_discretas ) == TRUE) & ( (input$Select_bidi2 %in% variables_discretas ) == TRUE) ){ 
      
      
      Tau_B <- KendallTauB(funcion_tabla_bidi(), conf.level=0.95)
      Tau_C <- StuartTauC(funcion_tabla_bidi(), conf.level=0.95)
      D_Somers <- SomersDelta (funcion_tabla_bidi())
      
      CramerV <- data.frame(Tau_B = Tau_B[1],Tau_C = Tau_C[1],D_Somers = D_Somers[1])
      return(CramerV)
      
    }else if( ( (input$Select_bidi1 %in% variables_continuas ) == TRUE) & ( (input$Select_bidi2 %in% variables_continuas ) == TRUE) ){ 
      
      
      CramerV <- cramerV(funcion_tabla_bidi())
      contingencia <- ContCoef(funcion_tabla_bidi())
      Phi <- Phi(funcion_tabla_bidi())
      CramerV <- data.frame(Coeficiente_Phi = Phi[1],Contingencia = contingencia[1],V_Cramer = CramerV[1])
      return(CramerV)
      
    }else{
      
      
      CramerV <- cramerV(funcion_tabla_bidi())
      contingencia <- ContCoef(funcion_tabla_bidi())
      Phi <- Phi(funcion_tabla_bidi())
      CramerV <- data.frame(Coeficiente_Phi = Phi[1],Contingencia = contingencia[1],V_Cramer = CramerV[1])
      return(CramerV)
    }
    
    
  })
  
  Tabla_de_contingencia <- eventReactive(input$Boton_agrup,{ 
    
   
    
    chi <- chisq.test(funcion_tabla_bidi())
    contingencia <- unname(sqrt(chi$statistic / (chi$statistic + sum(x))))
    contingencia <- data.frame(Contingencia = contingencia[1])
    return( contingencia )
  
    
  })
  
  Tabla_de_Phi_coef <- eventReactive(input$Boton_agrup,{ 
    
    Phi <- unname(sqrt(chisq.test(funcion_tabla_bidi())$statistic / sum(x))) 
    Phi <- data.frame(Coeficientes.Phi = Phi[1])
    
    return(Phi)
    
  })
  
 
  
  
  funcion_tabla_bidi <- function() {
    Fichero_csv <- paste(ruta,input$Select_Arc_bidi,sep="")
    data1 <- read.csv(Fichero_csv)
    
    
    if( ( (input$Select_bidi1 %in% variables_continuas )== TRUE) | ( (input$Select_bidi2 %in% variables_continuas )== TRUE) ){ 
      
      continua <- data1[[input$Select_bidi1]]
      factor <- data1[,input$Select_bidi2]
      
      if(( (input$Select_bidi1 %in% variables_continuas )== TRUE) & ( (input$Select_bidi2 %in% variables_continuas )== TRUE)){
        k <- nclass.Sturges(factor)
        A = diff(range(factor)) / k
        i = min(factor)
        L <- i - 0.05 + A *(0:k)
        factor <- cut(factor,breaks=L, right=TRUE)
        
      }else if((input$Select_bidi1 %in% variables_continuas )== TRUE){
        continua <- data1[[input$Select_bidi1]]
        factor <- data1[,input$Select_bidi2]
      }else{
        
        continua <- data1[[input$Select_bidi2]]
        factor <- data1[,input$Select_bidi1]
      }
      
      k <- nclass.Sturges(continua)
      A = diff(range(continua)) / k
      i = min(continua)
      L <- i - 0.05 + A *(0:k)
      valores_agrupados <- cut(continua,breaks=L, right=TRUE)
      
      
      #xtabs(~valores_agrupados + factor, data = data1 ) 
      
      Tabla_bidi <- table(factor,valores_agrupados)
      
      return(cbind(names = names(Tabla_bidi), Tabla_bidi))
      
    }
    
    if( ( (input$Select_bidi1 %in% variables_continuas )== FALSE) & ( (input$Select_bidi2 %in% variables_continuas )== FALSE) ){ 
      
      
      x <- table(data1[,input$Select_bidi1],data1[,input$Select_bidi2])
      return(cbind(names = names(x), x))
    }
    
  }
  
  Tablas_frecuencias_bidi <- eventReactive(input$Boton_agrup,{
    
    
    return(funcion_tabla_bidi())
    
  })
  
 
  
  descriptivos_1 <- eventReactive(input$Vamos,{
    
    #min(dataMerge()$Select_mer4)
    #min(dataMerge()$DNI)
    #x = toString(input$Select_mer4)
    
    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      
      Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
      
      algo <- read.csv(Fichero_csv)

      vals <- algo[, input$Select_mer3]

      vals = as.numeric(vals)
      
  
      temp <- is.error( quantile(vals) )
      
      if(temp == FALSE){
      
      min(vals)
      max(vals)
      
      range = max(vals) - min(vals)
      sum(vals)
      
      mean(vals) #media
      median(vals) #mediana
      
      #modes(vals) #moda
      #mlv(vals) #moda
 
      quantile(vals)
      
      var(vals) #varianza
      sd(vals) #desviacion tipica
      mfv(vals) #moda

      
      #valor1 = data.frame( Minimo = min(vals), Media = mean(vals),Cuasivarianza = var(vals) ) 
      #valor2 = data.frame( Maxima = max(vals), Mediana = median(vals), CT = sd(vals) )
      #valor3 = data.frame( Rango = max(vals) - min(vals), Moda = mfv(vals), DT = (sqrt(var(vals)) * (length(vals)-1)/length(vals)) )
      
      
      #M_General
      
      Min <- round(min(vals),2)
      Max <- round(max(vals),2)
      Rango <- round(max(vals)- min(vals),2)
      Suma <- sum(vals)
      
      #M_Tendencia_Central
      
      Media <- round(mean(vals),2)
      Mediana <- round(median(vals),2)
      Moda <- round(mfv(vals),2)
      
      #M_Dispersion
      
      Cuasivarianza <- round(var(vals),2)
      Cuasidesviacion_Tipica <- sd(vals,2)
      C_V_Pearson <- sd(vals)/abs(mean(vals))
        
      #M_Posicion
        
      Q1 <-
      Q2 <-
      Q3 <-
      Percentil_5 <-
      Percentil_95 <- 
        
      #M_Forma
        
      Coeficiente_Asimetria <- skewness(vals)
      Coeficiente_Curtosis <- kurtosis(vals)
      
      Minimo <- c(round(min(vals),2), "Maxima", round(mean(vals),2), "Rango", round(max(vals)- min(vals),2) )
      
      Media <- c(round(mean(vals),2), "Mediana", round(median(vals),2), "Moda", round(mfv(vals),2) )
      
      Cuasivarianza <- c(round(var(vals),2), "CT", round(sd(vals,2)), "DT", round((sqrt(var(vals)) * (length(vals)-1)/length(vals)),2)) 
     
      valor = cbind(Min,Max,Rango,Suma)
      #vals <- dataMerge()[, input$Select_mer4]
      
      }
      
      
    }
  })
  
  
  
  
  descriptivos_2 <- eventReactive(input$Vamos,{

    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      
      Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
      algo <- read.csv(Fichero_csv)
      vals <- algo[, input$Select_mer3]
      vals = as.numeric(vals)
      
      Media <- mean(vals)
      Mediana <- median(vals)
      Moda <- as.numeric(names(which.max(table(vals))))
      
      
      #Moda <- mfv(vals)
      
      valor = cbind(Media,Mediana,Moda)
    }
    
  })
  
  descriptivos_3 <- eventReactive(input$Vamos,{
    
    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      
      Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
      algo <- read.csv(Fichero_csv)
      vals <- algo[, input$Select_mer3]
      vals = as.numeric(vals)
      
      Cuasivarianza <- round(var(vals),2)
      Cuasidesviacion.Tipica <- sd(vals,2)
      C.V.Pearson <- sd(vals)/abs(mean(vals))
      
      valor = cbind(Cuasivarianza,Cuasidesviacion.Tipica,C.V.Pearson)
    }
    
  })
  
  descriptivos_4 <- eventReactive(input$Vamos,{
    
    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      
      Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
      algo <- read.csv(Fichero_csv)
      vals <- algo[, input$Select_mer3]
      vals = as.numeric(vals)
      
      Coeficiente.Asimetria <- skewness(vals)
      Coeficiente.Curtosis <- kurtosis(vals)
      
      valor = cbind(Coeficiente.Asimetria,Coeficiente.Curtosis)
    }
    
  })
  
  
  cuartil <- eventReactive(input$Vamos,{
    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      
      Fichero_csv <- paste(ruta,input$Select_Arc_4,sep="")
      algo <- read.csv(Fichero_csv)
      
      vals <- algo[, input$Select_mer3]
      vals = as.numeric(vals)
      cuantiles <- quantile(x = algo[, input$Select_mer3], prob=c(0.05,0.25,0.5,0.75,0.95,1))
      
      
      
      return(nuevo_df <- data.frame( "Q1" = c(cuantiles[2]), "Q2"  = c(cuantiles[3]), "Q3"  = c(cuantiles[4]), "P5"  = c(cuantiles[1]), "P95"  = c(cuantiles[5])  )  )
      
     
     
      
    }
    
    
    
  })
  
  output$table_descriptiva_1 <- renderTable({
    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      return(descriptivos_1()) 
    }
  })
    
  output$table_descriptiva_2 <- renderTable({
    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      return(descriptivos_2()) 
    }
  })
    
  output$table_descriptiva_3 <- renderTable({
    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      return(descriptivos_3()) 
    }
  })
  
  output$table_descriptiva_4 <- renderTable({
    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      return(descriptivos_4()) 
    }
  })
    
    output$table_frecuencias <- renderTable({
      
      if((input$Tablas_box == TRUE) | (input$Todos_box == TRUE)){ 
        
        return(Tablas_frecuencias()) 
      }
    
    })
  
  
  output$table_frecuencias_bidi <- renderTable(rownames = TRUE,{
    
    if((input$Tablas_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){ 
      return(Tablas_frecuencias_bidi())
    }
    return()
  })
  
  output$table_chi_cuadrado <- renderTable({
    
    if((input$Independecia_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){ 
      return(Tabla_chi_cuadrado())
    }
    return()
  })
  
  output$table_fisher <- renderTable({
    
    if((input$Independecia_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){ 
      return(Tabla_Fisher())
    }
    return()
  })
  
  output$table_contingencia <- renderTable({
    
    if((input$Asociacion_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){ 
      return(Tabla_de_contingencia())
    }
    return()
  })
  
  output$table_phi_coef <- renderTable({
    
    if((input$Asociacion_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){ 
      return(Tabla_de_Phi_coef())
    }
    return()
  })
  output$table_cramer <- renderTable({
    
    if((input$Asociacion_box_bidi == TRUE) | (input$Todos_box_bidi == TRUE)){ 
      return(Tabla_Cramer())
    }
    return()
  })
  
  
  

  
  output$table_cuartil <- renderTable({
    
    if((input$Estadisticos_box == TRUE) | (input$Todos_box == TRUE)){ 
      return(cuartil()) 
    }
    
  })
  
  

  
  
  output$table1 <- renderDT({
    
    if(is.null(input$files)){
      validate(need(input$files != "", ""))
      }
    
    validate(need(input$Select_visualizar != "", ""))
    A <- input$Select_visualizar
    B <- paste(ruta,A,sep="")
    
    data <- read.csv(B ,header = TRUE, sep = ",", dec = ".",
                     comment.char = "", strip.white = TRUE,
                     stringsAsFactors = TRUE, encoding = "UTF-8")
    
    data <- lapply(data, iconv, to = "UTF-8")
    
    return (datatable(data, style = "bootstrap",options = list(lengthMenu = c(5,10),
                                                                  columnDefs = list(list(
                                                                    targets = "_all",
                                                                    render = JS(
                                                                      "function(data, type, row, meta) {",
                                                                      "return type === 'display' && data != null && data.length > 30 ?",
                                                                      "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                                      "}")
                                                                    
                                                                  ))),class = "display") )
    
    #return (data)
    
    
  })
  
  output$table5 <- renderTable({
    
    req(input$files)
    
    if(is.null(input$files)){return()}
    
    Fichero_csv <- paste(ruta,input$Select_Arc_3,sep="")
    algo <- read.csv(Fichero_csv)
    
    return (algo)
    
    
  })
  
  
  output$table1 <- renderDT({
    
    if(is.null(input$files)){
      validate(need(input$files != "", ""))
    }
    
    validate(need(input$Select_visualizar != "", ""))
    A <- input$Select_visualizar
    B <- paste(ruta,A,sep="")
    
    data <- read.csv(B ,header = TRUE, sep = ",", dec = ".",
                     comment.char = "", strip.white = TRUE,
                     stringsAsFactors = TRUE, encoding = "UTF-8")

    return (datatable(data, style = "bootstrap",options = list(lengthMenu = c(5,10),
                                                               columnDefs = list(list(
                                                                 targets = "_all",
                                                                 render = JS(
                                                                   "function(data, type, row, meta) {",
                                                                   "return type === 'display' && data != null && data.length > 30 ?",
                                                                   "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                                   "}")
                                                                 
                                                               ))),class = "display") )

    
  })
  
  output$table_elim <- renderDT({  #renderTable
    
    req(input$files)

    if(is.null(input$files)){validate(need(input$files != "", ""))}
    if(is.null(values)){validate(need(values != "", ""))}
    
    return (datatable(values$df, style ="bootstrap",options = list(lengthMenu = c(5,10),
                                                                  columnDefs = list(list(
                                                                    targets = "_all",
                                                                    render = JS(
                                                                      "function(data, type, row, meta) {",
                                                                      "return type === 'display' && data != null && data.length > 30 ?",
                                                                      "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                                      "}")
                                                                    
                                                                  ))),class = "display") )
    
    
    #return (values$df)
    
    
  })
  
  
  output$table_correcion <- renderDT({  #renderTable
    
    req(input$files)
    
    if(is.null(input$files)){validate(need(input$files != "", ""))}
    if(is.null(values)){validate(need(values != "", ""))}
    
    return (datatable(values$df, style ="bootstrap",options = list(lengthMenu = c(5,10),
                                                                   columnDefs = list(list(
                                                                     targets = "_all",
                                                                     render = JS(
                                                                       "function(data, type, row, meta) {",
                                                                       "return type === 'display' && data != null && data.length > 30 ?",
                                                                       "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                                       "}")
                                                                     
                                                                   ))),class = "display") )
    
    
    #return (values$df)
    
    
  })
  
  output$table_categorizacion <- renderDT({  #renderTable
    
    req(input$files)
    
    if(is.null(input$files)){validate(need(input$files != "", ""))}
    if(is.null(values)){validate(need(values != "", ""))}
    
    return (datatable(values$df, style ="bootstrap",options = list(lengthMenu = c(5,10),
                                                                   columnDefs = list(list(
                                                                     targets = "_all",
                                                                     render = JS(
                                                                       "function(data, type, row, meta) {",
                                                                       "return type === 'display' && data != null && data.length > 30 ?",
                                                                       "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                                       "}")
                                                                     
                                                                   ))),class = "display") )
    
    
    #return (values$df)
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

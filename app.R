library(collapse)
library(shiny)
library(bslib)
library(rlang)
library(echarts4r)
library(data.table)
library(dplyr)

options(encoding = "UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

setwd(dir = getwd())

# Cargar base de datos ----------------------------------------------------
rutaData <- "./data/"
file_data <- list.files(rutaData) |> as.vector()

list_data_vis <- list()
for ( i in file_data){
  data <- readr::read_csv(file = paste0(rutaData,i)) |>
    janitor::clean_names() |> tibble::as_tibble()
  list_data_vis[[paste0(i)]] <- data
}


## Data para filtros
data_filtros <- list_data_vis$data2_empresa_producto.csv |> 
  fcount(empresa,producto)

empresa_mayor_menciones <- list_data_vis$data1_empresa.csv |>
  tibble::as_tibble() |>
  fgroup_by(empresa) |>
  fsummarise(emp_menc = fsum(total_menciones,na.rm = T)) |>
  fungroup() |>
  fsubset(emp_menc == fmax(emp_menc,na.rm = T)) |>
  dplyr::distinct(empresa) |>
  dplyr::pull()

vect_empresa <- funique(data_filtros$empresa)
vect_producto <- funique(data_filtros$producto)

fecha_min <- fmin(list_data_vis$data1_empresa.csv$fecha_dia)
fecha_max <- fmax(list_data_vis$data1_empresa.csv$fecha_dia)
fecha_inicio_graf <- fecha_max - months(1)

# ui ----------------------------------------------------------------------

## Tema 
theme <- bs_theme(
  bg = "#14213d", fg = "white",
  "input-border-color" = "white"
)

## UI
ui <- fluidPage(
  
  theme = theme,
  
  tags$h2("Monitorio Análisis de sentimientos - Mercado Financiero"),
  
  tabsetPanel(
    
    tabPanel(
      "Menciones",
      
      ## Filtros por empresa
      fluidRow(
        column(
          3,
          selectInput(
            inputId = "vect_empresa",
            choices = vect_empresa,
            label = "Empresa: ",
            selected = empresa_mayor_menciones
          )
        ),
        column(
          3, 
          selectInput(
            inputId = "vect_producto",
            choices = vect_producto,
            label = "Producto: "
          )
        ),
        column(
          4,
          dateRangeInput(
            "rango_dias",
            "Periodo",
            start = fecha_min,
            end = fecha_max,
            format = "d-M-yy",
            language = "es"
          )
        )
      ),
      fluidRow(
        column(
          6,
          echarts4rOutput("graf_1_rank_empresas", height = "400px", width = "100%")
        ),
        column(
          6,
          echarts4rOutput("graf_3_rank_emp_prod", height = "400px", width = "100%")
        )
      ),
      fluidRow(
        column(
          6,
          echarts4rOutput("graf_2_rank_producto", height = "400px", width = "100%")
        ),
        column(
          3,
          echarts4rOutput("graf_4.1_word_pos")
        ),
        column(
          3,
          echarts4rOutput("graf_4.2_word_neg")
        )
      )
    ),
    tabPanel(
      "Monitoreo",
      tags$h2("En construcción..."),
    ),
    tabPanel(
      "Correlaciones",
      tags$h2("En construcción...")
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session){
  
  ## Sección de mennciones
  
  ### Filtros 
  observe({
    updateSelectInput(
      session,
      "vect_empresa",
      choices = vect_empresa,
      selected = isolate(input$vect_empresa)
    )
  }) ### Empresas
  
  observe({
    updateSelectInput(
      session,
      "vect_producto",
      choices = data_filtros |> 
        fsubset(empresa == input$vect_empresa) |> 
        fselect(producto) %>%
        .[[1]] %>% 
        sort(.)
    )
  }) ### Productos
  
  ### Gráfico de ranking por empresa
  output$graf_1_rank_empresas <- renderEcharts4r({
    
    data_rank <- list_data_vis$data1_empresa.csv |> 
      fgroup_by(empresa) |> 
      fsummarise(total_menciones = fsum(total_menciones,na.rm = T)) |> 
      fungroup() |> 
      roworder(total_menciones) %>%
      fmutate( color = c(rep("gray",nrow(.)-1),"#d80032"))
    
    data_rank |> 
      e_chart(empresa) |> 
      e_bar(total_menciones) |> 
      e_flip_coords() |> 
      e_add("itemStyle", color) |> 
      e_legend(show = FALSE) |> 
      e_title(
        text = "Total de menciones acumuladas",
        subtext = paste0(format(fecha_min,"%b")," - ",format(fecha_max,"%b %y")),
        
        textStyle = list(
          color    = "white",  
          fontSize = 20),
        subtextStyle = list(
          color    = "white",  # color azul
          fontSize = 14
        )
      ) |> 
    e_x_axis(
      axisLabel = list(color = "white"),
      axisLine  = list(lineStyle = list(color = "white"))
    ) %>%
      # Eje Y: etiquetas y línea de color azul
      e_y_axis(
        axisLabel = list(color = "white"),
        axisLine  = list(lineStyle = list(color = "white"))
      )
    
  })
  
  ### Gráfico de ranking de cada producto por empresa
  output$graf_2_rank_producto <- renderEcharts4r({
    
    data_producto <- list_data_vis$data2_empresa_producto.csv |> 
      fgroup_by(empresa,producto) |> 
      fsummarise(
        menciones_positivas = fsum(menciones_positivas,na.rm = T),
        menciones_negativas = fsum(menciones_negativas,na.rm = T),
        menciones_neutras = fsum(menciones_neutras,na.rm = T)
      ) |> 
      fungroup() |> 
      fmutate(total = menciones_positivas+menciones_negativas+menciones_neutras)
    
    data_producto |> 
      fsubset(empresa == input$vect_empresa) |> 
      roworder(total) |> 
      fmutate(producto = stringr::str_wrap(producto,width = 30)) |> 
      e_chart(producto,stack = 'group') |> 
      e_bar(menciones_positivas,color = "#008000") |> 
      e_bar(menciones_negativas, color = "#d80032") |> 
      e_bar(menciones_neutras, color = "gray") |> 
      e_flip_coords() |> 
      e_legend(orient = "horizontal", 
               left = "center", 
               top = "bottom",
               textStyle = list(color = "white")) |> 
      e_title(text = "Total de menciones acumuladas por producto",
              subtext = paste0(format(fecha_min,"%b")," - ",format(fecha_max,"%b %y")),
              textStyle = list(
                color    = "white",  
                fontSize = 20),
              subtextStyle = list(
                color    = "white", 
                fontSize = 14
              )
              ) |> 
      e_x_axis(
        axisLabel = list(color = "white"),
        axisLine  = list(lineStyle = list(color = "white"))
      ) %>%
     
      e_y_axis(
        axisLabel = list(color = "white"),
        axisLine  = list(lineStyle = list(color = "white"))
      )
    
    
  })
  
  ### Gráfico de evolución de sentimientos por empresa
  output$graf_3_rank_emp_prod <- renderEcharts4r({
    
    data_emp_prod <- list_data_vis$data2_empresa_producto.csv |> 
      fsubset(empresa == input$vect_empresa) |> 
      fsubset(producto == input$vect_producto)
    
    data_emp_prod |> 
      e_chart(fecha_dia) |> 
      e_area(menciones_positivas,color = "#008000") |> 
      e_area(menciones_negativas, color = "#d80032") |> 
      # e_area(menciones_neutras, color = "gray") |> 
      e_legend(orient = "horizontal", 
               left = "center", 
               top = "bottom",
               textStyle = list(color = "white")) |> 
      e_title(text = paste0("Evolución de los sentimientos por producto: ",funique(data_emp_prod$producto)),
              subtext = paste0(format(fecha_min,"%b")," - ",format(fecha_max,"%b %y")),
              textStyle = list(
                color    = "white",  
                fontSize = 20),
              subtextStyle = list(
                color    = "white",  # color azul
                fontSize = 14
              )
              ) |> 
      e_x_axis(
        axisLabel = list(color = "white"),
        axisLine  = list(lineStyle = list(color = "white"))
      ) %>%
      
      e_y_axis(
        axisLabel = list(color = "white"),
        axisLine  = list(lineStyle = list(color = "white"))
      )
  })
  
  ### Gráficos de nubes de palabras por sentimiento
  
  #### Positivo
  output$graf_4.1_word_pos <- renderEcharts4r({
    
    data_conteo_prod <- list_data_vis$word_pos.csv |> 
      fsubset(empresa == input$vect_empresa) |> 
      fsubset(producto == input$vect_producto) |> 
      fgroup_by(producto,palabra) |> 
      fsummarise(conteo = fsum(conteo,na.rm = T)) |> 
      fungroup()
    
    data_conteo_prod |> 
      # e_color_range(conteo,palabra) |> 
      e_chart() |> 
      e_cloud(palabra, conteo, shape = "diamond") |> 
      e_color("#008000") |> 
      e_title(
        text = "Sentimientos positivos más mencionados",
        subtext = funique(data_conteo_prod$producto),
        textStyle = list(
          color    = "white",  
          fontSize = 20),
        subtextStyle = list(
          color    = "white",  # color azul
          fontSize = 14
        )
      )
  })
  
  #### Negativo
  output$graf_4.2_word_neg <- renderEcharts4r({
    
    data_conteo_neg <- list_data_vis$word_neg.csv |> 
      fsubset(empresa == input$vect_empresa) |> 
      fsubset(producto == input$vect_producto) |> 
      fgroup_by(producto,palabra) |> 
      fsummarise(conteo = fsum(conteo,na.rm = T)) |> 
      fungroup()
    
    data_conteo_neg |> 
      # e_color_range(conteo,palabra) |> 
      e_chart() |> 
      e_cloud(palabra, conteo, shape = "diamond") |> 
      e_color("#d80032") |> 
      e_title(
        text = "Sentimientos negativos más mencionados",
        subtext = funique(data_conteo_neg$producto),
        textStyle = list(
          color    = "white",  
          fontSize = 20),
        subtextStyle = list(
          color    = "white",  # color azul
          fontSize = 14
        )
      )
  })

}

shinyApp(ui,server)

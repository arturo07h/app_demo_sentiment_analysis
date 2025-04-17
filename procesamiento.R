library(collapse)
library(data.table)

options(encoding = "UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")


# rutas -------------------------------------------------------------------

ruta_raw <- "./data-raw/"
files <- list.files(ruta_raw)


# Cargar data -------------------------------------------------------------

list_data_raw <- list()
for ( i in files){
  data <- readr::read_csv(file = paste0(ruta_raw,i)) |> 
    janitor::clean_names() |> tibble::as_tibble()
  
  list_data_raw[[paste0(i)]] <- data
}

# Creación de tablas para visualizaciones ---------------------------------

## Data menciones 
data_menciones <- list_data_raw$table2.csv |> 
  fmutate(
    total_menciones = menciones_positivas+menciones_neutras+menciones_positivas
  ) |> 
  fmutate(
    fecha_dia = as.Date(fecha)
  )

## Data palabras más mencionadas
data_palabras <- list_data_raw$table3_wordcounts_time.csv 

## Cuenta de menciones por empresa 
data1_empresa <- data_menciones |> 
  fgroup_by(fecha_dia,empresa) |> 
  fsummarise(
    total_menciones = fsum(total_menciones,na.rm = T)
  ) |> 
  fungroup()

## Cuenta de menciones producto - sentimiento
data2_empresa_producto <- data_menciones |> 
  fgroup_by(fecha_dia,empresa, producto) |> 
  fsummarise(
    menciones_positivas = fsum(menciones_positivas,na.rm = T),
    menciones_negativas = fsum(menciones_negativas,na.rm = T),
    menciones_neutras = fsum(menciones_neutras,na.rm = T)
  )

## Evolución diaria de menciones de sentimientos
data3_evo_menciones <- data_menciones |> 
  fgroup_by(
    fecha_dia,
    empresa
  ) |> 
  fsummarise(
    menciones_positivas = fsum(menciones_positivas,na.rm = T),
    menciones_negativas = fsum(menciones_negativas,na.rm = T),
    menciones_neutras = fsum(menciones_neutras,na.rm = T)
  )
  

## Lisdta de menciones por sentimiento
data1_sentimiento <- split(data_palabras,data_palabras$sentimiento)

## Lista objetos visualizaciones
list_vis <- list(
  data1_empresa = data1_empresa,
  data2_empresa_producto = data2_empresa_producto,
  data3_evo_menciones = data3_evo_menciones,
  word_pos = data1_sentimiento$positivas,
  word_neg = data1_sentimiento$negativas
)

## Exportar archivo csv

for ( i in seq_along(list_vis)){
  write.csv(x = list_vis[[i]],file = paste0("./data/",names(list_vis[i]),".csv"))
}

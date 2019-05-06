## Paso 1: Configuramos el entorno de trabajo

Sys.setlocale(category = "LC_ALL", locale = "es_ES")
install.packages("rtweet")
library(dplyr)
library(rtweet)
library(tidyverse)
library(lubridate)
library(htttuv)

## Paso 2: Cargamos las credenciales de Twitter

appname <- "QQQQ"
key <- "WWWW"
secret <- "EEEE"
accesstoken <- "RRRR"
accesssecret <- "TTTT"

## Paso 3: Vamos a capturar tuits

# pedimos 15 mil tuits, que no incluyan retuits
GOTstarbucks <- search_tweets("got starbucks", n = 15000, include_rts = FALSE)

# capturamos las tendencias que están sucediendo en este momento

trendsAR <- get_trends("argentina")

### En este link un diccionario con las columnas que me devuelve una búsqueda http://bit.ly/columnasrtweet


## Paso 4: Armamos lista UBA - El error que surgió aquí es porque a la mayoría les habilitaron permisos "Read only" de sólo lectura :(

facusuba <- c("DerechoUBA","FFyB_UBA","ubasociales","Exactas_UBA","FaduComunica","FAUBA_oficial","filo_uba","UBAeconomicas","IngenieriaUBA","UBAPsicologia")
FacultadesUBA <- post_list(facusuba,
                           "facusuba", description = "Facultades Universidad de Buenos Aires, creado via rtweet")

# Aquí hacemos una consulta sobre una lista que ya existe, en este caso una mía
FacultadesUBA <- lists_members(slug = "facusuba", owner_user = "juanibelbis")

## Paso 5: Captura listas y captura de tuits de UBA

twtFacusUBA <- vector("list", nrow(FacultadesUBA))
for (i in seq_along(twtFacusUBA)) {
  if ((i %% 56) == 0) {
    message("Sleeping for 15 minutes...")
    Sys.sleep(60*15)
  }
  twtFacusUBA[[i]] <- get_timeline(FacultadesUBA$user_id[i], n=3200)
  message(i, "/", nrow(FacultadesUBA))
}
twtFacusUBA <- do.call(rbind, twtFacusUBA)

## Paso 6: Acomodamos la zona horaria de los tuits a GMT-3, operacionalizamos las fechas y filtramos

# definimos nuestra zona horaria
tz_local <- "America/Argentina/Buenos_Aires"

# definimos el contenido de la columna "created_at" con formato temoporal
twtFacusUBA$created_at <-ymd_hms(twtFacusUBA$created_at)

# convertimos las fechas a nuestra zona horaria
with_tz(twtFacusUBA$created_at, tz_local)

# armamos un nuevo dataframe con menos columnas y un filtro temporal (el mes de abril)
twtFacusUBACORE <- twtFacusUBA %>%
  select(status_id,created_at,screen_name) %>%
  filter(created_at >= as.Date("2019-04-01") & created_at <= as.Date("2019-04-30"))

## Paso 7: contemos tuits por cuenta en los dos frames, el global y el de abril

TuitsPorFacultad <-
  twtFacusUBA %>%
  count(screen_name, sort = TRUE)

TuitsPorFacultadCORE <-
  twtFacusUBACORE %>%
  count(screen_name, sort = TRUE)


## Paso 8: Armamos una línea de tiempo con la frecuencia de los tuits

ts_plot(twtFacusUBACORE, "1 hour") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Gráfico 1 - Línea de tiempo - Tuits Facultades UBA",
    subtitle = "Cantidad de publicaciones (tuits) en intervalos de 1 hora",
    caption = "\nSource: Datos recolectados de la API REST de Twitter via rtweet" )


## Paso 10: Capturamos tuits sobre Mercado Libre y Juan Grabois

# pedimos 150 mil tuits de cada búsqueda, que incluyan retuits y que espere y siga intentando cuando llega al límite de 18 mil cada 15 minutos

MeLi <- search_tweets("mercado libre", n = 150000, include_rts = TRUE, retryonratelimit = TRUE )
Grabois <- search_tweets("grabois", n = 150000, include_rts = TRUE, retryonratelimit = TRUE)

# unimos los dos dataframes

tuitsMeliJG<- rbind(Grabois,MeLi)

# eliminamos los duplicados

tuitsMeliJG_CL<- tuitsMeliJG %>% distinct(status_id, .keep_all = TRUE) 

# filtramos los que fueron emitidos entre el cuatro y el siete de mayo

tuitsMeliJG_CL <- tuitsMeliJG_CL %>%
  select(status_id,created_at,screen_name,source) %>%
  filter(created_at >= as.Date("2019-05-04") & created_at <= as.Date("2019-05-07"))


## Paso 11: Armemos una línea de tiempo con la frecuencia de los tuits

ts_plot(tuitsMeliJG_CL, "15 minutes") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Gráfico 2 - Línea de tiempo - Tuits Conversación Mercado Libre / Juan Grabois",
    subtitle = "Cantidad de publicaciones (tuits) en intervalos de 15 minutos",
    caption = "\nSource: Datos recolectados de la API REST de Twitter via rtweet" )


## Paso 12: Fuentes de publicación

tuitsMeliJG_CL %>% group_by(source)%>% 
  summarise(Total=n()) %>% arrange(desc(Total)) %>% head(10) %>%
  ggplot(aes(reorder(source, Total), Total, fill = source)) + geom_bar(stat="identity") + coord_flip() + 
  labs(title="Gráfico 3 - Fuentes de publicación - Discusión MeLi/Grabois", x="", 
       subtitle="Tuits agrupados por fuente de publicación", 
       caption = "\nSource: Datos recolectados de la API REST de Twitter via rtweet")

## Paso 13: Formas de hacer backup

save.image("backup.RData")

# con write_as_csv podemos guardar como archivos .csv cada uno de los dataframes

write_as_csv(trendsAR,"trendsAR.csv")

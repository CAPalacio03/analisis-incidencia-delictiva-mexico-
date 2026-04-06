
library(DT)         # xie2020
library(knitr)      # xie2014
library(reshape2)   # wickham2007
library(ggplot2)    # wickham2016
library(dplyr)      # wickham2020b
library(scales)     # wickham2020
library(kableExtra) # zhu2021
library(Hmisc)      # harrell2020

# EXPORTACION DE DATOS
datos <- 
  read.table(
    file = 'IDEFC_NM_may22.csv'
    , header = TRUE
    , sep = ','
    , quote = "\"" 
    ,fileEncoding = "latin1")

datatable(data = head(datos), rownames = FALSE)

# exploraciones de los datos es el paquete *Hmisc*:
Hmisc::describe(x = datos)


#PREPARACION DE LOS DATOS
datos.2 <- 
  melt(
    data = datos, 
    id.vars = 1:(ncol(datos)-12), 
    measure.vars = (ncol(datos)-11):ncol(datos), 
    variable.name = 'MES',
    value.name = 'DELITOS')

str(datos.2)

datatable(data = head(datos.2), rownames = FALSE)


# Convierto el mes a caracter porque se encontraba en formato de factor.

datos.2$MES <- as.character(datos.2$MES)

# Elimino los registros correspondientes al total del año.
datos.2 <- datos.2[datos.2$MES != 'Total',]

# Los siguientes "catálogos" me permitirán crear la columna de fechas.
mes <- 
  c(
    'Enero',
    'Febrero',
    'Marzo',
    'Abril',
    'Mayo',
    'Junio',
    'Julio',
    'Agosto',
    'Septiembre',
    'Octubre',
    'Noviembre',
    'Diciembre')

mes.indice <- c(1:12)

mes.dia <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# Busca el mes en el catálogo de meses y le asigna el número de mes que le
#  corresponde.
# 
datos.2$MES.2 <- 
  sapply(datos.2$MES, FUN = function(x){mes.indice[mes == x]})

# Le asigna el día correspondiente al último día del mes.
# 
datos.2$MES.DIA <- sapply(datos.2$MES.2, function(x){mes.dia[x]})

# Crea la fecha (como tipo de dato fecha).
# 
datos.2$FECHA <- 
  as.Date(
    x = paste(datos.2$Año, datos.2$MES.2, datos.2$MES.DIA, sep = '-'),
    format = '%Y-%m-%d')

# observamos como quedaron los datos
Hmisc::describe(x = datos.2)


# DATOS FALTANTES
# En nuestro caso no hay datos faltantes

# Nos concentramos en los tipos y sub-tipos.

datos.3 <- datos.2[which(datos.2$Tipo.de.delito == 'Homicidio'),]

head(datos.3)

# Agrupamos para no tener duplicados
datos.3 <- 
  aggregate(
    x = list(DELITOS = datos.3$DELITOS)
    , by = 
      list(
        FECHA = datos.3$FECHA
        , Entidad = datos.3$Entidad
        , Subtipo.de.delito = datos.3$Subtipo.de.delito
      )
    , FUN = function(x) sum(x, na.rm = TRUE)
  )

# agregamaos la población estatal de cada año


print(readLines(con = 'pob_mit_proyecciones.csv', n = 10))

poblacion <- 
  read.table(file = 'pob_mit_proyecciones.csv', 
             sep = ','
             , header = TRUE
             , fileEncoding = "latin1")

# Lo agregamos para obtener la población total.
# 
poblacion <- 
  aggregate(
    x = list(POBLACION = poblacion$POBLACION)
    , by = list(AÑO = poblacion$AÑO, ENTIDAD = poblacion$ENTIDAD)
    , FUN = sum
  )

# Agregamos la poblacion a nuestros datos.
# 
datos.3$AÑO <- substr(datos.3$FECHA, start = 1, stop = 4)

# Limpiamos los nombres de algunos estados ...
# 
datos.3$Entidad[which(datos.3$Entidad == 'Coahuila de Zaragoza')] <- 'Coahuila'
datos.3$Entidad[which(datos.3$Entidad == 'Michoacán de Ocampo')] <- 'Michoacán'
datos.3$Entidad[which(datos.3$Entidad == 'Veracruz de Ignacio de la Llave')] <- 
  'Veracruz'

# Agregamos la población haciendo un join por la derecha 
datos.3 <- 
  merge(
    x = datos.3
    , y = poblacion
    , by.x = c('AÑO', 'Entidad')
    , by.y = c('AÑO', 'ENTIDAD')
    , all.x = TRUE
  )

summary(datos.3)

# Calculamos los delitos per capita ...
# 
datos.3$DELITOS.PC <- datos.3$DELITOS / datos.3$POBLACION

# Multiplicamos por 100,000
# 
datos.3$DELITOS.PC <- datos.3$DELITOS.PC*100000

# ESTADISTICAS DE RESUMEN

# asociamis los datos de delitos sobre Entidad Federativa 
estados <- 
  aggregate(
    x = list(Delitos = datos.3$DELITOS)
    , by = list(Entidad = datos.3$Entidad)
    , FUN = sum
  )

estados$Participación <- estados$Delitos / sum(estados$Delitos)

estados <- estados[order(estados$Participación, decreasing = TRUE),]

kable(
  x = 
    estados %>% 
    mutate(Delitos = comma(Delitos, accuracy = 1)) %>% 
    mutate(Participación = percent(Participación, accuracy = 0.01))
  , row.names = FALSE
  , format = 'pandoc'
  , align = c('l', 'r' ,'r')
)

#Vla categoría más frecuentemente observada:
estados$Entidad

# Guardar datasets limpios
saveRDS(datos, "datos_crudos_incidencia.csv")
saveRDS(datos.2, "datos_limpios_incidencia.csv")
saveRDS(datos.3, "datos_homicidio_limpios.csv")

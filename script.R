"-----------------------------------------------------------------
------------------------------------------------------------------
                      Laboratorio - R
                Alveiro Garcia N, Viviana Cardozo E
            Universidad de los Andes, Bogotá, Colombia
            {a.garcian2 ; vy.cardozo}@uniandes.edu.co
             Fecha de presentación: noviembre de 2021
------------------------------------------------------------------
------------------------------------------------------------------"


"/*=================================================*/
 /*===================LIBRERIAS=====================*/
/*=================================================*/" 
library(tidyverse)
library(ggplot2)

"/*=================================================*/
 /*==============MACROVARIABLES=====================*/
/*=================================================*/" 

#Ruta del archivo
rFile <- 'C:\\Andes\\ingsegpriv\\ranalysis\\datos.txt';

"/*=================================================*/
 /*==================FUNCIONES=====================*/
/*================================================*/" 

"*****************************************************************
 ******************* 1.Lectura de Datos        *******************
 *****************************************************************"

tdatos <- read.table(rFile, header=TRUE)

"*****************************************************************
 ***** 2.Calcular el valor máximo de una columna     *************
 *****************************************************************"

max_column <- function(v_column,v_data) {
  v_max_column <- format(round(max(tdatos[,4:4]), 3), nsmall = 2)
  return(paste("1.Valor máximo de una columna:",v_max_column))
}

"*****************************************************************
 ***** 3.Calcular el valor mínimo de una columna     *************
 *****************************************************************"

min_column <- function(v_column,v_data) {
  v_min_column <- format(round(min(tdatos[,v_column:v_column]), 3), nsmall = 2)
  return(paste("2.Valor mínimo de una columna:",v_min_column))
}

"*****************************************************************
 ***** 4.Calcular el valor promedio de una columna ***************
 *****************************************************************"

min_prom <- function(v_column,v_data) {
  v_prom_column <- format(round(mean(tdatos[,v_column:v_column]), 3), nsmall = 2)
  return(paste("3.Valor promedio de una columna:",v_prom_column))
}


"**********************************************************************************
 ** 5. Buscar el índice de la columna con el valor promedio más alto **************
 **********************************************************************************"

min_icolumn <- function(v_data,i_column,f_column) {
  tdata=v_data[,i_column:f_column]
  nCol=ncol(tdata)
  dDatosAcum <- data.frame(column = character(nCol), indcolumn = numeric(nCol),prom= numeric(nCol), stringsAsFactors = FALSE)
  i=1    
  for(j in i_column:f_column){
    dDatosAcum$column[i] <- names(tdatos[j])
    dDatosAcum$indcolumn[i] <- j
    dDatosAcum$prom[i]  <- as.numeric(format(round(mean(tdatos[,j:j]), 3), nsmall = 2))
    i=i+1
  }
  r_column<-dDatosAcum$column[dDatosAcum$prom ==max(dDatosAcum$prom)]
  r_ind<-dDatosAcum$indcolumn[dDatosAcum$prom ==max(dDatosAcum$prom)]
  return(paste("4.Índice de la columna con el valor promedio más alto:","(Columna)",r_column,",(Índice)",r_ind))
}

"**********************************************************************************
 ** 6. Buscar el índice de la fila con el valor más alto en un campo particular ***
 **********************************************************************************"
max_id_column <- function(v_column,v_data) {
  v_max_column <- as.numeric(format(round(max(tdatos[,v_column:v_column]), 3), nsmall = 2))
  r_id<-list(v_data$id[v_data[v_column]==v_max_column])
  return(paste("5.Índices de las filas con el valor más alto en un campo:",r_id))
}

"**********************************************************************************
 ** 7. Histograma que agrupación de los valores para una de las actividades *******
 **********************************************************************************"
data=data.frame(value=tdatos[,4:4])

p <- ggplot(data, aes(x=value)) + 
  geom_histogram()
p

df<-tdatos %>% 
    #Creo una variable explícita ad hoc con los grupos. 
    # No es elegante, pero sirve para validar el paso intermedio.
    mutate(intervalo = case_when(tarea3 > 4.5 ~ "mayor45",
                                 between(tarea3, 3.8, 4.5) ~ "38-45", 
                                 tarea3 < 3.8 ~ "menor38")) %>% 
    #Agrupo por las categorías de intervalo
    group_by(intervalo) %>% 
    #Sumo Resultado dentro de cada grupo
    summarise(n = sum(n))


"**********************************************************************************
 ** 8. Promedio y la desviación estándar para las actividades *********************
 **********************************************************************************"



"/*=================================================*/
 /*==================EJECUCION=====================*/
/*================================================*/" 
#Indice de la columna a evaluar
x_col<-4
#Indice incial a evaluar
i_col<-2
#Indice final a evaluar
f_col<-10

#-------Resultado Función (1)-------#
max_column(x_col,tdatos)

#-------Resultado Función (2)-------#
min_column(x_col,tdatos)

#-------Resultado Función (3)-------#
min_prom(x_col,tdatos)

#-------Resultado Función (4)-------#
min_icolumn(tdatos,i_col,f_col)

#-------Resultado Función (4)-------#
max_id_column(x_col,tdatos)



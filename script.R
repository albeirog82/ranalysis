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
library(highcharter)

"/*=================================================*/
 /*==============MACROVARIABLES=====================*/
/*=================================================*/" 

#Ruta del archivo
#rFile <- './Documents/Maestria/Semestre4/Seguridad/LabR/datos.txt';
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

hist_graph <- function(field){
  hc <- hchart(
    field, 
    color = "#978D8B", name = "Histograma"
  )
  return(hc)
}

"**********************************************************************************
 ** 8. Promedio y la desviación estándar para las actividades *********************
 **********************************************************************************"
promedio_desviacion_graph <- function(v_data,i_column,f_column) {
  tdata=v_data[,i_column:f_column]
  nCol=ncol(tdata)
  dDatosAcum <- data.frame(column = character(nCol), indcolumn = numeric(nCol),prom= numeric(nCol), stringsAsFactors = FALSE)
  i=1    
  for(j in i_column:f_column){
    dDatosAcum$column[i] <- names(tdatos[j])
    dDatosAcum$prom[i]  <- as.numeric(format(round(mean(tdatos[,j:j]), 3), nsmall = 2))
    dDatosAcum$desv[i] <- as.numeric(format(round(sd(tdatos[,j:j]), 2), nsmall = 2))
    i=i+1
  }
  
  hcg<-highchart() %>%
       hc_xAxis(type =  "category")%>% 
       hc_add_series(data = dDatosAcum, 
                    type = "column",
                    mapping= hcaes(x=column, y=prom),
                    color= '#4990E2')%>%
       hc_add_series(data = dDatosAcum, 
                    type = "line",
                    mapping= hcaes(x=column, y=desv),
                    color= '#FF5733')%>%
       hc_legend(enabled = FALSE)%>%
       hc_plotOptions(
         series = list(
           boderWidth = 0,
           dataLabels = list(enabled = TRUE))) %>%
       hc_tooltip(style=list(color='blue',
                            fontWeight='bold',
                            fontSize='18px'),
                 headerFormat='<span style="font-size: 18px;">{point.key}</span><br/>')
    
  return(hcg)  
}


"/*=================================================*/
 /*==================EJECUCION=====================*/
/*================================================*/" 
#Indice de la columna a evaluar
x_col<-4
#Indice incial a evaluar 
i_col<-2
#Indice final a evaluar
f_col<-10

#====================================#
#-------Función valor máximo --------#
#====================================#
"Esta función recibe dos parámetros:
1. La posición o índice de la columna que se quiere evaluar, por ejemplo, 
   si se quiere evaluar la columna con nombre tarea5 el índice corresponde a 6
2. Es el nombre de tabla que contiene los datos"

max_column(x_col,tdatos)


#====================================#
#-------Función valor mínimo --------#
#====================================#
"Esta función recibe dos parámetros:
1. La posición o índice de la columna que se quiere evaluar, por ejemplo, 
   si se quiere evaluar la columna con nombre tarea5 el índice corresponde a 6
2. Es el nombre de tabla que contiene los datos"

min_column(x_col,tdatos)


#====================================#
#-----Función valor promedio --------#
#====================================#
"Esta función recibe dos parámetros:
1. La posición o índice de la columna que se quiere evaluar, por ejemplo, 
   si se quiere evaluar la columna con nombre tarea5 el índice corresponde a 6
2. Es el nombre de tabla que contiene los datos"

min_prom(x_col,tdatos)

#=========================================================#
#--- Función índice de la columna promedio más alto ------#
#=========================================================#
"Esta función recibe tres parámetros:
 Si por ejemplo las columnas que se quieren evaluar corresponden a Tarea1, Tarea2,Tarea3 y Tarea4, 
 los parámetros que se reciben son:
1. Es el nombre de tabla que contiene los datos
2. Índice de la columna en la mínima posición de izquierda a derecha,siguiendo el ejemplo, Tarea1 es igual a 2
3. Índice de la columna en la primera posición de derecha a izquierda,siguiendo el ejemplo, Tarea4 es igual a 5
"

min_icolumn(tdatos,i_col,f_col)

#=========================================================#
#--- Índice de la fila con el valor más alto -------------#
#=========================================================#
"Esta función recibe dos parámetros:
1. La posición o índice de la columna que se quiere evaluar, por ejemplo, 
   si se quiere evaluar la columna con nombre tarea5 el índice corresponde a 6
2. Es el nombre de tabla que contiene los datos"

max_id_column(x_col,tdatos)


#=========================================================#
#--------------- Histograma por actividad ----------------#
#=========================================================#
"Esta función recibe un parámetro:
1. Concatenar el nombre de la tabla que contiene los datos y el nombre del campo que se quiere evaluar, 
   se debe utilizar el símbolo $ para separar: Ejemplo: tdatos$tarea7"

hist_graph(tdatos$tarea1)


#=========================================================#
#------------ Promedio y desviación estándar  ------------#
#=========================================================#
"Esta función recibe tres parámetros:
 Si por ejemplo las columnas que se quieren evaluar corresponden a Tarea1, Tarea2,Tarea3 y Tarea4, 
 los parámetros que se reciben son:
1. Es el nombre de tabla que contiene los datos
2. Índice de la columna en la mínima posición de izquierda a derecha,siguiendo el ejemplo, Tarea1 es igual a 2
3. Índice de la columna en la primera posición de derecha a izquierda,siguiendo el ejemplo, Tarea4 es igual a 5
"

promedio_desviacion_graph(tdatos,i_col,f_col)
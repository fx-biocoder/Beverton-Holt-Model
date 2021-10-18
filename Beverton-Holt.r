# 1 - Librerías necesarias
install.packages("ggplot2")
library(ggplot2)

# 2 - Definición de la función
#     R  = índice de proliferación
#     K  = capacidad de carga del entorno
#     N0 = población en el tiempo inicial
#     Tf = tiempo final (puede ser cualquier valor entero)
BevertonHolt = function(R, K, N0, Tf){
  poblacion = numeric()

  for(t in 0:Tf){
    Nt = (K * N0)/(N0 + (K - N0)* exp(-R*t)) #  Para cada valor de tiempo, se calcula un tamaño poblacional
    poblacion <- c(poblacion, Nt) 
  }
  
  return(poblacion)
}

# 3 - Prueba de la función con R = 0.5, K = 500, población inicial de 2 individuos
evolucion_temporal <- BevertonHolt(0.5, 500, 2, 100)
tiempo = 0:100

# 4 - Creación de un dataframe para almacenar los datos
grafico <- data.frame(evolucion_temporal, tiempo)

# 5- Gráfico del comportamiento de la población
comportamiento <- ggplot(grafico, 
                         aes(x = tiempo, 
                             y = evolucion_temporal)) + 
                  geom_point() + 
                  labs(title = "Comportamiento temporal de la población", 
                           x = "Tiempo", 
                           y = "Tamaño de la población")

# 6 - En caso de que K sea aleatoria y siga una distribución normal con media y desvío conocidos, puede adaptarse así:
BevertonHolt = function(R, N0, Tf, k_mean, k_sdev){
  poblacion = numeric()

  for(i in 0:Tf){
    # Uso de la función rnorm() para generar valores aleatorios normales de K
    K <- rnorm(1, k_mean, k_sdev) # Donde "k_mean" es el valor medio de K, y "k_sdev" el desvío estándar
    Nt = (K * N0)/(N0 + (K - N0)* exp(-R*i)) 
    poblacion <- c(poblacion, Nt) 
    }

  return(poblacion)
}

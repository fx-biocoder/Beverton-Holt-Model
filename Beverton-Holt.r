# Librerías necesarias
install.packages("ggplot2")
library(ggplot2)

# Definición de la función
BevertonHolt = function(R, K, N0, Tf){
  poblacion = numeric()
  for(t in 0:Tf){
    Nt = (K * N0)/(N0 + (K - N0)* exp(-R*t)) 
    poblacion <- c(poblacion, Nt) 
  }
  
  return(poblacion)
}

# Prueba de la función con R = 0.5, K = 500, población inicial de 2 individuos
evolucion <- BevertonHolt(0.5, 500, 2, 100)
tiempo = 0:100

# Creación de un dataframe
grafico <- data.frame(evolucion, tiempo)

# Gráfico del comportamiento de la población
comportamiento <- ggplot(grafico, 
                         aes(x = tiempo, 
                             y = evolucion)) + 
                  geom_point() + 
                  labs(title = "Comportamiento temporal de la población", 
                           x = "Tiempo", 
                           y = "Tamaño de la población")




































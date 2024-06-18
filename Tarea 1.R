##Tarea 2 

Votes <- readRDS("C:/Users/Usuario/Documents/Python1/votes.rds")
descriptions <- readRDS("C:/Users/Usuario/Documents/Python1/descriptions.rds")

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)

class(Votes)
summarise(Votes)
Summary(Votes)
attach(Votes)

Votos<- Votes

#1 Filtrar los datos por los votos por Si, Se abstiene y No

Votos1<-Votos%>%
  filter(vote%in% c(1,2,3))

print(Votos1)

#2 Agregue una columna llamada “year”. La primera sesión se llevó a cabo en el 1946, es decir, este año sería la “session” número 1.

Votos1<-Votos1%>%
  mutate(year=1946+session-1)

#3 Agregue una columna llamada “country”. Para ello, utilice el paquete “countrycode” y dentro de él la función “countrycode” con los parámetros: variable a recodificar, “cown”,”country.name”.

install.packages("countrycode")
library(countrycode)

Votos1<-Votos1%>%
  mutate(country=countrycode(ccode, "cown", "country.name"))

#4 Recodifique la variable país recién creada de forma que “United States of America” sea “United States” y “United Kingdom of Great Britain and Northern Ireland” sea “United Kingdom”

Votos1<- Votos1%>%
  mutate(country=recode(country,
                        'United States of America' = 'United States',
                        'United Kingdom of Great Britain and Northern Ireland' = 'United Kingdom'))

#5 Encuentre el total y porcentaje del total para los que votaron “yes”. Use la función summarise.

Votos1%>%
  group_by(vote)%>%
  summarise(
    Total=n(),
    Porcentaje=(Total/nrow(Votos1))*100)%>%
  filter(vote==1)

# 6 Haga lo mismo del punto 5 sólo que agrupando por año.

Porcentaje_Anno<-Votos1%>%
  filter(vote==1)%>%
  group_by(year)%>%
  summarise(
    Total=n(),
    Porcentaje=(Total/nrow(Votos1))*100)

#7 Haga lo mismo del punto 5 sólo que agrupando por país.
Porcentaje_Pais<- Votos1%>%
  filter(vote==1)%>%
  group_by(country)%>%
  summarise(
    Total=n(),
    Porcentaje=(Total/nrow(Votos1))*100)

#8 Ordene el resultado del punto 7 de mayor a menor según el porcentaje de los que dijeron “yes”.
Orden_Pais<- Votos1%>%
  filter(vote==1)%>%
  group_by(country)%>%
  summarise(
    Total=n(),
    Porcentaje=(Total/nrow(Votos1))*100)%>%
  arrange(desc(Porcentaje))

#9 Utilice el resultado del punto 6 y construya un gráfico de línea con ggplot2. Analice el resultado.
ggplot(Porcentaje_Anno, aes(x = year, y = Porcentaje, group = 1)) +
  geom_line(color = "green") +
  geom_point(color = "blue") + 
  labs(title = "Porcentaje de votos ' Sí ' por año",
       x = "Año",
       y = "Porcentaje") +
  scale_x_continuous(breaks = seq(min(Porcentaje_Anno$year), max(Porcentaje_Anno$year), by = 10)) +
  theme_minimal()


#10 Haga el punto 9 pero que sea posible comparar entre países en un mismo gráfico.

Porcentaje_Anno_Pais<- Votos1%>%
  filter(vote==1)%>%
  group_by(year,country)%>%
  summarise(
    Total=n(),
    Porcentaje=(Total/nrow(Votos1))*100)


ggplot(Porcentaje_Anno_Pais, aes(x = year, y = Porcentaje, color = country)) +
  geom_line(color = "green") +
  geom_point(color = "blue") +
  labs(title = "Porcentaje de votos 'Sí' por año y país",
       x = "Año",
       y = "Porcentaje",
       color = "País") +
  theme_minimal()

#11 Considere usar facet para representar el punto anterior. Compare los resultados.

ggplot(Porcentaje_Anno_Pais, aes(x = year, y = Porcentaje)) +
  geom_line() +
  geom_point() +
  labs(title = "Porcentaje de votos 'Sí' por año",
       x = "Año",
       y = "Porcentaje") +
  facet_wrap(~country) +
  theme_minimal()

#Pregunta opcional - funcion para optimizar

custom_optimize <- function(fun, par_init, limites_inferiores, limites_superiores) {
  respuesta <- optim(par = par_init, fn = fun, method = "L-BFGS-B", lower = limites_inferiores, upper = limites_superiores)
  return(respuesta)
}

# Ejemplo de función a optimizar
fun <- function(x) {
  return((x[1] - 1)^2 + (x[2] - 2)^2)
}

# Parámetros iniciales y límites
par_init <- c(0, 0)
limites_inferiores <- c(-2, -2)
limites_superiores <- c(4, 4)

# Llamada a la función de optimización
respuesta <- custom_optimize(fun, par_init, limites_inferiores, limites_superiores)

# Resultado
Resultado <- list(respuesta$par, respuesta$value)
print(Resultado)
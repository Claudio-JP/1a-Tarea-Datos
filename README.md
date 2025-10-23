##Gráfico de visualización de top libros de Star Wars en base de datos desde Good Reads

#Primero, se incorporan los paquetes que serán necesarios
library(tidyverse)
library(readxl)
library(readr)

#Se importa la base de datos, la cual fue creada a partir del API de Goodreads, la cual se descontinuó en 2020.
goodreads <- read_excel("base de datos goodreads.xlsx")

#Posteriormente, se realiza una selección de las variables relevantes que se utilizarán para el análisis, y se realiza un filtro en la variable Title, para considerar solo los libros de Star Wars
goodread_clean <- goodreads %>% select("Title", "avg_rating", "rating count", "text reviews count")
goodread_SW <- goodread_clean %>% filter(str_detect(Title, "Star Wars"))

#En pos de hacer más legible la base resultante y estandarizarla, se cambian los nombres de las columnas
goodread_SW <- goodread_SW %>% rename(titulo = Title,
                                      calif_promedio = avg_rating,
                                      conteo_calif = "rating count",
                                      conteo_calif_text = "text reviews count")
                                      
#Con esto, se procede a ordenar la base en orden descendiente, según la calificación promedio de los libros, y a seleccionar los 10 libros con mayor rating
goodread_SW_10 <- goodread_SW %>% arrange(desc(calif_promedio)) %>%
                               head(10)
                               
#Con los datos ya ordenados y filtrados, se procede a hacer la visualización: se busca mostrar la cantidad de reviews totales y reviews escritas por cada libro en formato de gráfico de columnas, con los libros ordenados según su calificación

colnames(goodread_SW_10)
summary(goodread_SW_10)

grafico_sw <- temp_data %>%
  pivot_longer(cols = c(conteo_calif, conteo_calif_text), 
               names_to = "Tipo", values_to = "Cantidad") %>%
  ggplot(aes(x = titulo, y = Cantidad, fill = Tipo)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Libros Star Wars", y = "Reviews", x = "") +
  theme(axis.text.y = element_text(size = 8))
  
print(grafico_sw)


#Como se percibe en el gráfico, ordenado de manera descendente en cuando a su promedio de calificación, pareciera existir una relación inversamente proporcional entre el rating promedio del libro y la cantidad de calificaciones que textuales y no textuales que reciben. Para explorar un poco más esta relación, se procede a calcular la correlación entre las variables mencionadas
cor_1 <- cor(as.numeric(goodread_SW_10$conteo_calif), as.numeric(goodread_SW_10$calif_promedio)) #-0.607
cor_2 <- cor(as.numeric(goodread_SW_10$conteo_calif_text), as.numeric(goodread_SW_10$calif_promedio)) #-0.442
#Los resultados obtenidos generan curiosidad con respecto a la relación entre las variables en toda la base de datos. Por tanto, se realizarán los mismos cálculos para todos los libros de SW
cor_3 <- cor(as.numeric(goodread_SW$conteo_calif), as.numeric(goodread_SW$calif_promedio)) #-0.107
cor_4 <- cor(as.numeric(goodread_SW$conteo_calif_text), as.numeric(goodread_SW$calif_promedio)) #-0.022
#Curiosamente, la relación entre las variables baja significativamente en intensidad al considerar la totalidad de libros de Star Wars. Para culminar la obtención de datos y pasar a un breve análisis, se obtienen los mismos cálculos para todos los libros en la base de datos
cor_c <- cor(as.numeric(goodread_clean$"rating count"), as.numeric(goodread_clean$avg_rating), use = "complete.obs") #0.035
cor_ct <- cor(as.numeric(goodread_clean$"text reviews count"), as.numeric(goodread_clean$avg_rating), use = "complete.obs") #0.031
#Estos resultados demuestran algunos puntos interesantes: en primer lugar, no hay una relación entre la cantidad de ratings y la valoración del libro. O sea, que más personas hayan leído el libro no significa que sea mejor. En segundo lugar, pareciera que, para el top 10 de Star Wars, los libros más masivos convocan también a lectores más casuales, que podrían dar calificaciones más bajas a estos libros, bajando su calificación promedio comparado a libros de menor convocatoria.
## Visualización de top Libros de Star Wars y análisis de relación entre cantidad de reviews y calificación
### El presente trabajo se desarrolló principalmente debido al gran interés personal por la lectura. Uno de mis principales hobbies desde que tengo memoria es la lectura, principalmente de ficción, por lo que me interesó aprovechar este espacio para explorar un poco más algunos datos disponibles sobre este hobby. Más específicamente, el universo de Star Wars siempre ha sido un interés central en mis lecturas, con lo que el presente trabajo tendrá ese enfoque temático
### De esta manera, para esta primera tarea se realiza un breve análisis exploratorio sobre el top 10 libros de Star Wars, y las relaciones relevantes que surgen entre las variables de interés
### Para esto, se utiliza una base de datos sobre libros disponible en Kaggle, construida a partir de la API de Goodreads, la cual fue descontinuada en 2020.

#### Primero, se incorporan los paquetes que serán necesarios:

library(tidyverse)
library(readr)

#### Posteriormente, se importa la base de datos mencionada: 

goodreads <- read_excel("base de datos goodreads.xlsx")

#### Con esta base, se realiza una selección de las variables relevantes que se utilizarán para el análisis, y se realiza un filtro en la variable Title, para considerar solo los libros de Star Wars:

goodread_clean <- goodreads %>% select("Title", "avg_rating", "rating count", "text reviews count")

goodread_SW <- goodread_clean %>% filter(str_detect(Title, "Star Wars"))

#### En pos de hacer más legible, y estandarizar, la base resultante, se cambian los nombres de las columnas
goodread_SW <- goodread_SW %>% rename(titulo = Title,
                                      calif_promedio = avg_rating,
                                      conteo_calif = "rating count",
                                      conteo_calif_text = "text reviews count")
                                      
#### Con esto, se procede a ordenar la base en orden descendiente, según la calificación promedio de los libros, y a seleccionar los 10 libros con mayor rating:

goodread_SW_10 <- goodread_SW %>% arrange(desc(calif_promedio)) %>%
                               head(10)
                               
#### Con los datos ya ordenados y filtrados, se construye un gráfico que muestre la cantidad de ratings y ratings escritos para el top 10 de libros de Star Wars en la base de datos, ordenados de manera tal que abajo se encuentra el top 1, y en la parte superior del gráfico se encuentra el n° 10, según su calificación promedio:

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

<img width="801" height="412" alt="image" src="https://github.com/user-attachments/assets/c4f67c5b-bd14-482a-ada5-68546ff68ba0" />


#### Como se percibe en el gráfico, pareciera existir una relación inversamente proporcional entre el rating promedio del libro y la cantidad de calificaciones textuales y no textuales que reciben. Para explorar un poco más esta relación, se procede a calcular la correlación entre las variables mencionadas:

###### correlación entre el n° de calificaciones y la calificación promedio
cor_1 <- cor(as.numeric(goodread_SW_10$conteo_calif), as.numeric(goodread_SW_10$calif_promedio))
##### -0.607

###### correlación entre el n° de calificaciones escritas y la calificación promedio
cor_2 <- cor(as.numeric(goodread_SW_10$conteo_calif_text), as.numeric(goodread_SW_10$calif_promedio)) 
##### -0.442
#### Los resultados obtenidos generan curiosidad con respecto a la relación más amplia entre las variables. Por tanto, se realizarán los mismos cálculos para todos los libros de Star Wars:

#### ###### correlación entre el n° de calificaciones y la calificación promedio
cor_3 <- cor(as.numeric(goodread_SW$conteo_calif), as.numeric(goodread_SW$calif_promedio)) 
##### -0.107

###### correlación entre el n° de calificaciones escritas y la calificación promedio
cor_4 <- cor(as.numeric(goodread_SW$conteo_calif_text), as.numeric(goodread_SW$calif_promedio)) 
##### -0.022

#### Curiosamente, la relación entre las variables baja significativamente en intensidad al considerar la totalidad de libros de Star Wars. Para culminar la obtención de datos y pasar a un breve comentario final, se obtienen los mismos cálculos para todos los libros en la base de datos:

##### correlación entre el n° de calificaciones y la calificación promedio global
cor_c <- cor(as.numeric(goodread_clean$"rating count"), as.numeric(goodread_clean$avg_rating), use = "complete.obs") 
##### 0.035

###### correlación entre el n° de calificaciones escritas y la calificación promedio global
cor_ct <- cor(as.numeric(goodread_clean$"text reviews count"), as.numeric(goodread_clean$avg_rating), use = "complete.obs") 
##### 0.031

### Estos resultados demuestran algunos puntos interesantes: en primer lugar, no hay una relación entre la cantidad de ratings y la valoración del libro. O sea, que más personas hayan leído el libro no significa que sea mejor. En segundo lugar, pareciera que, para el top 10 de Star Wars, los libros más masivos convocan también a lectores más casuales, que podrían dar calificaciones más bajas a estos libros, bajando su calificación promedio comparado a libros de menor convocatoria.

##### N del T: no queda claro porqué los códigos quedan guardados en formato de texto en vez de como código. Este error se corregirá para futuras entregas

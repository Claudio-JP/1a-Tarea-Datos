## Visualización de top Libros de Star Wars y análisis de relación entre cantidad de reviews y calificación
### El presente trabajo se desarrolló principalmente debido al gran interés personal por la lectura. Uno de mis principales hobbies desde que tengo memoria es la lectura, principalmente de ficción, por lo que me interesó aprovechar este espacio para explorar un poco más algunos datos disponibles sobre este hobby.Inicialmente, el presente proyecto comenzó con el interés de hacer un estudio sobre los libros de Star Wars registrados en la base. Empero, los resultados obtenidos (y exploraciones de la base de datos no registradas acá) fueron generando un mayor interés por las relaciones entre las distintas variables de interés consideradas: la calificación promedio, el conteo de calificaciones, y las calificaciones de texto por cada libro. Así, se realiza un trabajo de análisis exploratorio sobre estas variables.
### Para esto, se utiliza una base de datos sobre libros disponible en Kaggle, construida a partir de la API de Goodreads, la cual fue descontinuada en 2020.

#### Primero, se incorporan los paquetes que serán necesarios:

library(tidyverse)
library(readxl)
library(readr)

#### Posteriormente, se importa la base de datos mencionada: 

goodreads <- read_excel("base de datos goodreads.xlsx")

#### Con esta base, se realiza una selección de las variables relevantes que se utilizarán para el análisis:

goodread_clean <- goodreads %>% select("Title", "avg_rating", "rating count", "text reviews count")

#### En pos de hacer más legible, y estandarizar, la base resultante, se cambian los nombres de las columnas

goodread_clean <- goodread_clean %>% rename(titulo = Title,
                                      calif_promedio = avg_rating,
                                      conteo_calif = "rating count",
                                      conteo_calif_text = "text reviews count")

#### Posteriormente, se realiza una nueva base, que contenga solamente aquellos libros que sean de Star Wars, realizando un filtro en la variable titulo

goodread_SW <- goodread_clean %>% filter(str_detect(titulo, "Star Wars"))
                                      
#### Con esto, se procede a ordenar la base en orden descendiente, según la calificación promedio de los libros, y a seleccionar los 10 libros con mayor rating:

goodread_SW_10 <- goodread_SW %>% arrange(desc(calif_promedio)) %>%
                               head(10)
                               
#### Con los datos ya ordenados y filtrados, se construye un gráfico que muestre la cantidad de ratings y ratings escritos para el top 10 de libros de Star Wars en la base de datos, ordenados de manera tal que abajo se encuentra el top 1, y en la parte superior del gráfico se encuentra el n° 10, según su calificación promedio:

grafico_sw <- goodread_SW_10 %>%
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

#### Curiosamente, la relación entre las variables baja significativamente en intensidad al considerar la totalidad de libros de Star Wars. Para extender más el análisis, se obtienen los mismos cálculos para todos los libros en la base de datos:

##### correlación entre el n° de calificaciones y la calificación promedio global
cor_c <- cor(as.numeric(goodread_clean$conteo_calif), as.numeric(goodread_clean$calif_promedio), use = "complete.obs") 
##### 0.035

###### correlación entre el n° de calificaciones escritas y la calificación promedio global
cor_ct <- cor(as.numeric(goodread_clean$conteo_calif_text), as.numeric(goodread_clean$calif_promedio), use = "complete.obs") 
##### 0.031

#### Debido a estos resultados, surge el interés de cómo se vería visualmente un gráfico con el mismo formato que el anterior, pero para los top 10 libros de la base de datos. Previo a esto, es necesario eliminar 4 libros que, por razones desconocidas, en la columna avg_rating o calif_promedio, presentan data escrita, no correspondiente a su calificación promedio.

goodread_top_10 <- goodread_clean %>% arrange(desc(calif_promedio)) %>%
                               slice(5:14)

grafico_t10 <- goodread_top_10 %>%
  pivot_longer(cols = c("conteo_calif", "conteo_calif_text"), 
               names_to = "Tipo", values_to = "Cantidad") %>%
  ggplot(aes(x = titulo, y = Cantidad, fill = Tipo)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Libros GoodReads", y = "Reviews", x = "") +
  theme(axis.text.y = element_text(size = 8))                             
print(grafico_t10)

### Los resultados obtenidos generan más preguntas que respuestas con respecto a la relación que existe entre las variables de calificación promedio y cantidad de calificaciones (y calificaciones de texto). Es posible que aquellas que tengan una mayor calificación tengan por explicación, precisamente, la poca cantidad de reviews, las cuales serían solo de fans ávidos de la temática o universo particular, mientras que aquellos que tienden a ser más reconocidos por la opinión pública como mejores libros (LoTR, Harry Potter, etc.) tienden a percibir su promedio de calificación reducido por una mayor variedad. Por tanto, y como paso final de análisis, se realizará un gráfico para los top 10 libros con más reviews y reviews escritas, ilustrando su calificación promedio.

goodread_clean <- goodread_clean %>% mutate(total_calif = conteo_calif + conteo_calif_text)

top_reviews <- goodread_clean %>% arrange(desc(total_calif)) %>%
                                   head(10)
                    
#### se realiza un ajuste previo por errores presentados al hacer el grafico
                                   
top_reviews <- top_reviews %>%
  mutate(calif_promedio = as.numeric(calif_promedio),
         total_calif = as.numeric(total_calif)) %>%
  na.omit()                
  
grafico_calif_promedio <- top_reviews %>%
  ggplot(aes(x = calif_promedio, y = reorder(titulo, -total_calif))) +  # Invertir ejes y ordenar por total_calif
  geom_col(fill = "#1f77b4") +  # Barras con color azul
  theme_minimal() +
  labs(title = "Calificación promedio de los 10 libros con mayor conteo de reviews",
       x = "Calificación Promedio", y = "") +
  theme(axis.text.y = element_text(size = 8)) +
  # Agregar conteo total como etiquetas a la derecha de las barras
  geom_text(aes(label = round(total_calif, 0), x = calif_promedio + 0.05), 
            hjust = 0, size = 3)

print(grafico_calif_promedio)

prom_calif <- sum(top_reviews$calif_promedio) / 10
desviacion_estandar <- sd(top_reviews$calif_promedio, na.rm = TRUE)

### El gráfico obtenido nos permite obtener ciertos valores interesantes para el análisis de la relación entre las variables. De particular interés son el promedio de las calificaciones promedio para estos libros, y la desviación estándar del conjunto con respecto a este promedio. El promedio de calificaciones es de 4.099, mientras que la desviación estándar es de 0.359. Esto ilustra el relativo equilibrio que genera en torno a la calificación promedio la existencia de una gran cantidad de calificaciones para los principales libros, ya que una mayor variedad implicaría una mayor presencia tanto de fans del tópico o universo, como de críticos o fans casuales que podrían no tener una visión tan positiva. Ciertamente, las relaciones entre las variables de interés del presente trabajo requieren de estudios más profundos, pero estos quedarán para futuros trabajos y tareas... posiblemente. 

##### N del T: no queda claro porqué los códigos quedan guardados en formato de texto en vez de como código. Este error se corregirá para futuras entregas.

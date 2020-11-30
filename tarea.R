library(tidyverse)
diamonds  <- read_csv("diamonds.csv")
dim(diamonds)
head(diamonds)
str(diamonds)
diamonds$price
colnames(diamonds)
colSums(is.na(diamonds))

#1. clasificación de variables
#1A
quantitative_vars <- c("X1", "carat", "depth", "table", "price", "x", "y", "z")
qualitative_vars <- c("cut", "color", "clarity")

#1B
interval_scale <- c("carat","depth", "table")
ratio_scale <- c("X1","price", "x", "y", "z")

nominal_scale <- c("color", "clarity")
ordinal_scale <- c("cut")


#2. Tablas de frecuencias variables cualitativas
library(dplyr)

#cut. Asigne la tabla de distribución de frecuencia a una variable denominada tabla_freq_cut. 
#Llame freq a la frecuencia absoluta, porcentage a la frecuencia relativa porcentual.

tabla_freq_cut <- diamonds %>%
  group_by(cut) %>%
  summarize(Freq = n()) %>% 
  mutate(Freq_rel=Freq/nrow(diamonds), 
         Porcentage=Freq_rel*100)

tabla_freq_cut <- diamonds %>%
  group_by(cut) %>%
  summarize(Freq = n()) %>%
  mutate(Porcentage = (Freq/nrow(diamonds)) * 100) 


tabla_freq_cut

#clarity. Asigne la tabla de distribución de frecuencia a una variable denominada table_freq_clarity. 
#Llame freq a la frecuencia absoluta, porcentage a la frecuencia relativa porcentual.

table_freq_clarity <- diamonds %>%
  group_by(clarity) %>%
  summarize(Freq = n()) %>% 
  mutate(Freq_rel=Freq/nrow(diamonds), 
         Porcentage=Freq_rel*100)

table_freq_clarity

#Intente analizar cada tabla e identificar cómo se distribuyen los valores y compararlos entre sí.


#Construya un gráfico de barras para la variable cut que muestre la frecuencia absoluta, 
#pinte las barras de color azul y construya un gráfico de barras para la variable clarity 
#que muestre la frecuencia relativa porcentual, pinte las barras de color rojo. 
#Comente cada uno de los gráficos.


ggplot(data = diamonds, 
       aes(x = cut, fill = cut)) +
  geom_bar(color = 'darkslategray', fill = 'steelblue') + 
  theme(legend.position = "none")+
  xlab("CUT") + 
  ylab("Cantidades") + 
  labs(title = "Gráfico de barras 1",
  subtitle = "Frecuencia absoluta cut")

ggplot(data = diamonds, 
       aes(x = clarity, fill = clarity)) +
  geom_bar(width = 0.4, fill='red', aes(y = (..count..)/sum(..count..))) +
  xlab("CLARITY") +
  scale_y_continuous("Porcentaje",labels=scales::percent) +
  labs(title = "Gráfico de barras 2",
  subtitle = "Frecuencia relativa porcentual clarity")

#Calcule la frecuencia relativa porcentual de la variable color 
#y Construya un gráfico circular. Comente.

color_prop <- diamonds %>% 
  group_by(color) %>% 
  summarize(Prop = n() / nrow(diamonds))
color_prop

ggplot(data = color_prop, 
       aes(x = "", y = Prop, fill = str_c(color," ",round(Prop * 100,2),"%" )   )) + 
  geom_bar(stat = "identity", width = 0.25) +
  coord_polar(theta = "y") +
  geom_text(aes(label = str_c(round(Prop * 100,1), "%")), 
            position = position_stack(vjust = 0.5)) + 
  labs(x = NULL, 
       y = NULL, 
       fill = NULL, 
       title = "Frecuencia relativa porcentual color") + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#si no se notan los %
porcentajes <- as.numeric(round(((prop.table(table(diamonds$color)))*100),2))
porcentajes


#Para agregar más información
#Construya un gráfico de barras para la variable cut por grupo de clarity
#(En la estetica use x=cut y agregue fill=clarity). 
#Luego construya el mismo gráfico pero en geom_bar use position="dodge". 
#Comente cada uno de los gráficos.

cutbyclarity <- diamonds %>% 
  group_by(cut,clarity) %>% 
  summarize(Prop = n() / nrow(diamonds))

cutbyclarity

p <- ggplot(data = diamonds,
            mapping = aes(x = factor(cut),
            fill = factor(clarity)))

# stacked bar chart
p + geom_bar(position = 'stack', stat = 'count')
# dogde bar chart
p + geom_bar(position = 'dodge', stat = 'count')
# stacked + percent barchart
p + geom_bar(position = 'fill', stat = 'count')



# 3
# Agregue una nueva columna al marco de datos diamonds llamada price_categories 
# que divida la columna price en 10 intervalos de clase.

# Utilice el valor de 4 para el argumento dig.lab.
# Asigne los resultados nuevamente al marco de datos diamonds

diamonds <- diamonds %>% 
  mutate(price_categories = cut(price, breaks = 10, dig.lab = 4))


# Cree una tabla de distribución de frecuencia agrupada para la variable price_categories 
# Para cada intervalo de clase, la tabla muestra la frecuencia como Freq y los porcentajes como Percentage.
# Asigne la tabla a una variable llamada price_grouped_freq_table, luego imprímala.

price_grouped_freq_table <- diamonds %>% 
  group_by(price_categories) %>% 
  summarize(Freq = n()) %>% 
  mutate(Percentage = Freq / nrow(diamonds) * 100) 

price_grouped_freq_table


#Construya un histograma para la variable price_categories que muestre la frecuencia absoluta, 
#use fill="yellow" para pintar las barras de amarillo y color="black" para que el contorno de la barras 
#sea negra. Use layer theme(panel.background = element_rect(fill = "white")) 
#para que el fondo sea blanco.

ggplot(data = diamonds, 
       aes(x = price_categories  ) ) +
  geom_histogram(stat = "identity", fill = "yellow", color= "black")


pl <- ggplot(diamonds, 
             aes(x=price))+ 
      geom_histogram(stat = "identity", fill = "yellow", color= "black" )

#funciona
ggplot(data = price_grouped_freq_table, 
       aes(x = price_categories,y = Freq  )) +
  geom_histogram(stat="identity", colour = "black", fill="yellow" )  

#probando
ggplot(data = price_grouped_freq_table,
       aes(x = price_categories, y = Freq)) + 
  geom_histogram(stat = "identity", fill = "yellow", color = "black") + 
  theme(panel.background = element_rect(fill = "white")) 


# Alinear 10 contenedores con el rango de valores presentes para la variable price. 
# Use fill="red" para pintar las barras y color="black" para el contorno.

custom_binwidth <- (max(diamonds$price) - min(diamonds$price))/10

ggplot(data = diamonds,
       aes(x = price)) +
  geom_histogram(boundary = min(  diamonds$price  ),
                 binwidth = custom_binwidth,
                 fill = "red", color = "black" )


ggplot(data = diamonds, 
       aes(x = price)) +
  geom_histogram(bins = 10)



# Calcule las medidas de tendencia central y analice la simetria de la distribución. 
# Apoye su respuesta con histogramas. Comente


media <- mean(diamonds$price)
       
ggplot(data = diamonds,
       aes(x = price)) +
  geom_histogram(alpha = 0.1, 
                 color='blue', 
                 fill='blue', bins=10) +
  geom_vline(aes(xintercept = mean(price), color = 'Mean'))


median  <- median(diamonds$price)

ggplot(data = diamonds,
       aes(x = price)) +
  geom_histogram(alpha = 0.1, 
                 color='blue', 
                 fill='red', bins=10) +
  geom_vline(aes(xintercept = median(price), color = 'Median'))


compute_mode <- function(vector) {
  counts_df  <-  tibble(vector) %>% 
    group_by(vector) %>% 
    summarise(frequency=n()) %>% 
    arrange(desc(frequency)) 
  counts_df$vector[1]
}

compute_mode(diamonds$price)

ggplot(data = diamonds,
       aes(x = price)) +
  geom_histogram(alpha = 0.1, 
                 color='blue', 
                 fill='red', bins=10) +
  geom_vline(aes(xintercept = compute_mode(diamonds$price) , color = 'Mode'))


#histograma con media mediana y moda
ggplot(data = diamonds,
       aes(x = price)) +
  geom_histogram(alpha = 0.1, 
                 color='blue', 
                 fill='red', bins=10) +
  geom_vline(aes(xintercept = mean(price), color = 'Mean'))+
  geom_vline(aes(xintercept = median(price), color = 'Median'))+
  geom_vline(aes(xintercept = compute_mode(diamonds$price) , color = 'Mode'))


#Construya un boxplot para la variable price
#Calcule rango intercuatilico asigne el resultado el nombre iqr,
#Calcule cuartil 1 y cuartil 3 asigne los nombres cuartil_1 y cuartil_3 respectivamente.
#Calcule los límites superior e inferior fuera de los cuales los valores se consideran 
#valores atípicos. Todos los resultados asignelos a una variable llamada ´atipic´. 
#Luego calcule los datos atípicos y asignelos a una variable llamada outliers.


boxplot <- ggplot(data = diamonds,
            aes(x = "", y = price, group = 1)) +
            geom_boxplot()


q1<- quantile(diamonds$price, p=0.25)
q1

q2<- quantile(diamonds$price, p=0.75)
q2

iqr <- q2 - q1
iqr
      
#valores atípicos se definen  Q3+1.5???IQR o más bajos que Q1???1.5???IQR.

lower_bound <- q1 - (1.5 * iqr)
lower_bound

upper_bound <- q2  + (1.5 * iqr)
upper_bound



outliers_low <- sum(diamonds$price < lower_bound) 
outliers_low

outliers_high <- sum(diamonds$price > upper_bound)
outliers_high


atipic<- diamonds %>% filter(price < lower_bound |  price > upper_bound) %>% select(price)

outliers <- ggplot_build( boxplot )[["data"]][[1]][["outliers"]]

#Construya un boxplot para la variable price agrupado por la variable cut. 
#use color="cut" para que pinte el contorno de las cajas distinguiendo el tipo de corte. 
#Comente sus resultados.
  # => es hermoso
            
 
boxplot2 <- ggplot(data = diamonds,
            aes(x = price, y = cut, color = cut)) + 
            geom_boxplot()
            
            
            


#4. Calculando percentiles y rango percentiles.
#¿Que porcentaje de los diamantes tiene un precio superior al precio promedio?  k.
#Calcule el 30% de los diamentes que tienen menor precio. Asigne su respuesta a la variable P30.
#Calcule el 20% de los diamantes que tienen mayor precio.

k <- mean( diamonds$price > media ) * 100
p30 <- quantile(diamonds$price, p=0.3)


p100 <- quantile(diamonds$price, p=1)
p80 <- quantile(diamonds$price, p=0,8)

pm20 = p100 -p80
pm20

#solo para ver de 10 en 10
ppp <- quantile(diamonds$price, prob = seq(0, 1, length = 11), type = 5)



#5. Comparando la variabilidad entre dos variables numéricas.
#Compare la variabilidad de las variables carat y depth 
#¿Qué variable es más homogena? asigne sus respuestas a cv_carat y cv_depth 
#Comente sus resultados.


var(diamonds$carat)
sd(diamonds$carat)


var(diamonds$depth)
sd(diamonds$depth)

# mientras mayor sea la desviación estándar, mayor será la dispersión de los datos.



cv_depth  <-sd(diamonds$depth)/mean(diamonds$depth)
cv_carat  <-sd(diamonds$carat)/mean(diamonds$carat)

p1<- ggplot(data = diamonds, 
            aes(x = depth)) +
  geom_histogram(bins = 10)
p1

p2<- ggplot(data = diamonds, 
            aes(x = carat)) +
  geom_histogram(bins = 10)
p2




#6. Covarianza y correlación
#Use el comando ´selec´parat seleccionar de la base de daos, 
#las variables numericas y asignelas a una variable llamada ´continuas´, 
#luego construya una matriz de varianza- covarianza y la matriz de correlaciones de estas variables 
#numéricas y asigne los resultados a la variable cov_continuas y cor_continuas respectivamente. 
#Comente sus respuestas y responda ¿Que variables están más correlacionadas?. Comente


#no sabia bien a que funcion se referia profe
continuas <- diamonds %>% select_if(function(col)is.numeric(col))
continuas2 <- diamonds %>% select(X1, carat, depth, table, price, x, y, z)


cov_matrix <- cov(continuas)
cor_matrix <- cor(continuas)


#apoye sus respuestas construyendo un gráfico para la matriz de correlaciones.

library(corrplot)
library(grDevices)
library(Hmisc)

corrplot(cor_matrix)

corrplot(cor_matrix,
         p.mat = cor_matrix,
         method="circle",
         type="full",
         order="FPC",
         tl.col="black",
         tl.srt = 20,
         pch.col = "blue",
         insig = "p-value",
         sig.level = -1,
         col = terrain.colors(100))


#COMO LA VISUALIZACION ES MAS FACIL, RESUMO:      
# Igual o cercano a cero  =>  relación debil o sin relación     
# Igual o cercano a 1,    =>  relación lineal directa y fuerte     
# Igual o cercano a -1,   =>  relación lineal inversa y fuerte 



#Use summary para calcular un resumen de las medidas estadíticas para cada variable continua.
#Asige sus resultados a la variable medidas_continuas. Comente.

s_x1 <- summary(diamonds$X1)
s_c <- summary(diamonds$carat)
s_d <- summary(diamonds$depth)
s_t <- summary(diamonds$table)
s_p <- summary(diamonds$price)
s_x <- summary(diamonds$x)
s_y <- summary(diamonds$y)
s_z <- summary(diamonds$z)

medidas_continuas = summary(continuas)
medidas_continuas

#Construya un gráfico de dispersión para las variables carat (en el ejex) y price (en el eje y). 
#Comente.

ggplot(data = diamonds,
       aes(x = carat, y = price, color=price)) + 
  geom_point() +
  geom_jitter()+
  labs(title="Grafico de dispersión carat VS price")

#Evidente realacion peso -> precio


#Construya un gráfico de dispersión para las variables carat (en el ejex) y price (en el eje y) 
#pero agrupe por clarity usando color= clarity en la estética. 
#Comente sus resultados.


ggplot(data = diamonds,
       aes(x = carat, y = price, color=clarity)) + 
  geom_point() +
  geom_jitter()+
  labs(title="Grafico de dispersión carat VS price agrupado por clarity")

#clarity I1 es mas desvalodara 



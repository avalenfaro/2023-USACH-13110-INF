# ---- ENUNCIADO ----
# Un equipo de investigadores del área de interacción humano-información está estudiando
# si el área temática y el nivel de dificultad del problema de información influyen en el 
# tiempo (en segundos) que toma un usuario en formular una consulta de búsqueda para resolver 
# dicho problema. Para ello, han reclutado a un grupo de participantes voluntarios, asignados aleatoriamente 
# a distintos grupos. Cada participante debe resolver tres problemas de información con diferentes niveles de 
# dificultad: baja, media y alta. A su vez, cada grupo debe resolver problemas relacionados a una temática diferente.
# Los datos recolectados contemplan las siguientes variables:

# ---- HIPOTESIS ----
# H0: El tiempo que tardan los usuarios en formular una consulta para un problema de dificultad fácil es igual en las áreas de computación, literatura y química.
# H0: El tiempo que tardan los usuarios en formular una consulta para un problema de dificultad fácil es diferente para al menos una de las áreas.

if(!require("ez")) install.packages("ez")
if(!require("readxl")) install.packages("readxl")
if(!require("ggpubr")) install.packages("ggpubr")
if(!require("tidyverse")) install.packages("tidyverse")

EP07_Datos <- read_excel("EP07 Datos.xlsx")
datos <- EP07_Datos %>% filter(dificultad == "Baja" & (area == "Computación" | area == "Literatura" | area == "Química"))

datos$id = factor(datos$id)
datos$area = factor(datos$area)


cat("1- Se cumple condición de la escala ya que es de tiempo\n")
cat("2- Se cumple segunda condición de independencia y aleatoriedad\n")
cat("3- El gráfico QQ nos demuestra que si se cumple el suspuesto de normalidad.\n")
cat("4- \n")


g1 <- ggqqplot(datos %>% filter(area == "Computación"),
              x = "tiempo",
              color = "area", 
              palette = "#0073C2FF",
              ggtheme = theme_pubclean())

g2 <- ggqqplot(datos %>% filter(area == "Literatura"),
              x = "tiempo",
              color = "area", 
              palette = "#FC4E07",
              ggtheme = theme_pubclean())

g3 <- ggqqplot(datos %>% filter(area == "Química"),
              x = "tiempo",
              color = "area", 
              palette = "green",
              ggtheme = theme_pubclean())

g <- ggarrange(g1, g2 ,g3, ncol = 3, nrow = 1)
print(g)

cat("Procedimiento ANOVA con azANOVA()\n")
prueba <- ezANOVA(
  data = datos,
  dv = tiempo,
  wid = id,
  between = area
)
print(prueba)




cat("---- RESULTADOS ANOVA ----\n")
cat("ANOVA nos da un valor p = 8.376809e-07. Por lo tanto rechazamos H0 en favor de HA.\n\n")

cat("---- HIPOTESIS TEST HOMOCEDASTICIDAD DE LEVENE ----\n")
# ---- HIPOTESIS EN TEST DE LEVENE ----
# H0: Las varianzas de las K poblaciones desde donde se obtuvieron las muestras son iguales.
# HA: Al menos una de las poblaciones de origen tiene una varianza diferente a alguna de las otras poblaciones.

cat("La prueba de homocedasticidad de Levene nos da resultado p = 0.066.\nEste al ser menor que 0.05 no podemos rechazar H0 en favor de HA.\n\n")


g_tam_efecto = ezPlot(
  data = datos,
  dv = tiempo,
  wid = id,
  between = area,
  x = area
)

print(g_tam_efecto)

# Paralelamente utilizamos la función aov para posteriomente utilizar el objeto av en la prueba tukey.
anova <- aov(tiempo ~ area, data = datos)
alfa <- 0.05

cat("---- PRUEBA HSD DE TUKEY ----\n\n")

post_hoc <- TukeyHSD(anova,
                    which = "area",
                    ordered = TRUE,
                    conf.level = 1 - alfa)

print(post_hoc)

cat("Podemos notar que el valor p ajustado entre los grupos Computación-Quimica y Literatura-Quimica son menores\n")
cat("al nivel de significación. Entre estos podemos notar que la mayor diferencia ocurre entre las áreas Literatura-Quimica")



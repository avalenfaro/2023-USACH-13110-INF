# ---- PREGUNTA ----
# La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes
# en asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3
# asignaturas, indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad?
#   Indicación: obtenga la muestra a partir del archivo “EP04 Datos.csv” que se encuentra en el directorio
#   compartido, usando la semilla 102. Considere un nivel de significación α=0,05.


# ---- HIPOTESIS ----
# H0: Existe diferencia significativa en el desempeño de los estudiantes en asignaturas críticas de primer semestre.
# HA: No existe diferencia significativa en el desempeño de los estudiantes en asignaturas críticas de primer semestre.

if(!require("plyr")) install.packages("plyr")
if(!require("readxl")) install.packages("readxl")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("dplyr")) install.packages("dplyr")

set.seed(102)

EP06_Datos <- read_excel("./EP06 Datos.xls") %>% column_to_rownames(., var = "Id")
muestra <- sample_n(EP06_Datos, 50) 

# Construimos la tabla de contengencia
algebra <- count(muestra, "Algebra")
fisica <- count(muestra, "Fisica")
calculo <- count(muestra, "Calculo")


aprueba <- c(algebra$freq[1], fisica$freq[1], calculo$freq[1])
rechaza <- c(algebra$freq[2], fisica$freq[2], calculo$freq[2])

tabla <- as.table(rbind(aprueba, rechaza))
dimnames(tabla) <- list(resultados <- c("Aprueba", "Rechaza"), 
                        asignaturas <- c("Algebra", "Física", "Cálculo"))

print(tabla)

# Realizamos la prueba chi-cuadrado de homogenidad
prueba <- chisq.test(tabla)

print(prueba)

cat("El valor p de la prueba chi-cuadrado de homogeidad es de 0.008006.\n")
cat("Este, al ser menor que nuestro nivel de significación (0.05) podemos rechazar H0 en favor de HA. \n")
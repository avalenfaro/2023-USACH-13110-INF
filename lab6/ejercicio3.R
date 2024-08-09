# ---- PREGUNTA ----
# En su afán de comprender mejor a sus eternos enemigos, los vampiros, Van Helsing ha decidido estudiar
# si vampiresas y vampiros tienen preferencias similares en cuanto al tipo sanguíneo de sus víctimas. ¿Qué
# puede concluir a partir las preferencias alimentarias de estos seres de acuerdo a sus registros, mostrados
# en la siguiente tabla?

# ---- HIPOTESIS ----
# H0: Vampiresas y Vampiros tienen preferencias similares en cuanto al tipo sanguineo de sus victimas.
# HA: Vampiresas y Vampiros NO tienen preferencias similares en cuanto al tipo sanguineo de sus victimas. \n")

alfa <- 0.05

# TABLA DE CONTINGENCIA
vampiro <- c(15, 12, 8, 6)
vampiresa <- c(9, 14, 5, 7)

tabla <- as.table(rbind(vampiro, vampiresa))
dimnames(tabla) <- list(genero = c("Vampiro", "Vampiresa"), 
                        sangre = c("A", "B", "AB", "O"))
print(tabla)

# Aplicamos prueba chi-cuadrado de homogeneidad.
prueba <- chisq.test(tabla)
print(prueba)

cat("Tenemos que el valor p es 0.5804 por lo tanto, al ser mayor al nivel de significación (0.05)\n")
cat("NO podemos rechazar H0 en favor de HA.")
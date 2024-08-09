# ---- PREGUNTA ----
# Una popular discoteca quiere analizar el interés que generan los concursos de baile entre sus visitantes
# más asiduos. Para ello ha invitado a sus mejores clientes, 8 hombres y 10 mujeres, a un evento privado
# con la posibilidad de participar en un concurso. 8 mujeres y 1 hombre decidieron participar. ¿Influye el
# género en la participación en concursos de baile?

# ---- HIPOTESIS ----
# H0: No influye el género en la participación en concursos de baile.
# HA: Si influye el género en la participación en concursos de baile.

# Dado que ambas variables son dicotómicas, aplicarémos el Test de Fisher.

Asistencia <- c(rep("Si", 9), rep("No", 9))
Genero <- c(rep("Mujer", 8), "Hombre", rep("Hombre", 7), rep("Mujer", 2))

data <- data.frame(Genero, Asistencia)
tabla <- xtabs(~., data)

print(tabla)

alfa <- 0.05
prueba <- fisher.test(tabla , 1 - alfa)

print(prueba)

cat("Tenemos que el valor p es 0.01522 por lo tanto, al ser menor al nivel de significación (0.05)\n")
cat("podemos rechazar la H0 en favor de HA.")
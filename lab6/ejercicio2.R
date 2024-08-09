
#En un estudio acerca del efecto de la deficiencia de vitamina B1 durante la infancia temprana, Katz, Haltus
#& Friedmann (2022) (Journal of Neurolinguistics, 62(5), 101042) concluyeron que tal carencia se traduce

#en severos problemas de lenguaje. Un nuevo estudio busca verificar este hallazgo, para lo cual ha
#evaluado el desarrollo del lenguaje a los 5 años de vida de 35 parejas de gemelos idénticos donde, por
#razones médicas, los bebés son alimentados con diferentes fórmulas (una carente de vitamina B1 y la otra, no). Los datos registrados muestran que:
#En 10 parejas de gemelos, ninguno presenta trastornos del lenguaje.
#En 5 parejas de gemelos, ambos presentan trastornos del lenguaje.
#En 9 parejas de gemelos, solo aquel que fue alimentado con la fórmula que sí contiene vitamina
#B1 desarrolla trastornos del lenguaje.
#En las 11 parejas de gemelos restantes, solo el gemelo con carencia de vitamina B1 presenta
#trastornos del lenguaje.
#¿Soportan los nuevos datos la conclusión del estudio original?

# ---- HIPOTESIS ----
# H0: No hay asociación entre la deficiencia de vitamina B1 y los trastornos del lenguaje en los gemelos evaluados a los 5 años de edad.
# HA: Existe una asociación entre la deficiencia de vitamina B1 y los trastornos del lenguaje en los gemelos evaluados a los 5 años de edad.


#por persona 
tabla <- matrix(c( 10, 10, 5, 5, 18, 0, 0 , 22), ncol=4)
colnames(tabla) <- c('-No trastorno', '-Ambos con trastorno', '-Sólo B1', '-Sólo sin B1')
tabla <- addmargins(tabla)
rownames(tabla) <- c('B1 presente', 'B1 ausente', 'Total')
tabla

prueba <- chisq.test(tabla)
prueba

# no se rechaza la hipótesis nula y no se puede afirmar que existe una asociación entre 
#la deficiencia de vitamina B1 y los trastornos del lenguaje en los gemelos evaluados a los 5 años de edad.


# Funciones útiles
media_muestral = function(sample){
  return(sum(sample) / length(sample))
}

# varianza = desvio_estandar^2
varianza = function(sample){
  return(sd(sample)^2)
}

# Ejercicio 1 

# Sea Xi - U(0, b)

b_momentos = function(sample) {
  return(2 * media_muestral(sample));
}

b_maxima_verosimilitud = function(sample) {
  return(max(sample));
}

# Ejercicio 2
b_mediana = function(sample){
  return(2*median(sample));
}

# Ejercicio 3
b = 1
sample = runif(15, min=0, max=b)

print(b_momentos(sample))
print(b_maxima_verosimilitud(sample))
print(b_mediana(sample))

# Ejercicio 4
# a)
sample = runif(15, min=0, max=b)

# b)
estimador_momentos = b_momentos(sample)
estimador_maxima_verosimilitud = b_maxima_verosimilitud(sample)
estimador_mediana = b_mediana(sample)

print(b_momentos(sample))
print(b_maxima_verosimilitud(sample))
print(b_mediana(sample))

# c)
cantidad_de_repeticiones = 1000 

variable_aleatoria_con_b_momentos = c()
variable_aleatoria_con_b_maxima_verosimilitud = c()
variable_aleatoria_con_b_mediana = c()

for(i in 1:cantidad_de_repeticiones){
  sample = runif(15, min=0, max=1)
  variable_aleatoria_con_b_momentos = c(variable_aleatoria_con_b_momentos, b_momentos(sample))
  variable_aleatoria_con_b_maxima_verosimilitud = c(variable_aleatoria_con_b_maxima_verosimilitud, b_maxima_verosimilitud(sample))
  variable_aleatoria_con_b_mediana = c(variable_aleatoria_con_b_mediana, b_mediana(sample))
}

# d)
media_muestral_b_momentos = media_muestral(variable_aleatoria_con_b_momentos)
media_muestral_b_maxima_verosimilitud = media_muestral(variable_aleatoria_con_b_maxima_verosimilitud)
media_muestral_b_mediana = media_muestral(variable_aleatoria_con_b_mediana)

sesgo_b_momentos = b - media_muestral_b_momentos
sesgo_b_maxima_verosimilitud = b - media_muestral_b_maxima_verosimilitud
sesgo_b_mediana = b - media_muestral_b_mediana

print(sesgo_b_momentos)
print(sesgo_b_maxima_verosimilitud)
print(sesgo_b_mediana)

# e) 
varianza_b_momentos = varianza(variable_aleatoria_con_b_momentos)
varianza_b_maxima_verosimilitud = varianza(variable_aleatoria_con_b_maxima_verosimilitud)
varianza_b_mediana = varianza(variable_aleatoria_con_b_mediana)

print(varianza_b_momentos)
print(varianza_b_maxima_verosimilitud)
print(varianza_b_mediana)

# f) ECM = varianza + sesgo^2
error_cuadratico_medio = function (varianza, sesgo) {
  return(varianza + sesgo^2)
}

ecm_b_momentos = error_cuadratico_medio(varianza_b_momentos, sesgo_b_momentos)
ecm_b_maxima_verosimilitud = error_cuadratico_medio(varianza_b_maxima_verosimilitud, sesgo_b_maxima_verosimilitud)
ecm_b_mediana = error_cuadratico_medio(varianza_b_mediana, sesgo_b_mediana)

print(ecm_b_momentos)
print(ecm_b_maxima_verosimilitud)
print(ecm_b_mediana)

# Ejercicio 5
simulacion_mv = function (b, n) {
  cantidad_de_repeticiones = 1000 
  variable_aleatoria_con_b_maxima_verosimilitud = c()
  for(i in 1:cantidad_de_repeticiones){
  sample = runif(n, min=0, max=b)
  variable_aleatoria_con_b_maxima_verosimilitud = c(variable_aleatoria_con_b_maxima_verosimilitud, b_maxima_verosimilitud(sample))
  }
  media_muestral_b_maxima_verosimilitud = media_muestral(variable_aleatoria_con_b_maxima_verosimilitud)
  return (list(sesgo_b_maxima_verosimilitud=(b - media_muestral_b_maxima_verosimilitud), varianza_b_maxima_verosimilitud=varianza(variable_aleatoria_con_b_maxima_verosimilitud)))
}

simulacion_mom = function (b, n) {
  cantidad_de_repeticiones = 1000 
  variable_aleatoria_con_b_momentos = c()
  for(i in 1:cantidad_de_repeticiones){
    sample = runif(n, min=0, max=b)
    variable_aleatoria_con_b_momentos = c(variable_aleatoria_con_b_momentos, b_momentos(sample))
  }
  media_muestral_b_momentos = media_muestral(variable_aleatoria_con_b_momentos)
  return (list(sesgo_b_momentos=(b - media_muestral_b_momentos), varianza_b_momentos=varianza(variable_aleatoria_con_b_momentos)))
}

simulacion_med = function (b, n) {
  cantidad_de_repeticiones = 1000 
  variable_aleatoria_con_b_mediana = c()
  for(i in 1:cantidad_de_repeticiones){
    sample = runif(n, min=0, max=b)
    variable_aleatoria_con_b_mediana = c(variable_aleatoria_con_b_mediana, b_mediana(sample))
  }
  media_muestral_b_mediana = media_muestral(variable_aleatoria_con_b_mediana)
  return (list(sesgo_b_mediana=(b - media_muestral_b_mediana), varianza_b_mediana=varianza(variable_aleatoria_con_b_mediana)))
}

simulacion_mv_b_n = simulacion_mv(1,15)
print(simulacion_mv_b_n)

simulacion_mom_b_n = simulacion_mom(1,15)
print(simulacion_mom_b_n)

simulacion_med_b_n = simulacion_med(1,15)
print(simulacion_med_b_n)

# Funciones útiles
media_muestral = function(sample){
  return(mean(sample))
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

sesgo_b_momentos = media_muestral_b_momentos - b 
sesgo_b_maxima_verosimilitud = media_muestral_b_maxima_verosimilitud - b
sesgo_b_mediana = media_muestral_b_mediana - b

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
  return (list(sesgo_b_maxima_verosimilitud=(media_muestral_b_maxima_verosimilitud - b), varianza_b_maxima_verosimilitud=varianza(variable_aleatoria_con_b_maxima_verosimilitud)))
}

simulacion_mom = function (b, n) {
  cantidad_de_repeticiones = 1000 
  variable_aleatoria_con_b_momentos = c()
  for(i in 1:cantidad_de_repeticiones){
    sample = runif(n, min=0, max=b)
    variable_aleatoria_con_b_momentos = c(variable_aleatoria_con_b_momentos, b_momentos(sample))
  }
  media_muestral_b_momentos = media_muestral(variable_aleatoria_con_b_momentos)
  return (list(sesgo_b_momentos=(media_muestral_b_momentos - b), varianza_b_momentos=varianza(variable_aleatoria_con_b_momentos)))
}

simulacion_med = function (b, n) {
  cantidad_de_repeticiones = 1000 
  variable_aleatoria_con_b_mediana = c()
  for(i in 1:cantidad_de_repeticiones){
    sample = runif(n, min=0, max=b)
    variable_aleatoria_con_b_mediana = c(variable_aleatoria_con_b_mediana, b_mediana(sample))
  }
  media_muestral_b_mediana = media_muestral(variable_aleatoria_con_b_mediana)
  return (list(sesgo_b_mediana=(media_muestral_b_mediana - b), varianza_b_mediana=varianza(variable_aleatoria_con_b_mediana)))
}

simulacion_mv_b_n = simulacion_mv(1,15)
print(simulacion_mv_b_n)

simulacion_mom_b_n = simulacion_mom(1,15)
print(simulacion_mom_b_n)

simulacion_med_b_n = simulacion_med(1,15)
print(simulacion_med_b_n)

# Ejercicio 6
valores_b = runif(100,min = 0,max = 2)

vec_var_mv = c()
vec_var_mom = c()
vec_var_med = c()

vec_sesgo_mv = c()
vec_sesgo_mom = c()
vec_sesgo_med = c()

vec_ecm_mv = c()
vec_ecm_mom = c()
vec_ecm_med = c()

for (i in valores_b) {
  simul_mv = simulacion_mv(i,15)
  simul_mom = simulacion_mom(i,15)
  simul_med = simulacion_med(i,15)
  
  vec_sesgo_mv = c(vec_sesgo_mv, simul_mv[[1]])
  vec_sesgo_mom = c(vec_sesgo_mom, simul_mom[[1]])
  vec_sesgo_med = c(vec_sesgo_med, simul_med[[1]])
  
  vec_var_mv = c(vec_var_mv, simul_mv[[2]])
  vec_var_mom = c(vec_var_mom, simul_mom[[2]])
  vec_var_med = c(vec_var_med, simul_med[[2]])
  
  vec_ecm_mv = c(vec_ecm_mv ,error_cuadratico_medio(simul_mv[[2]],simul_mv[[1]]))      
  vec_ecm_mom = c(vec_ecm_mom ,error_cuadratico_medio(simul_mom[[2]],simul_mom[[1]]))  
  vec_ecm_med = c(vec_ecm_med, error_cuadratico_medio(simul_med[[2]],simul_med[[1]]))
}

par(mfrow = c(1,3))

plot(valores_b,vec_sesgo_mv,col ='red',xlab = 'b',ylab = 'Sesgo',main = 'Sesgo de los Estimadores')
points(valores_b,vec_sesgo_mom,col = 'black')
points(valores_b,vec_sesgo_med,col ='blue')
legend("bottomleft",c("mv","mom","med"),fill=c("red","black","blue"))

plot(valores_b,vec_var_mv,col ='red',xlab = 'b',ylab = 'Varianza',main = 'Varianza de los Estimadores')
points(valores_b,vec_var_mom,col = 'black')
points(valores_b,vec_var_med,col ='blue')
legend("topleft",c("mv","mom","med"),fill=c("red","black","blue"))

plot(valores_b,vec_ecm_mv,col ='red',xlab = 'b',ylab = 'ECM',main = 'ECM de los Estimadores')
points(valores_b,vec_ecm_mom,col = 'black')
points(valores_b,vec_ecm_med,col ='blue')
legend("topleft",c("mv","mom","med"),fill=c("red","black","blue"))

# Ejercicio 7
valores_n = c(15,30,60,120,240)

ecm_mv =c()
ecm_mom = c()
ecm_med = c()

for (i in valores_n) {
  simul_mv = simulacion_mv(1,i)
  simul_mom = simulacion_mom(1,i)
  simul_med = simulacion_med(1,i)
  
  ecm_mv = c(ecm_mv,error_cuadratico_medio(simul_mv[[2]],simul_mv[[1]]))
  ecm_mom = c(ecm_mom,error_cuadratico_medio(simul_mom[[2]],simul_mom[[1]]))
  ecm_med = c(ecm_med,error_cuadratico_medio(simul_med[[2]],simul_med[[1]]))
}

plot(valores_n,ecm_mv,ylim = c(0,0.07),type = 'l',col ='red',xlab = 'n',ylab = 'ECM',main = 'ECM de los Estimadores')
lines(valores_n,ecm_mom,col = 'black')
lines(valores_n,ecm_med,col ='blue')
legend("topright",c("mv","mom","med"),fill=c("red","black","blue"))

# Ejercicio 8
muestra = c(0.917, 0.247, 0.384, 0.530, 0.798, 0.912, 0.096, 0.684, 0.394, 20.1, 0.769, 0.137, 0.352, 0.332, 0.670)

estimador_mv_muestra = b_maxima_verosimilitud(muestra) 
estimador_mom_muestra = b_momentos(muestra)
estimador_med_muestra = b_mediana(muestra)
  
print(estimador_mv_muestra)
print(estimador_mom_muestra)
print(estimador_med_muestra)

# Ejercicio 9
simulacion_mv_con_outliers = function (b, n) {
  cantidad_de_repeticiones = 1000 
  variable_aleatoria_con_b_maxima_verosimilitud = c()
  for(i in 1:cantidad_de_repeticiones){
    sample = runif(n, min=0, max=b)
    sample_with_outliers = c()
    for(value in sample){
      if(rbinom(n=1, size=1, prob=0.005)){
        sample_with_outliers = c(sample_with_outliers, value*100)
      } else {
        sample_with_outliers = c(sample_with_outliers, value)
      }
    }
    variable_aleatoria_con_b_maxima_verosimilitud = c(variable_aleatoria_con_b_maxima_verosimilitud, b_maxima_verosimilitud(sample_with_outliers))
  }
  media_muestral_b_maxima_verosimilitud = media_muestral(variable_aleatoria_con_b_maxima_verosimilitud)
  varianza_maxima_verosimilitud = varianza(variable_aleatoria_con_b_maxima_verosimilitud)
  sesgo_maxima_verosimilitud = media_muestral_b_maxima_verosimilitud - b
  return (list(sesgo_b_maxima_verosimilitud=sesgo_maxima_verosimilitud, varianza_b_maxima_verosimilitud=varianza_maxima_verosimilitud, ecm_b_maxima_verosimilitud=error_cuadratico_medio(varianza_maxima_verosimilitud, sesgo_maxima_verosimilitud)))
}

simulacion_mom_con_outliers = function (b, n) {
  cantidad_de_repeticiones = 1000 
  variable_aleatoria_con_b_momentos = c()
  for(i in 1:cantidad_de_repeticiones){
    sample = runif(n, min=0, max=b)
    sample_with_outliers = c()
    for(value in sample){
      if(rbinom(n=1, size=1, prob=0.005)){
        sample_with_outliers = c(sample_with_outliers, value*100)
      } else {
        sample_with_outliers = c(sample_with_outliers, value)
      }
    }
    variable_aleatoria_con_b_momentos = c(variable_aleatoria_con_b_momentos, b_momentos(sample_with_outliers))
  }
  media_muestral_b_momentos = media_muestral(variable_aleatoria_con_b_momentos)
  varianza_momentos = varianza(variable_aleatoria_con_b_momentos)
  sesgo_momentos = media_muestral_b_momentos - b
  return (list(sesgo_b_momentos=sesgo_momentos, varianza_b_momentos=varianza_momentos, ecm_b_momentos=error_cuadratico_medio(varianza_momentos, sesgo_momentos)))
}

simulacion_med_con_outliers = function (b, n) {
  cantidad_de_repeticiones = 1000 
  variable_aleatoria_con_b_mediana = c()
  for(i in 1:cantidad_de_repeticiones){
    sample = runif(n, min=0, max=b)
    sample_with_outliers = c()
    for(value in sample){
      if(rbinom(n=1, size=1, prob=0.005)){
        sample_with_outliers = c(sample_with_outliers, value*100)
      } else {
        sample_with_outliers = c(sample_with_outliers, value)
      }
    }
    variable_aleatoria_con_b_mediana = c(variable_aleatoria_con_b_mediana, b_mediana(sample_with_outliers))
  }
  media_muestral_b_mediana = media_muestral(variable_aleatoria_con_b_mediana)
  varianza_mediana = varianza(variable_aleatoria_con_b_mediana)
  sesgo_mediana = media_muestral_b_mediana - b
  return (list(sesgo_b_mediana=sesgo_mediana, varianza_b_mediana=varianza_mediana, ecm_b_mediana=error_cuadratico_medio(varianza_mediana, sesgo_mediana)))
}

simulacion_mv_b_n = simulacion_mv_con_outliers(1,15)
print(simulacion_mv_b_n)

simulacion_mom_b_n = simulacion_mom_con_outliers(1,15)
print(simulacion_mom_b_n)

simulacion_med_b_n = simulacion_med_con_outliers(1,15)
print(simulacion_med_b_n)

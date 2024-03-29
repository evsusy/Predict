library(chemosensors)

###Load UNIMANshort, databases de Universidad de Manchester, çposee 200 muestras de 17 sensores.
###Posee 8 clases (Amoniaco 0.01,0.02 0.05 - Acido Propanoico 0.01, 0.02, 0.05 - N-buthanol 0.1, 1) (200x3)
data(UNIMANshort)

print(str(UNIMANshort))

#Matriz dat, posee 200 filas y 17 columnas, contiene las señales de estado estable de 17 sensores en respuesta la perfil de concentracion

X <- UNIMANshort$dat

# se extrae la matriz C que es la matriz de concentración 200 filas y 3 columnas
# se extraen todas las  concentraciones diferentes de cero

conc <- apply(UNIMANshort$C, 1, function(x) x[x != 0])

ind <- apply(UNIMANshort$C, 1, function(x) which(x != 0))
gas <- LETTERS[ind]

save(conc, ind, gas, file = "/home/susana/Documents/projetcs/02-validation-caret-package/Data/UNIMANshort.conc-ind-gas.RData")

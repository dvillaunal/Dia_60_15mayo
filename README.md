```{r Protocolo, eval=FALSE, include=TRUE}
"Protocolo:

 1. Daniel Felipe Villa Rengifo
 
 2. Lneguaje: R

 3. Tema: Funciones built-in in R con valores numéricos  (realice al menos dos ejercicios que requieran cargar archivos externos *.csv cada uno con al menos 50 filas y tres datos por fila, lea y procese la información del archvo leído, y guarde las respuestas a los ejercicios  en archivos independientes tipo *.txt)

 4. Fuentes:
    https://github.com/dvillaunal/Dia_44_29abril"

```

# Ejercicio 1:

Según la información dada se obtiene la sigueinte tabla:

En un estudio estadistico se ha preguntado a un
grupo de estudiantes cuantos discos (CD's)
compraron el año pasado, obteniendose los
resultados que figuran en la tabla.

+ `xi` : variable estadistica (N° de CD's)
+ `ni` : frecuencia absoluta
+ `Ni` : frecuencia absoluta acumulada

Generar:

+ `fi` : frecuencia relativa
+ `Fi` : frecuencia relativa acumulada

```{r}
# Exportamos la base dada:
base1 <- read.csv(file = "Base1.csv", header = T, sep = ",", dec = ".")

# Convertimos en factor los xi:
# N° de CD vendidos en combo:

base1$xi <- as.factor(base1$xi)
```

```{r}
# La frecuencia relativa se calcula así => ni/sum(Ni):
Sumani <- sum(base1$ni)

## Creamos una nueva columna que nos saque la frecuencia acumulda fi:

base1$fi <- base1$ni / Sumani
print(base1$fi)

## Para no tener tantas cifras decimales lo redondeamos a 3 cifras decimales:

base1$fi <- round(base1$fi, digits = 3)

## Corroboramos que la suma de todos los terminos de 1:

print(sum(base1$fi), digits =1)
```

```{r}
# Ahora calculamos Fi frecuncia acumulada:
# La función cumsum nos calcula la frecuencia acumulada:
# Redeondeamos las cifras decimales a 2:

base1$Fi <- round(cumsum(base1$fi), digits = 2)
```


Ahora Vamos a calcular los siguientes estadisticos:

+ Media
+ Mediana
+ Moda
+ Rango
+ Varianza
+ Desviación típica

```{r}
# Calculemos la media:
media <- as.numeric(mean(base1$ni))

# Calculamos la mediana:
mediana <- as.numeric(median(base1$ni))

# Calculamos la moda (esta es facil, ya que solo es mirar cual es el valor ni maximo, y despues mirarla en xi)

print(max(base1$ni))

"Esto corresponde al termino n° 41 de los xi, es decir:"

moda <- as.character(base1$xi[41])

"Moda = paquete de 45 CD's"

# Rango:
" Es restar el max(xi)- min(xi)"
rango <- 50-5

"La varianza es sd(base1$ni)^2"
varianza <- as.numeric(round(sd(base1$ni)^2, digits = 2))

desviación <- as.numeric(round(sd(base1$ni), digits = 2))

# Ahora vamos a calcular el N° de CD's vendidos xi*ni:
# Suma total:

sumaTotal <- sum(as.numeric(base1$xi) * base1$ni)
```

Exportamos los estadisticos dichos:

```{r}
#Exportamos los estadisticos:

data1 <- data.frame(
    "Estadistico" = c("Media",
                      "Mediana",
                      "Moda",
                      "Rango",
                      "Varianza",
                      "Desviación Estandar",
                      "CD's vendidos en Total"),
    "Resultado" = c(media,
                    mediana,
                    moda,
                    rango,
                    varianza,
                    desviación,
                    sumaTotal)
)

write.table(data1, file = "Resultado1.txt", sep = ",", row.names = F)

# Exportamos una segunda tabla por las modificaciones hechas anteriormente:
write.table(base1, file = "Base1mejorado.txt", sep = ",", dec = ".", row.names = F)
```


# 2° Ejercicio:

Vamos a traer una tabla de sueldo y ventas de empleados de una empresa:

Con ellas vamos a calcular:

+ Comisión por venta (2% por venta hecha)
+ Sueldo Bruto (Comisión + Sueldo Total)
+ % de ventas sobre el total `(venta1 / sum(ventas))`
+ Booleanos si cumplio con las metas (TRUE: Positivo = Metas - Ventas, FALSE en caso contrario)


```{r}
# Exportamos la base de datos:
base2 <- read.csv(file = "Base2.csv", sep = ",", dec = ".", header = T)
```

```{r}
# Calculemos las comisiones de cada empleados:
# Funcion Built-in = with(data, operación)
# La añadimos como columna a base2:

base2$comision <- with(base2, Ventas*(0.02))

# Calculamos el sueldo Bruto:

base2$Sueldo_Bruto <- with(base2, comision + Sueldo_Base)

#  % de ventas sobre el total `(venta1 / sum(ventas))`:

base2$"%Ventas_s/total" <- with(base2, round(Ventas/sum(Ventas), digits = 3))

## Verifiquemos que la suma de la nueva columna de 1:

print(sum(base2$"%Ventas_s/total"), digits = 2)


# Ahora hagamos un boleano para ver si cumplio con las metas:

base2$Bool_Metas <- with(base2, Metas - Ventas >= 0)
```

```{r}
# Premiemos a los empleados que cumplieron sus metas:
# Agregando 10.000  pesos a sus sueldos o el total:
metas_cumplidas <- dplyr::filter(base2, Bool_Metas == T)

metas_cumplidas$Sueldo_Bruto <- metas_cumplidas$Sueldo_Bruto + 10000

# Ahora premiamos el Empleado del mes:
print(max(metas_cumplidas$Ventas))

## El que cumple el anterior criterio es Orlando:
Orlando <- dplyr::filter(metas_cumplidas, Nombres == "Orlando")

"Felicidades por ser el empleado del mes"
```

```{r}
# Exportamos los resultados:
write.table(base2, file = "Base2Mejorada.txt", sep = ",", dec = ".", row.names = F)

write.table(metas_cumplidas, file = "Metas_Cumplidas.txt", sep = ",", dec = ".", row.names = F)

write.table(Orlando, file = "EmpleadoDelMes.txt", sep = ",", dec = ".", row.names = F)

```


#lang racket

#|

 PRACTICO 7

|#

#|

funciones de una variable

|#

#|

2) Escribir una función llamada triple que, dado cualquier valor x, retorne el triple de x.
> (triple 4) => 12

|#

(define (triple numero) (* numero 3))

#|

3) Función que, dado cualquier número, retorne la mitad del mismo.
> (mitad 4) => 2

|#

(define (mitad numero) (/ numero 2))


#|

4) Función que, dado cualquier número, retorne la diferencia entre ese número y 20.
> (diferencia20 4) => 16

|#

(define (diferencia20 numero) (abs(- numero 20)))

#|

5) Escribir una función que, dado cualquier valor x, retorne el duplo de la suma de 15 + x.
> (duplo 4) => 38

|#

(define (duplo numero) (* (+ numero 15) 2))

#|

6) Definir una función “circunferencia”, que dado el radio retorne la longitud de la misma.
> (circunferencia 4) => 25.132741228718345

|#

(define (circunferencia diametro) (* 2 pi diametro))

#|

7) Escribir una función que, dada una temperatura en grados Celsius, retorne su valor
expresado en grados Fahrenheit. La fórmula de conversión es f = (9/5) . c + 32
> (c2f c) => 39 1/5

|#

(define (c2f gradosCelsius) (* (+ gradosCelsius 32) 9/5))


#|

8) Sabiendo que: 1 pie = 12 pulgadas; 1 yarda = 3 pies; 1 pulgada = 2.54 centímetros.
Definir tres funciones (yardas, pulgadas y pies) que, dado un valor en centímetros,
retornen esa longitud expresada en esas unidades.
> (pulgadas 1524) => 600
> (pies 1524) => 50
> (yardas 1524) => 16.666666666666668

|#

(define (pulgadas centimetro) (/ centimetro 2.54))

(define (pies centrimetro) (/ (pulgadas centrimetro) 12))

(define (yardas centrimetro) (/ (pies centrimetro) 3))


#|

Funciones de dos variables

|#

#|

9) Definir una función “superficieRectangulo” que dado el ancho y el largo de una
habitación retorne su superficie.
> (superficieRectangulo 2 4) => 8

|#

(define (superficieRectangulo ancho largo) (* ancho largo))


#|

10) Definir una función que dada la base y la altura de un triángulo devuelva el valor del
área.
> (superficieTriangulo 2 4) => 4

|#

(define (superficieTriangulo base altura) (/ (* base altura) 2))

#|

11) La relación entre los lados (a, b) de un triángulo y la hipotenusa viene dada por la
fórmula a
2 + b
2 = h
2

Definir una función para que, dadas las longitudes de los lados, calcule y devuelva la
hipotenusa.
> (pitagoras 3 4) => 5

|#

(define (pitagoras ladoUno ladoDos) (sqrt (+ (expt ladoUno 2) (expt ladoDos 2))))

#|

Funciones de tres o más variables

|#

#|

13) El área de un triángulo cuyos lados son a, b y c se puede calcular por la fórmula:
A = sqrt(p * (p - a) * (p - b) * (p - c))
donde p = (a + b + c) / 2. Definir una función que dados a, b y c, retorne el área del
triángulo.
> (heron 3 4 5) => 6

|#

(define (p a b c) (/ (+ a b c) 2))

(define (heron ladoa ladob ladoc)
  (sqrt(* (p ladoa ladob ladoc) (- (p ladoa ladob ladoc) ladoa) (- (p ladoa ladob ladoc) ladob) (- (p ladoa ladob ladoc) ladoc)))
  )

#|

15) En una carrera de turismo carretera el tiempo se mide en minutos, segundos y
centésimas de segundo y, el espacio recorrido se mide en metros. Definir una función
para ingresarle la distancia recorrida y el tiempo tardado, y devuelva la velocidad
promedio de un automóvil en km/hr.
Sabiendo que 1 metro/segundo = 3.6 Km/h, se puede hacer el cálculo en m/s y
convertir. Para mayor claridad pasamos el tiempo a segundos:
> (velocidad 1000 1 0 0) => 60

|#

(define (velocidad metros minutos segundos centesimas)
  (* (/ metros (+ (* minutos 60) segundos (/ centesimas 100))) 3.6)
  )

#|

Funciones que devuelven listas

|#

#|

16) Definir una función que acepte dos números enteros, y devuelva una lista con el
cociente y el resto la división entera entre los dos números.
> (dcyr 100 15) => '(6 10)


|#

(define (auxiliar dividendo divisor resultado)
  (if (>= dividendo divisor)
      (auxiliar (- dividendo divisor) divisor (+ resultado 1))
      (list resultado dividendo)
      )
  )

(define (divisionEntera dividendo divisor)
  (auxiliar dividendo divisor 0)
  )

#|

17) Definir una función que, dada una cantidad de segundos, devuelva una lista con la
misma cantidad expresada en horas, minutos y segundos.
> (hms 7200) => (2 0 0)

|#

(define (auxiliarSegundos segundosAModificar horas minutos segundos)
  (cond
    ((>= segundosAModificar 3600) (auxiliarSegundos (- segundosAModificar 3600) (+ horas 1) minutos segundos))
    ((>= segundosAModificar 60) (auxiliarSegundos (- segundosAModificar 60) horas (+ minutos 1) segundos))
    (else (list horas minutos segundosAModificar))
    )
  )

(define (hms segundos)
  (auxiliarSegundos segundos 0 0 0)
  )

#|

18) Definir una función que acepte la cantidad de varones y mujeres que hay en un recinto,
y devuelva una lista con el porcentaje de varones y mujeres.
> (porcentajeDeVyM v m) => ‘ (60 40)

|#

(define (verPorcentajes personas totalDePersonas)
  (/ personas totalDePersonas)
  )

(define (porcentajeDeVyM varones mujeres)
  (list (verPorcentajes varones (+ varones mujeres)) (verPorcentajes mujeres (+ varones mujeres)))
  )

#|

Decisiones en Scheme

|#

#|

21) Definir un predicado que acepta un número y devuelve #T si ese número es par.
> (par 4) => #t
> (par 5) => #f

|#

(define (resto dividendo divisor)
  (if (>= dividendo divisor)
      (resto (- dividendo divisor) divisor)
      dividendo
      )
  )

(define (par numero)
  (= 0 (resto numero 2))
  )

#|

22) Definir un predicado que acepta un número y devuelve #T si ese número es divisible
por tres y #F de otro modo.
> (divisiblePor3 6) => #t
> (divisiblePor3 5) => #f

|#

(define (divisiblePor3 numero)
  (= (resto numero 3) 0)
  )

#|

23) Definir un predicado que acepta un número y devuelve #T si ese número es divisible
por seis, un número es divisible por seis si es divisible por dos y por tres.
> (divisiblePor6 6) => #t
> (divisiblePor6 10) => #f

|#

(define (divisiblePor6 numero)
  (and (par numero) (divisiblePor3 numero))
  )

#|

24) Definir un predicado que acepta un número real y devuelve #T si ese número pertenece
al intervalo [0,1].
> (entre0y1 0.5) => #t
> (entre0y1 1.1) => #f

|#

(define (entre0y1 numero)
  (and (rational? numero) (>= numero 0) (<= numero 1))
  )

#|

25) Definir una función para aceptar dos números y devolver el mayor.
> (mayor 2 3) => 3

|#

(define (mayor numeroUno numeroDos)
  (cond
    ((> numeroUno numeroDos) numeroUno)
    ((< numeroUno numeroDos) numeroDos)
    (else numeroUno)
    )
  )

#|

26) Definir un predicado que acepta dos números y devuelve #T si el segundo es múltiplo
del primero.
> (multiplo 2 6) => #t
> (multiplo 6 2) => #f

|#

(define (multiplo numeroUno numeroDos)
  (= (resto numeroDos numeroUno) 0)
  )

#|

27) Definir un predicado que acepta dos números y devuelve #T si el segundo es múltiplo
del primero o viceversa.
> (multiploODivisor 2 6) => #t
> (multiploODivisor 6 3) => #t

|#

(define (multiploODivisor numeroUno numeroDos)
  (or (multiplo numeroUno numeroDos) (multiplo numeroDos numeroUno))
  )

#|

28) En un grupo de tres personas hay dos mellizos, el restante es mucho más viejo.
Preparar una función que acepte las tres edades y devuelva la edad de los mellizos.
> (mellizosYViejo 2 2 5) => 2

|#

(define (mellizosYViejo personaUno personaDos personaTres)
  (cond
    ((or (= personaUno personaDos) (= personaUno personaTres)) personaUno)
    ((= personaDos personaTres) personaDos)
    (else #f)
    )
  )

#|

29) En un grupo de tres personas hay un joven menor de edad y dos mayores de edad
(+18). Construir una función para ingresarle las tres edades y devuelva la edad del
joven.
> (jovenYAdultos 15 20 25) => 15

|#

(define (jovenYAdultos personaUno personaDos personaTres)
  (cond
    ((<= personaUno 18) personaUno)
    ((<= personaDos 18) personaDos)
    ((<= personaTres 18) personaTres)
    (else #f)
    )
  )

#|

30) Escribir un predicado que acepta tres números y devuelve #T si los tres números son
iguales.
> (iguales 2 2 2) => #t
> (iguales 2 3 4) => #f

|#

(define (iguales numeroUno numeroDos numeroTres)
  (= numeroUno numeroDos numeroTres))


#|

31) Escribir una función que acepte tres números y devuelva la diferencia entre el mayor y
el menor.

|#

(define (diferencia numeroUno numeroDos numeroTres)
  (- (max numeroUno numeroDos numeroTres) (min numeroUno numeroDos numeroTres))
  )

#|

33) En una fábrica, la eficiencia de una máquina se calcula en función de las piezas
producidas, por una parte, y de las piezas defectuosas por la otra. Las condiciones son
las siguientes:

a. Producción mínima: 1000 piezas
b. Máximo de piezas defectuosas: 20

El puntaje asignado a la máquina se calcula como sigue:

Puntaje 1: si no cumple con ninguna de las condiciones
Puntaje 2: si cumple solo con la segunda
Puntaje 3: si cumple solo con la primera
Puntaje 4: si cumple con las dos

Definir una función que acepta la cantidad de piezas producidas y la cantidad de
defectuosas y devuelve el puntaje asignado.

|#

(define (eficiencia cantidadDePiezas piezasDefectuosas)
  (cond
    ((and (>= cantidadDePiezas 1000) (<= piezasDefectuosas 20)) 4)
    ((and (>= cantidadDePiezas 1000) (> piezasDefectuosas 20)) 3)
    ((and (< cantidadDePiezas 1000) (<= piezasDefectuosas 20)) 2)
    ((and (< cantidadDePiezas 1000) (> piezasDefectuosas 20)) 1)
    )
  )

#|

37) Función que acepta un número que representa un año, y devuelve #T si ese año es
bisiesto. Un año es bisiesto si es divisible por 4, pero no es divisible por 100, salvo que
sea divisible por 400.

|#

(define (esBisiesto? año)
  (and (= (remainder año 4) 0) (or (not (= (remainder año 100) 0)) (= (remainder año 400) 0)))
  )

#|

43) Preparar una función que acepte dos fechas como DIA1, MES1, AÑO1, y DIA2, MES2,
AÑO2 respectivamente, y devuelva la cantidad de días transcurridos entre ambas.

|#

(define (pasarADias dia mes año)
  (+ dia (* mes 30) (* año 365))
  )

(define (cantidadDeDias diaUno mesUno añoUno diaDos mesDos añoDos)
  (- (pasarADias diaDos mesDos añoDos) (pasarADias diaUno mesUno añoUno))
  )

#|

Recursividad en Scheme

|#

#|

48) Escribir una función recursiva que calcule la potencia con exponente algebraicamente:
        | 1 si n=0
  m^n = |
        | m * m^(n-1) si n > 0

|#

(define (potencia base exponente)
  (if (= exponente 0)
      1
      (* base (potencia base (- exponente 1)))
      )
  )

#|

49) Escribir una función que reciba dos números naturales y devuelva su máximo común
divisor, sabiendo que, si los números son iguales, el MCD es el mismo número, en otro
caso el MCD es igual al MCD entre el menor de ellos y la diferencia entre ambos.

|#

(define (auxiliarMCD numeroUno numeroDos)
  (if (= numeroUno numeroDos)
      numeroUno
      (auxiliarMCD (min numeroUno numeroDos) (- (max numeroUno numeroDos) (min numeroUno numeroDos)))
      )
  )

(define (MCD numeroUno numeroDos)
  (and (and (integer? numeroUno) (positive? numeroUno)) (and (integer? numeroDos) (positive? numeroDos)) (auxiliarMCD numeroUno numeroDos))
  )

#|

51) Números Primos
Divisores
Escribir una función "divisor" que acepta un número "n" y un candidato a divisor
"c", devolviendo TRUE cuando c es divisor de "n" y "NIL" de otro modo.
Números primos
Definir una función que acepte un número "n", y devuelva T cuando "n" es primo,
y nil en otro caso.

|#

(define (divisor numero candidato)
  (= (remainder numero candidato) 0)
  )

(define (auxiliarPrimo numero candidato)
  (if (> candidato 1)
      (and (not (divisor numero candidato)) (auxiliarPrimo numero (- candidato 1)))
      #t
      )
  )

(define (primo numero)
  (auxiliarPrimo numero (- numero 1))
  )

#|

Ejercicios de evaluaciones pasadas:

|#

#|

53) Un número es divisible por 7 cuando separando la primera cifra de la derecha,
multiplicándola por 2, restando este producto de lo que queda a la izquierda y así
sucesivamente, da cero o múltiplo de 7.

Por ejemplo, los siguientes números son divisibles entre 7, porque:
32291 última cifra 1 -> 1x2=2
3229-2 = 3227 última cifra 7 -> 7x2=14
322-14 = 308 última cifra 8 -> 8x2=16
30-16 = 14 resultado parcial 14.
Como 14 pertenece a la tabla del 7 (7x1 … 7x10) el número original es divisible
por 7.

Generar un algoritmo que decida si un número dado es múltiplo de 7 o no,
utilizando este método

|#

(define (calcularNuevoNumero numero)
  (- (quotient numero 10) (* (remainder numero 10) 2))
  )

(define (MultiploDe7 numero)
  (cond
    ((or (= numero 0) (= (remainder numero 7) 0)) #t)
    ((> numero 7) (MultiploDe7 (calcularNuevoNumero numero)))
    (else #f)
      )
  )

#|

54) Se dice que n es un ńumero que explota, cuando éste explota en varios fragmentos
más chicos que él, dada una bomba. Si se tiene que n es el número y b la bomba, tales
que n es mayor que b, se puede hacer que n explote en dos números n1 = n / b (división
entera) y n2 = n - (n / b). Pero b es una bomba que produce una reacción en cadena, si
n1 o n2 son mayores que b, éstos también explotan con la regla anterior, hasta que se
encuentre que el número no es mayor que b, entonces se dice que ya no se puede
explotar el número.

Escribe una función que retorne una lista con los pedazos del número n, dado que se
tiene la bomba b.
> (explota 10 3) => ‘(3 2 1 1 3)
> (explota 20 5) => ‘(4 3 2 2 1 1 1 1 5)

|#

(define (explota numero bomba)
  (if (> numero bomba)
      (cons (explota (quotient numero bomba) bomba) (explota (- numero (quotient numero bomba)) bomba))
      numero
      )
  )









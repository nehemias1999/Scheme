#lang racket

#|

PRACTICO 6

|#

#|

1) Predicado que relaciona una lista numérica de dos elementos con otra lista con esos
dos elementos ordenados de menor a mayor.

|#

(define (acomodados numeroUno numeroDos)
  (if (< numeroUno numeroDos)
      (list numeroUno numeroDos)
      (list numeroDos numeroUno)
      )
  )

#|

2) Predicado que vincula una lista de números enteros, con la cantidad de números
naturales que contiene.

|#

(define (naturales lista)
  (cond
    ((null? lista) 0)
    ((and (integer? (car lista)) (positive? (car lista))) (+ 1 (naturales (cdr lista))))
    (else (naturales (cdr lista)))
    )
  )

#|

3) Predicado que relaciona una lista de temperaturas con la cantidad de temperaturas
que se encuentran entre los 10o y los 20o.

?- cantidadBajoCero([15, 20, -2, -3, 14, 32], X).
X = 2

|#

(define (cantidadBajoCero lista)
  (cond
    ((null? lista) 0)
    ((and (>= (car lista) 10) (<= (car lista) 20)) (+ 1 (cantidadBajoCero (cdr lista))))
    (else (cantidadBajoCero (cdr lista)))
    )
  )

#|

4) Predicado que vincula una lista numérica, con la suma de sus elementos.

?- suma([6, 7, 4, 3, 2, 8], X).
X = 30

|#

(define (suma lista)
  (if (not (null? lista))
      (+ (car lista) (suma (cdr lista)))
      0
      )
  )

#|

5) Predicado "estaEntre", que vincula dos números enteros "m" y "n", con la lista de los
enteros mayores o iguales que "m" y menores o iguales que "n".

?- estaEntre(2, 5, L).
L = [2, 3, 4, 5]

|#

(define (estaEntre minimo maximo)
  (if (<= minimo maximo)
      (cons minimo (estaEntre (+ minimo 1) maximo))
      '()
      )
  )

#|

6) Predicado unario que es verdadero cuando su sujeto es una lista numérica ordenada
en forma creciente.

?- ordenada([3, 6, 8]).
YES
?- ordenada([6, 3, 8]).
FAIL

|#

(define (auxiliarOrdenada ultimo lista)
  (if (null? lista)
      #t
      (and (<= ultimo (car lista)) (auxiliarOrdenada (car lista) (cdr lista)))
      )
  )

(define (ordenada lista)
  (auxiliarOrdenada (car lista) (cdr lista))
  )

#|

7) Predicado que relaciona una lista cualquiera con el elemento que se encuentra en el
último lugar.
?- ultimo([a, b, c, r, f, h], X).
X = h

|#

(define (ultimo lista)
  (if (not (null? (cdr lista)))
      (ultimo (cdr lista))
      (car lista)
      )
  )

#|

8) Predicado cuyos sujetos son dos listas, y que es verdadero cuando la primera lista es
un subconjunto de la segunda.

?- subconjunto([d, a, b], [m, b, f, r, d, a]).
YES
?- subconjunto([d, a, b], [m, k, f, r, d, a]).
FAIL

|#

(define (buscarElemento elemento listaDos)
  (cond
   ((null? listaDos) #f)
   ((= elemento (car listaDos)) #t)
   (else (buscarElemento elemento (cdr listaDos)))
   )
  )

(define (subconjunto listaUno listaDos)
  (cond
    ((null? listaUno) #t)
    ((buscarElemento (car listaUno) listaDos) (subconjunto (cdr listaUno) listaDos))
    (else #f)
    )
  )

#|

9) Predicado que relaciona dos listas con una tercera, formada con los elementos de
ambas.

?- concatenadas([d, a, b], [m, k, f, r, d, a], X).
X = [d, a, b, m, k, f, r, d, a]

|#

(define (concatenar listaUno listaDos)
  (cond
    ((not (null? listaUno)) (cons (car listaUno) (concatenar (cdr listaUno) listaDos)))
    (else listaDos)
    )
  )

(define (concatenadas listaUno listaDos)
  (cond
    ((null? listaUno) listaDos)
    ((null? listaDos) listaUno)
    ((and (null? listaUno) (null? listaDos)) '())
    (else (concatenar listaUno listaDos))
    )
  )

#|

10) Predicado que relaciona una lista L1 con otra lista L2, con los mismos elementos que
L1, pero rotados un lugar a la izquierda.

?- rotada1([a, b, c, d], X).
X = [b, c, d, a]

(define (longitud lista)
  (if (null? lista)
      0
      (+ 1 (longitud (cdr lista)))
      )
  )

(define (auxiliarRotar1 lista)
  (append () ())
  )

(define (rotada1 lista)
  (cond
    ((null? lista) '())
    ((and (not (null? lista)) (> (longitud lista) 1)) (ultimo lista))
    (else lista)
    )
  )


11) Predicado que relaciona una lista L1 con otra lista L2, con los mismos elementos que
L1, pero rotados N lugares a la izquierda.

?- rotadan(4, [a, b, c, d, e, f, g, h, i, j], X).
X = [e, f, g, h, i, j, a, b, c, d]

|#

#|

12) Predicado que relaciona una lista L1 y un elemento A perteneciente a la misma, con
otra lista L2, formada con los mismos elementos de L1, menos A.

?- menos1(d, [a, b, c, d, e, f, g, h, i, j], X).
X = [a, b, c, e, f, g, h, i, j]

|#

(define (auxiliarMenos1 elemento lista)
  (cond
    ((null? lista) '())
    ((= elemento (car lista)) (cdr lista))
    (else (cons (car lista) (auxiliarMenos1 elemento (cdr lista))))
    )
  )

(define (menos1 elemento lista)
  (if (null? lista)
    '()
    (auxiliarMenos1 elemento lista)
    )
  )

#|

13) Predicado que relaciona dos átomos A y B, y dos listas C y D, tal que D es igual a C,
pero con el átomo A sustituido por el B, en su primera ocurrencia.

?- sustituida1(d, m, [a, b, c, d, e, f, g, h, i, j], X).
X = [a, b, c, m, e, f, g, h, i, j]

|#

(define (auxiliarSustituida1 elemento aCambiar lista)
  (cond
    ((null? lista) '())
    ((= elemento (car lista)) (cons aCambiar (cdr lista)))
    (else (cons (car lista) (auxiliarSustituida1 elemento aCambiar (cdr lista))))
    )
  )

(define (sustituida1 elemento aCambiar lista)
  (if (null? lista)
    '()
    (auxiliarSustituida1 elemento aCambiar lista)
    )
  )

#|

15) Predicado que vincula un número decimal, la nueva base y la lista formada por los
dígitos que representan el mismo número en la nueva base.

?- enBase(67, 2, L).
L = [1, 0, 0, 0, 1, 1]

|#

(define (cambiarABase numero base)
  (if (>= numero base)
      (cons (remainder numero base) (cambiarABase (quotient numero base) base))
      (list numero)
      )
  )

(define (aHexadecimal numero)
  (cond
    ((= numero 10) #\a)
    ((= numero 11) #\b)
    ((= numero 12) #\c)
    ((= numero 13) #\d)
    ((= numero 14) #\e)
    ((= numero 15) #\f)
    (else numero)
    )
  )

(define (cambiarAHexadecimal lista)
  (map aHexadecimal lista)
  )

(define (enBase numero base)
  (if (not (= base 16))
      (reverse (cambiarABase numero base))
      (reverse (cambiarAHexadecimal (cambiarABase numero base)))
    )
  )

#|

14) Predicado que relaciona dos átomos A y B, y dos listas C y D, tal que D es igual a C,
pero con el átomo A sustituido por el B, en todas sus ocurrencias.

?- sustituidan(d, m, [a, b, c, d, e, d, g, h, d, j], X).
X = [a, b, c, m, e, m, g, h, m, j]

|#

(define (auxiliarSustituidaN elemento aCambiar lista)
  (cond
    ((null? lista) '())
    ((= elemento (car lista)) (cons aCambiar (auxiliarSustituidaN elemento aCambiar (cdr lista))))
    (else (cons (car lista) (auxiliarSustituidaN elemento aCambiar (cdr lista))))
    )
  )

(define (sustituidaN elemento aCambiar lista)
  (if (null? lista)
    '()
    (auxiliarSustituidaN elemento aCambiar lista)
    )
  )

#|

16) Predicado que relaciona una lista L1 y un número N, con otra lista L2, con los
elementos de L1 menos los primeros N.

?- menosn(4, [a, b, c, d, e, f, g, h, i, j], X).
X = [e, f, g, h, i, j]

|#

(define (auxiliarMenosN elemento lista)
  (cond
    ((null? lista) '())
    ((= elemento (car lista)) (auxiliarMenosN elemento (cdr lista)))
    (else (cons (car lista) (auxiliarMenosN elemento (cdr lista))))
    )
  )

(define (menosN elemento lista)
  (if (null? lista)
      '()
      (auxiliarMenosN elemento lista)
      )
  )

#|

17) Predicado que relaciona una lista con su inversa.

?- inversa([a, b, c, d, e, f, g, h, i, j], X).
X = [j, i, h, g, f, e, d, c, b, a]

|#

(define (eliminarUltimo lista)
  (if (= (length lista) 1)
      '()
      (cons (car lista) (eliminarUltimo (cdr lista)))
      )
  )

(define (auxiliarInversa lista)
  (if (null? lista)
      '()
      (cons (last lista) (auxiliarInversa (eliminarUltimo lista)))
      )
  )

(define (inversa lista)
  (if (or (null? lista) (<= (length lista) 1))
      lista
      (auxiliarInversa lista)
      )
  )

#|

18) Predicado que relaciona un número N y una lista numérica ordenada L1, con otra lista
numérica ordenada L2, formada con los elementos de L1 y el número N.

?- insertado(6, [2, 3, 7, 9, 15], X).
X = [2, 3, 6, 7, 9, 15]

|#

(define (insertado elemento lista)
  (cond
    ((null? lista) (list elemento))
    ((> elemento (car lista)) (cons (car lista) (insertado elemento (cdr lista))))
    (else (cons elemento lista))
    )
  )

#|

19) Predicado que relaciona una lista L1 y un número N, menor o igual que la longitud de
L1, con el enésimo elemento de L1.

?- enesimo(4, [a, b, c, d, e, f, g, h, i, j], X).
X = d

|#

(define (enesimo posicion lista)
  (cond
    ((null? lista) #f)
    ((> posicion 1) (enesimo (sub1 posicion) (cdr lista)))
    (else (car lista))
    )
  )

#|

24) Se necesita saber el puntaje que suma un Pacman en su recorrido hasta llegar a la
terminar el nivel. En su recorrido se puede encontrar con frutas con distinto puntaje y
con el activador del bonus que duplica todos los puntos de las cosas que coma
durante 5 pasos. Escribir un predicado que relacione una lista de objetos que se come
el Pacman con el puntaje obtenido. Para ello usar la siguiente base de hechos.

puntaje(puntito, 1).
puntaje(frutilla, 10).
puntaje(banana, 30).
puntaje(cerezas, 50).
puntaje(fantasma, 100).

?- puntajePacman([puntito, frutilla, puntito, puntito, banana, puntito, puntito,
fantasma, puntito, puntito, cerezas, puntito], Puntos).
Puntos = 198.

?- puntajePacman([puntito, puntito, puntito, banana, puntito, puntito, bonus,
puntito, puntito, fantasma, puntito, puntito, cerezas, puntito], Puntos).
Puntos = 294.


|#

(define (puntos punto)
  (cond
    ((equal? punto "puntito") 1)
    ((equal? punto "frutilla") 10)
    ((equal? punto "banana") 30)
    ((equal? punto "cerezas") 50)
    ((equal? punto "fantasma") 100)
    (else punto)
    )
  )

(define (calcularPuntaje lista bonus)
  (cond
    ((null? lista) 0)
    ((equal? (car lista) "bonus") (calcularPuntaje (cdr lista) 5))
    ((> bonus 0) (+ (* (puntos (car lista)) 2) (calcularPuntaje (cdr lista) (sub1 bonus))))
    (else (+ (puntos (car lista)) (calcularPuntaje (cdr lista) bonus)))
    )
  )

(define (puntajePacman lista)
  (if (null? lista)
      0
      (calcularPuntaje lista 0)
      )
  )

#|

28) Predicado que relaciona una lista con la cantidad de átomos que contiene.

?- atomos([a,b,[c,d],[e],[f,[g,h]]],N).
N = 8

|#

(define (atomos lista)
  (cond
    ((null? lista) 0)
    ((list? (car lista)) (+ (atomos (car lista)) (atomos (cdr lista))))
    (else (+ 1 (atomos (cdr lista))))
    )
  )

#|

29) Predicado que relaciona una lista de listas, con otra lista formada únicamente con los
átomos de la primera.

?- planchada([a,[b,[c,d,[e]],f],g,h],L).
L=[a,b,c,d,e,f,g,h]

|#

(define (planchada lista)
  (cond
    ((null? lista) lista)
    ((list? (car lista)) (append (planchada (car lista)) (planchada (cdr lista))))
    (else (cons (car lista) (planchada (cdr lista))))
    )
  )

#|

31)[Corregido] Predicado que relaciona una lista numérica, un número N y una lista
conteniendo dos sublistas, una con los elementos menores que N y otra con los
mayores a N.

?- separaMayMen( [1, 8, 3, 4, 5, 2, 7], 4, L)
L = [[1, 3, 2], [8, 5, 7]]

|#

(define (separar lista elemento listaMenores listaMayores)
  (cond
    ((null? lista) (list listaMenores listaMayores))
    ((< (car lista) elemento) (separar (cdr lista) elemento (append listaMenores (list (car lista))) listaMayores))
    (else (separar (cdr lista) elemento listaMenores (append listaMayores (list (car lista)))))
    )
  )

(define (separarMayMen lista elemento)
  (if (null? lista)
      empty
      (separar lista elemento empty empty)
      )
  )

#|

35) Escribir un predicado que relacione a una lista multinivel con el nivel más profundo en
el que se encuentre un elemento dado

% nivelMasProfundo(Elemento, ListaM, Nivel).
?- nivelMasProfundo(a, [a, b, [c, d, [e, [f], [g, a]]]], Nivel)
Nivel = 4

|#

(define (auxiliarNivelMasProfundo elemento lista nivelActual nivelNodo)
  (cond
    ((null? lista) nivelNodo)
    ((list? (car lista)) (max (auxiliarNivelMasProfundo elemento (car lista) (add1 nivelActual) nivelNodo) (auxiliarNivelMasProfundo elemento (cdr lista) nivelActual nivelNodo)))
    ((= elemento (car lista)) (auxiliarNivelMasProfundo elemento (cdr lista) nivelActual nivelActual))
    (else (auxiliarNivelMasProfundo elemento (cdr lista) nivelActual nivelNodo))
    )
  )

(define (nivelMasProfundo elemento lista)
  (if (null? lista)
      0
      (auxiliarNivelMasProfundo elemento lista 1 0)
      )
  )














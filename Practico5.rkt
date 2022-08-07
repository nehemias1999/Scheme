#lang racket

#|

41) Otra forma de calcular recursivamente la potencia con exponente natural más
eficientemente es usando la siguiente función:

      | 1 si n es cero
      |
m^n = | (m^2)^(n/2) si n es par
      |
      | m*(m^2)^((n-1)/2) si n es impar

Podemos probar esta función con un caso, por ejemplo 2^8, para mostrar que
efectivamente computa m^n.

2^8 = (2*2)^(8/2) = 4^4 = (4*4)^(4/2) = 16^2 = (16*16)^1 = 256^1 =
256*(256*256)^((1-1)/2) = 256 * (256*256)^0 = 256 * 1 = 256

|#

(define (potencia base exponente)
  (cond
    ((= exponente 0) 1)
    ((even? exponente) (potencia (expt base 2) (/ exponente 2)))
    (else (* base (potencia (expt base 2) (/ (sub1 exponente) 2))))
    )
  )

#|

44) Resolver el problema de encontrar el MCD entre dos números, sabiendo que, si los
números son iguales, el MCD es el mismo número, en otro caso el MCD es igual al
MCD entre el menor de ellos y la diferencia entre ambos.

|#

(define (MCD numeroUno numeroDos)
  (if (= numeroUno numeroDos)
      numeroUno
      (MCD (min numeroUno numeroDos) (- (max numeroUno numeroDos) (min numeroUno numeroDos)))
      )
  )

#|

Escribe un proceso recursivo que permita decidir si un número natural N es divisible por 11. 
Dado que se sabe que un número es divisible entre 11, si y solo si la suma de los digitos de 
posición par menos la suma de los dígitos de posición impar es un múltiplo de 11. 
Por ejemplo: sea N = 2341675, entonces (5 + 6 + 4 + 2) - (7 + 1 + 3) = 6, que no es múltiplo de 11, 
por lo tanto N no es divisible entre 11. 

|#

(define (auxiliarDivisible numero posicion sumaPares sumaImpares)
  (cond
    ((= numero 0) (= (remainder (- sumaPares sumaImpares) 11) 0))
    ((= posicion 0) (auxiliarDivisible (quotient numero 10) (add1 posicion) (+ (remainder numero 10) sumaPares) sumaImpares))
    (else (auxiliarDivisible (quotient numero 10) (sub1 posicion) sumaPares (+ (remainder numero 10) sumaImpares)))
    )
  )

(define (esDivisiblePor11 numero)
  (and (>= numero 11) (auxiliarDivisible numero 0 0 0))
  )


#lang racket

#|----------------------TDA zonas----------------------|#

;Representacion Zonas: Lista de listas
;Zonas de trabajo
;Zona 1: WorkSpace
;Zona 2: Index
;Zona 3: Local Repository
;Zona 4: Remote Repository
;La representacion del TDA zonas quedara de la siguiente manera: '('(WorkSpace) '(Index) '(LocalRepository) '(RemoteRepository))

;Constructor Zonas

;Funcion que crea una lista de lista con todas listas de repositorios
;Dominio: Lista x lista x lista x lista
;Recorrido: Lista
(define (zonas WorkSpace Index LocalRepository RemoteRepository)
                (list WorkSpace Index LocalRepository RemoteRepository))

;Pertenencia Zonas

;Funcion que comprueba que zonas sea una lista
;Dominio: Elemento
;Recorrido: Boolean
(define (zonas? zonas)
  (if (list? zonas)
       #t
       #f))

;Funcion que comprueba que WorkSpace sea una lista
;Dominio: Elemento
;Recorrido: Boolean
(define (WorkSpace? WorkSpace)
  (if (list? WorkSpace)
      #t
      #f))

;Funcion que comprueba que Index sea una lista
;Dominio: Elemento
;Recorrido: Boolean
(define (Index? Index)
  (if (list? Index)
      #t
      #f))

;Funcion que comprueba que LocalRepository sea una lista
;Dominio: Elemento
;Recorrido: Boolean
(define (LocalRepository? LocalRepository)
  (if (list? LocalRepository)
      #t
      #f))

;Funcion que comprueba que RemoteRepository sea una lista
;Dominio: Elemento
;Recorrido: Boolean
(define (RemoteRepository? RemoteRepository)
  (if (list? RemoteRepository)
      #t
      #f))

;Selector Zonas

;Funcion que obtiene el repositorio WorkSpace
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getWorkSpace zonas)
  (if (null? zonas)
      null
      (car zonas)))

;Funcion que obtiene el repositorio Index
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getIndex zonas)
  (if (null? zonas)
      null
      (cadr zonas)))

;Funcion que obtiene el repositorio LocalRepository
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getLocalRepository zonas)
  (if (null? zonas)
      null
      (caddr zonas)))

;Funcion que obtiene el repositorio RemoteRepository
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getRemoteRepository zonas)
  (if (null? zonas)
      null
      (cadddr zonas)))

;Modificadores Zonas

;Funcion que elimina un elemento de una determinada posicion de la lista
;Dominio: Lista x entero
;Recorrido: Lista nueva con el elemento del indice "i" eliminado
;Tipo de recursion: Lineal/Natural
(define (eliminarElemento lista i)
  (if (= i 0)
      (cdr lista)
      (cons (car lista) (eliminarElemento (cdr lista) (- i 1)))))

;Funcion que inserta un elemento en una determinada posicion de la lista
;Dominio: Lista x entero x string
;Recorrido: Lista con el elemento agregado en la posicion de la lista indicada
;Tipo de recursion: Lineal/Natural
(define (insertarElemento lista i elemento)
      (if (= i 0)
          (cons elemento lista)
          (cons (car lista) (insertarElemento (cdr lista) (- i 1) elemento))))

;Funcion que agrega un elemento a la lista (repositorio) en una posicion dada de este, y almacena los cambios del repositorio
;Dominio: Lista x entero x string
;Recorrido: Lista actualizada con el elemento agregado en la posicion indicada
;Tipo de recursion: Lineal/Natural
(define (actualizarLista lista i elemento)
  (if (= i 0)
      (cons elemento (cdr lista))
      (cons (car lista) (actualizarLista (cdr lista) (- i 1) elemento))))

;Funcion que coloca un elemento al final de una lista
;Dominio: Lista x Lista
;Recorrido: Lista
;Tipo de recursion: de Cola
(define (appendCola lista elemento)
  (if (null? lista)
       elemento
      (cons (car lista)(appendCola (cdr lista) elemento))))

;Funcion que compara el primer elemento de L1 con todos los elementos de L2
;Dominio: Lista x lista
;Recorrido: Lista
;Tipo de recursion: Lineal/Natural
(define compararElementos (lambda (L1) (lambda (L2)
  (if (null? L2)
      null
      (if (eqv? L1 (car L2))
          (cons (car L2) ((compararElementos L1) (cdr L2)))
          ((compararElementos L1) (cdr L2)))))))

;Funcion auxiliar de compararElementos, que avanza al siguiente elemento de L1
;Dominio: Lista x lista
;Recorrido: Lista
(define compararElementosAux (lambda (L1)(lambda (L2)
  (if (null? L1)
      null
      (appendCola ((compararElementos (car L1)) L2)((compararElementosAux (cdr L1)) L2))))))


(provide (all-defined-out))
#lang racket

#|----------------------TDA zonas----------------------|#

;Representacion Zonas: Lista de listas
;Zonas de trabajo
;Zona 1: WorkSpace
;Zona 2: Index
;Zona 3: Local Repository
;Zona 4: Remote Repository
;La representacion del TDA zonas quedara de la siguiente manera: '('(WorkSpace) '(Index) '(LocalRepository) '(RemoteRepository))

;Constructor Repositorios

;Funcion que crea un repositorio como lista vacia (se usara para los 4 repositorios)
;Dominio: Lista vacia
;Recorrido: Lista de strings
(define (crearRepositorio)
  (list '()))

;Pertenencia Repositorios

;Funcion que comprueba que crearRepositorio sea una lista
;Dominio: Lista
;Recorrido: Boolean
(define (crearRepositorio? lista)
  (if (list? lista)
      #t
      #f))

;Constructor Zonas

;Funcion que crea una lista de lista con todas listas de repositorios
;Dominio: Lista x lista x lista x lista
;Recorrido: Lista
(define (zonas WorkSpace Index LocalRepository RemoteRepository)
                (list WorkSpace Index LocalRepository RemoteRepository))

;Pertenencia Zonas

;Funcion que comprueba que crearZonas sea una lista
;Dominio: Lista de elementos
;Recorrido: Boolean
(define (zonas? lista)
  (if (list? lista)
       #t
       #f))

;Selector Zonas

;Funcion que obtiene el repositorio WorkSpace
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getWorkSpace zonas)
  (car zonas))

;Funcion que obtiene el repositorio Index
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getIndex zonas)
  (cadr zonas))

;Funcion que obtiene el repositorio LocalRepository
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getLocalRepository zonas)
  (caddr zonas))

;Funcion que obtiene el repositorio RemoteRepository
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getRemoteRepository zonas)
  (cadddr zonas))

;-----------------------Manejo de listas---------------

;Funcion que elimina un elemento de una determinada posicion de la lista
;Dominio: Lista x entero
;Recorrido: Lista nueva con el elemento de la posicion "pos" eliminado
;Tipo de recursion: Lineal/Natural
(define (eliminarElemento lista pos)
  (if (= pos 0)
      (cdr lista)
      (cons (car lista) (eliminarElemento (cdr lista) (- pos 1)))))

;Funcion que inserta un elemento en una determinada posicion de la lista
;Dominio: Lista x entero x string
;Recorrido: Lista con el elemento agregado en la posicion de la lista indicada
;Tipo de recursion: Lineal/Natural
(define (insertarElemento lista pos elemento)
      (if (= pos 0)
          (cons elemento lista)
          (cons (car lista) (insertarElemento (cdr lista) (- pos 1) elemento))))

;Funcion que agrega un elemento a la lista (repositorio) en una posicion dada de este, y almacena los cambios del repositorio
;Dominio: Lista x entero x string
;Recorrido: Lista actualizada con el elemento agregado en la posicion indicada
;Tipo de recursion: Lineal/Natural
(define (actualizarLista lista pos elemento)
  (if (= pos 0)
      (cons elemento (cdr lista))
      (cons (car lista) (actualizarLista (cdr lista) (- pos 1) elemento))))

;Funcion que agrega un elemento al final de la lista
;Dominio: Lista x elemento
;Recorrido: Lista nueva con el elemento agregado al final
(define (agregarElemento lista elemento)
  (append lista (list elemento)))


#|-----------------------TDA Git-----------------------|#

;Representacion

;Funcion que mediante currificacion permite aplicar comandos a otras funciones, dejando un registro de cada comando aplicado.
;En este caso los comandos que se podran utilizar seran seis: Pull, Add, Commit, Push, Status y Log
;Dominio: string (que representara los comandos de la funcion)
;Recorrido: Funcion (funcion en la cual se aplicara el dominio)



#|-----------------------TDA pull----------------------|#

;Representacion

;Funcion que retorna una lista con los cambios realizados desde el RemoteRepository al WorkSpace
;Dominio: Lista x Lista
;Recorrido: Lista
;Tipo de Recursion: 

;Constructor

(define (pull zonas)
  (if (null? zonas)
      (list (getWorkSpace) (getIndex) (getLocalRepository) (getRemoteRepository))
      (if (null? (getRemoteRepository zonas))
          zonas
          (actualizarLista zonas 0 (getRemoteRepository zonas)))))
      
      
;(define (pull RemoteRepository LocalRepository)
;  (if (null? RemoteRepository)
;      LocalRepository
;      (cons (car RemoteRepository)(pull (cdr RemoteRepository) LocalRepository))))

       

#|-----------------------TDA add-----------------------|#

;Representacion

;Funcion que añade los cambios locales registrados en el Workspace al Index
;Dominio: Lista
;Recorrido: Lista 
;Tipo de Recursion:

;Constructor



#|-----------------------TDA commit--------------------|#

;Representacion

;Funcion que genera un commit con los cambios almacenados en el Index, especificando un mensaje descriptivo de este cambio, para llevarlos al LocalRepository
;Dominio: String x Lista
;Recorrido: Lista

#|-----------------------TDA push----------------------|#

;Representacion

;Funcion que envia los commit desde el LocalRepository al RemoteRepository
;Dominio: String (en este caso para las zonas de trabajo)
;Recorrido: Lista

#|------------------TDA zonas->String------------------|#

;Representacion

;Funcion que recibe las zonas de trabajo y entrega una representacion de las mismas como un string posible de visualizar de forma comprensible al usuario.
;Dominio: String (en este caso para las zonas de trabajo) 
;Recorrido: String

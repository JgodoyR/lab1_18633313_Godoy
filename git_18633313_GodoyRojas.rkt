#lang racket

#|----------------------TDA zonas----------------------|#

;Representacion Zonas: Lista de listas
;Zonas de trabajo
;Zona 1: WorkSpace
;Zona 2: Index
;Zona 3: Local Repository
;Zona 4: Remote Repository

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

;Funcion que crea una lista de lista con todas l
;Dominio: Lista x lista x lista x lista
;Recorrido: Lista
(define crearZonas (lambda (WorkSpace Index LocalRepository RemoteRepository)
                (list (WorkSpace Index LocalRepository RemoteRepository))))

;Pertenencia Zonas

;Funcion que comprueba que crearZonas sea una lista
;Dominio: Lista de elementos
;Recorrido: Boolean
(define (crearZonas? lista)(if (list? lista)
                              #t
                              #f))

;Selector Zonas

;Funcion que obtiene el repositorio WorkSpace
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getWorkSpace crearZonas)
  (car crearZonas))

;Funcion que obtiene el repositorio Index
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getIndex crearZonas)
  (cadr crearZonas))

;Funcion que obtiene el repositorio LocalRepository
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getLocalRepository crearZonas)
  (caddr crearZonas))

;Funcion que obtiene el repositorio RemoteRepository
;Dominio: TDA Zonas
;Recorrido: Lista
(define (getRemoteRepository crearZonas)
  (cadddr crearZonas))



#|-----------------------TDA Git-----------------------|#
;Funcion que mediante currificacion permite aplicar comandos a otras funciones, dejando un registro de cada comando aplicado.
;En este caso los comandos que se podran utilizar seran seis: Pull, Add, Commit, Push, Status y Log
;Dominio: string (que representara los comandos de la funcion)
;Recorrido: Funcion (funcion en la cual se aplicara el dominio)




#|-----------------------TDA pull----------------------|#
;Funcion que retorna una lista con los cambios realizados desde el RemoteRepository al LocalRepository
;Dominio: Lista x Lista
;Recorrido: Lista
;Tipo de Recursion: Lineal/Natural

;Constructor

(define (pull RemoteRepository LocalRepository)
  (if (null? RemoteRepository)
      LocalRepository
      (cons (car RemoteRepository)(pull (cdr RemoteRepository) LocalRepository))))

       

#|-----------------------TDA add-----------------------|#
;Funcion que añade los cambios locales registrados en el Workspace al Index
;Dominio: Lista
;Recorrido: Lista 
;Tipo de Recursion:

;Constructor

;Funcion que inserta un elemento en una determinada posicion de la lista
;Dominio: Lista x entero x string
;Recorrido: Lista
;Tipo de recursion: De Cola
(define (insertarElemento lista pos elemento)
  (if (= pos 0)
      (cons elemento lista)
      (cons (car lista) (insertarElemento (cdr lista) (- pos 1) elemento))))

#|-----------------------TDA commit--------------------|#
;Funcion que genera un commit con los cambios almacenados en el Index, especificando un mensaje descriptivo de este cambio, para llevarlos al LocalRepository
;Dominio: String x Lista
;Recorrido: Lista

#|-----------------------TDA push----------------------|#
;Funcion que envia los commit desde el LocalRepository al RemoteRepository
;Dominio: String (en este caso para las zonas de trabajo)
;Recorrido: Lista

#|------------------TDA zonas->String------------------|#
;Funcion que recibe las zonas de trabajo y entrega una representacion de las mismas como un string posible de visualizar de forma comprensible al usuario.
;Dominio: String (en este caso para las zonas de trabajo) 
;Recorrido: String

;Se crea un String para representar las zonas de trabajo
;Dominio: Lista de listas
;Recorrido: String
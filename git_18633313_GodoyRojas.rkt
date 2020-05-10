#lang racket


#|-----------------------TDA Git-----------------------|#
;Funcion que mediante currificacion permite aplicar comandos a otras funciones, dejando un registro de cada comando aplicado.
;En este caso los comandos que se podran utilizar seran seis: Pull, Add, Commit, Push, Status y Log
;Dominio: string (que representara los comandos de la funcion)
;Recorrido: Funcion (funcion en la cual se aplicara el dominio)





;Representacion Zonas: Lista de listas
;Zonas de trabajo
;Zona 1: WorkSpace
;Zona 2: Index
;Zona 3: Local Repository
;Zona 4: Remote Repository

;Constructor Zonas
;Se crea la lista zonas, que contendra una lista para cada zona
(define zonas (lambda (WorkSpace Index LocalRepository RemoteRepository)
                (list (WorkSpace Index LocalRepository RemoteRepository))))

;Pertenencia Zonas
;Dominio: Lista de elementos
;Recorrido: Boleano
(define (zonas? lista)(if (list? lista)
                              #t
                              #f))


#|-----------------------TDA pull----------------------|#
;Funcion que retorna una lista con los cambios realizados desde el RemoteRepository al LocalRepository
;Dominio: String (en este caso para las zonas de trabajo)
;Recorrido: Lista
;Tipo de Recursion:

#|-----------------------TDA add-----------------------|#
;Funcion que añade los cambios locales registrados en el Workspace al Index
;Dominio: Lista
;Recorrido: Lista 
;Tipo de Recursion:

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
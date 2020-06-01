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

;Funcion que define a null como una lista vacia
(define null (list))

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

;Funcion que agrega un elemento al final de la lista
;Dominio: Lista x elemento
;Recorrido: Lista nueva con el elemento agregado al final
(define (agregarElemento lista elemento)
  (append lista (list elemento)))

;Funcion que cambia el elemento en la posicion i de la lista
;Dominio: Lista x Entero x Elemento (entero o string)
;Recorrido: Lista
;Tipo de Recursion: de Cola
(define (cambiarElemento lista i valor)(if (null? lista)
                         null
                         (cons(if (= i 0)
                                   valor
                                   (car lista))
                                   (cambiarElemento (cdr lista)(- i 1)valor))))

#|-----------------------TDA Git-----------------------|#

;Representacion: Funcion currificada

;Constructor

;Funcion que mediante currificacion permite aplicar comandos a otras funciones, dejando un registro de cada comando aplicado.
;En este caso los comandos que se podran utilizar seran seis: Pull, Add, Commit, Push, Status y Log
;Dominio: String (que representara los comandos de la funcion)
;Recorrido: Funcion (funcion en la cual se aplicara el dominio)

(define git (lambda (comando) (lambda (a) (comando a))(lambda (b) (comando b))))

;Ejemplo de uso

;> ((git pull) (zonas '() '() '() '("lab1.rkt" "lab2.rkt")))
;'(("lab1.rkt" "lab2.rkt") () () ("lab1.rkt" "lab2.rkt"))
;(((git commit) "modificacion TDA")(zonas '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))
;'(("lab1.rkt") ("lab2.rkt") ("lab2.rkt" "modificacion TDA") ("lab4.rkt"))


#|-----------------------TDA pull----------------------|#

;Representacion: Lista de listas

;Constructor

;Funcion que retorna una lista con los cambios realizados desde el RemoteRepository al WorkSpace
;Dominio: Lista de listas
;Recorrido: Lista de listas con el contenido del RemoteRepository copiado al WorkSpace (nueva lista zonas)
;Tipo de Recursion: Lineal/Natural
(define (pull zonas)
  (if (null? zonas)
      (list (getWorkSpace) (getIndex) (getLocalRepository) (getRemoteRepository))
      (if (null? (getRemoteRepository zonas))
          zonas
          (actualizarLista zonas 0 (getRemoteRepository zonas)))))

;Ejemplo de uso

;>(pull (zonas '("lab1.rkt") '() '() '()))
;'(("lab1.rkt") () () ())
;> (pull (zonas '() '() '() '("lab1.rkt")))
;'(("lab1.rkt") () () ("lab1.rkt"))
;> (pull (zonas '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))
;'(("lab4.rkt") ("lab2.rkt") ("lab3.rkt") ("lab4.rkt"))

     
#|-----------------------TDA add-----------------------|#

;Representacion: Lista de listas

;Constructor

;Funcion que añade los cambios locales registrados en el Workspace al Index
;Dominio: Lista de listas
;Recorrido: Lista de listas
;Tipo de Recursion: de cola



;(define (add archivos)
;  (if (null? archivos)
;      (list (getWorkSpace) (getIndex) (getLocalRepository) (getRemoteRepository))
;      (if (null? (getWorkSpace zonas))
;          zonas
;          (actualizarLista archivos 1 (getWorkSpace zonas)))))


#|-----------------------TDA commit--------------------|#

;Representacion: Lista de listas

;Constructor

;Funcion que genera un commit con los cambios almacenados en el Index, especificando un mensaje descriptivo de este cambio, para llevarlos al LocalRepository
;Dominio: String x Listas de listas
;Recorrido: Lista de listas
                  
(define commit (lambda (mensaje) (lambda (zonas)
                                   (if (string? mensaje)
                                       (if (null? (getLocalRepository zonas))
                                           (eliminarElemento (insertarElemento zonas 2 (agregarElemento (getIndex zonas) mensaje)) 3)                                         
                                           (list (getWorkSpace zonas) (getIndex zonas) (agregarElemento (getLocalRepository zonas) (agregarElemento (getIndex zonas) mensaje))(getRemoteRepository zonas)))
                                       "El mensaje no es un string, por tanto no es valido")
                                        )))

;ver si es nulo

;> ((commit "modificacion 1")(zonas '() '() '() '()))
;'(() () ("modificacion 1") ())
;> ((commit "modificacion 2")(zonas '("lab1.rkt") '("lab2.rkt") '() '()))
;'(("lab1.rkt") ("lab2.rkt") ("lab2.rkt" "modificacion 2") ())
;> ((commit "modificacion 2")(zonas  '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))
;'(("lab1.rkt")
;  ("lab2.rkt")
;  ("lab3.rkt" ("lab2.rkt" "modificacion 2"))
;  ("lab4.rkt"))

           
#|-----------------------TDA push----------------------|#

;Representacion: Lista de listas

;Constructor

;Funcion que envia los commit desde el LocalRepository al RemoteRepository
;Dominio: Lista de listas
;Recorrido: Lista de listas

(define (push zonas)
  (if (null? zonas)
      (list (getWorkSpace) (getIndex) (getLocalRepository) (getRemoteRepository))
      (if (null? (getLocalRepository zonas))
          zonas
          (actualizarLista zonas 3 (getLocalRepository zonas)))))

;Ejemplo de uso
;> (push (zonas '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))
;'(("lab1.rkt") ("lab2.rkt") ("lab3.rkt") ("lab3.rkt"))
;> (push (zonas '("lab1.rkt") '("lab2.rkt") '() '("lab4.rkt")))
;'(("lab1.rkt") ("lab2.rkt") () ("lab4.rkt"))
;> (push (zonas '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '()))
;'(("lab1.rkt") ("lab2.rkt") ("lab3.rkt") ("lab3.rkt"))

#|------------------TDA zonas->String------------------|#

;Representacion:

;Constructor

;Funcion que recibe las zonas de trabajo y entrega una representacion de las mismas como un string posible de visualizar de forma comprensible al usuario.
;Dominio: String (en este caso para las zonas de trabajo) 
;Recorrido: String

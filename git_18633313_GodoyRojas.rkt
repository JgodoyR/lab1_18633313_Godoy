#lang racket

#|-----------------------Manejo de listas---------------|#

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
;
;

#|-----------------------TDA pull----------------------|#

;Representacion: Lista de listas

;Constructor

;Funcion que retorna una lista con los cambios realizados desde el RemoteRepository al WorkSpace
;Dominio: Lista de listas
;Recorrido: Lista de listas con el contenido del RemoteRepository copiado al WorkSpace (nueva lista zonas)
;Tipo de Recursion: de Cola

(define (pull zonas)
  (if (null? zonas)
      (list (getWorkSpace) (getIndex) (getLocalRepository) (getRemoteRepository))
      (if (null? (getRemoteRepository zonas))
          zonas
          (list (appendCola (getRemoteRepository zonas)(getWorkSpace zonas))(getIndex zonas) (getLocalRepository zonas) (getRemoteRepository zonas)))))
          

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
;Tipo de Recursion: Lineal

(define add (lambda (archivos)(lambda (zonas)
                                (if (archivos? archivos)
                                    (if (null? archivos)
                                        zonas                                    
                                        (if (null? (getIndex zonas))                                        
                                            (list (getWorkSpace zonas) ((compararElementosAux archivos)(getWorkSpace zonas)) (getLocalRepository zonas) (getRemoteRepository zonas))
                                            (list (getWorkSpace zonas) (cons ((compararElementosAux archivos)(getWorkSpace zonas)) (getIndex zonas)) (getLocalRepository zonas) (getRemoteRepository zonas))))
                                    "El tipo de dato de los archivos no es valido"))))

;Pertenencia

;Funcion que comprueba que archivos sea una lista de elementos o una lista nula
;Dominio: Elemento
;Recorrido: Boolean
(define (archivos? archivos)
  (if (list? archivos)
      #t
      #f))

;> ((add '("lab1.rkt"))(zonas '("no" "lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))
;'(("no" "lab1.rkt")
;  (("lab1.rkt") "lab2.rkt")
;  ("lab3.rkt")
;  ("lab4.rkt"))

#|-----------------------TDA commit--------------------|#

;Representacion: Lista de listas

;Constructor

;Funcion que genera un commit con los cambios almacenados en el Index, especificando un mensaje descriptivo de este cambio, para llevarlos al LocalRepository
;Dominio: String x Listas de listas
;Recorrido: Lista de listas
;Tipo de recursion: de Cola
                  
(define commit (lambda (mensaje) (lambda (zonas)
                                   (if (mensaje? mensaje)
                                       (if (null? (getLocalRepository zonas))
                                           (eliminarElemento (insertarElemento zonas 2 (appendCola (getIndex zonas) mensaje)) 3)                                         
                                           (list (getWorkSpace zonas) (getIndex zonas) (appendCola (getLocalRepository zonas) (appendCola (getIndex zonas) mensaje))(getRemoteRepository zonas)))
                                       "El mensaje no es un string, por tanto no es valido")
                                        )))

;Pertenencia:

;Funcion que comprueba que mensaje sea un string
;Dominio: Elemento
;Recorrido: Boolean
(define (mensaje? mensaje)
  (if (string? mensaje)
      #t
      #f))

;> ((commit "modificacion 1")(zonas '() '() '() '()))
;'(() () ("modificacion 1") ())
;> ((commit "modificacion 2")(zonas  '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))
;'(("lab1.rkt")
;  ("lab2.rkt")
;  ("lab3.rkt" ("lab2.rkt" "modificacion 2"))
;  ("lab4.rkt"))
;> ((commit "test")(zonas '("lab.rkt" "lab1.rkt") '("lab2.rkt" "lab.rkt" "lab1.rkt" "lab3.rkt") '("lab5.rkt") '("lab4.rkt")))
;'(("lab.rkt" "lab1.rkt")
;  ("lab2.rkt" "lab.rkt" "lab1.rkt" "lab3.rkt")
;  ("lab5.rkt" ("lab2.rkt" "lab.rkt" "lab1.rkt" "lab3.rkt" "test"))
;  ("lab4.rkt"))
           
#|-----------------------TDA push----------------------|#

;Representacion: Lista de listas

;Constructor

;Funcion que envia los commit desde el LocalRepository al RemoteRepository
;Dominio: Lista de listas
;Recorrido: Lista de listas
;Tipo de recursion: Lineal

(define (push zonas)
  (if (null? zonas)
      (list (getWorkSpace) (getIndex) (getLocalRepository) (getRemoteRepository))
      (if (null? (getLocalRepository zonas))
          zonas
          (actualizarLista (actualizarLista (actualizarLista zonas 3 (getLocalRepository zonas)) 2 '()) 1 '()))))

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

;Funcion aux de zonas->string que recorre cada elemento de la lista zonas y lo convierte en string, y cuando llega al final de la lista (o zona de trabajo)
;se agrega una etiqueta para señalar el repositorio y un salto de linea, y esto se hace mediante el uso del largo de la lista zonas
;Dominio: Lista de listas (zonas) x Entero
;Recorrido: String
;Tipo de recursion: de Cola
(define (zonas->stringAux zonas largo)
  (if (null? zonas)
      (if (= largo 4)
          '(" | WorkSpace" "\n") ;salto de linea
          (if (= largo 3)
              '(" | Index" "\n")
              (if (= largo 2)
                  '(" | Local Repository" "\n")
                  (if (= largo 1)
                      '(" | Remote Repository" "\n")
                      '("\n")))))
      (appendCola (list (car zonas))(zonas->stringAux (cdr zonas) largo))))
  
;Funcion que recibe las zonas de trabajo y entrega una representacion de las mismas como un string posible de visualizar de forma comprensible al usuario.
;Dominio: Lista de listas (zonas)
;Recorrido: String
;Tipo de recursion: de Cola
(define (zonas->string zonas)
  (if (null? zonas)
      null
      (appendCola (zonas->stringAux (car zonas) (length zonas))(zonas->string (cdr zonas)))))

;(display (zonas->string (zonas '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt"))))
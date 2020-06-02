#lang racket

(require "zonas_18633313_GodoyRojas.rkt")

#|-----------------------TDA Git-----------------------|#

;Representacion: Funcion currificada

;Constructor

;Funcion que mediante currificacion permite aplicar comandos a otras funciones, dejando un registro de cada comando aplicado.
;En este caso los comandos que se podran utilizar seran seis: Pull, Add, Commit, Push, Status y Log
;Dominio: String (que representara los comandos de la funcion)
;Recorrido: Funcion (funcion en la cual se aplicara el dominio)

(define git (lambda (comando) (lambda (a) (comando a))(lambda (b) (comando b))))


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
                                           (if (null? (getIndex zonas))
                                               "No se puede realizar el commit debido a que no hay archivos en el Index"
                                               (eliminarElemento (insertarElemento zonas 2 (appendCola (getIndex zonas) mensaje)) 3))                                          
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
          (actualizarLista (actualizarLista (actualizarLista zonas 3 (list(getRemoteRepository zonas) (getLocalRepository zonas))) 2 '()) 1 '()))))


#|------------------TDA zonas->String------------------|#

;Representacion

;Constructor

;Funcion aux de zonas->string que recorre cada elemento de la lista zonas y lo convierte en string, y cuando llega al final de la lista (o zona de trabajo)
;se agrega una etiqueta para señalar el repositorio y un salto de linea, y esto se hace mediante el uso del largo de la lista zonas
;Dominio: Lista de listas (zonas) x Entero
;Recorrido: String
;Tipo de recursion: de Cola

(define (zonas->stringAux zonas largo)
  (if (null? zonas)
      (if (= largo 4)
          '(" | WorkSpace" "\n")
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


;------------------------Ejemplos---------------------------;

;Funcion git
((git pull)(zonas '() '() '() '("lab.rkt")))
(((git add)'("lab.rkt"))(zonas '("lab.rkt") '("pacman.rkt") '() '("lab.rkt")))
(((git commit)"primera modificacion")(zonas '("lab.rkt") '("lab.rkt" "pacman.rkt") '() '("lab.rkt")))
((git push)(zonas '("lab.rkt") '("lab.rkt" "pacman.rkt") '("lab.rkt" "pacman.rkt" . "primera modificacion") '("lab.rkt")))

;Funcion pull
(pull (zonas '("lab1.rkt") '() '() '()))
(pull (zonas '() '() '() '("lab1.rkt")))
(pull (zonas '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))

;Funcion add
((add '("lab.rkt"))(zonas '("lab.rkt" "lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))
((add '())(zonas '("lab.rkt" "lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))
((add '("tetris.rkt" "sudoku.c" "asteroid.rkt"))(zonas '("lab.rkt" "tetris.rkt" "sudoku.c") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))

;Funcion commit
((commit "test")(zonas '("lab.rkt" "lab1.rkt") '("lab2.rkt" "lab.rkt" "lab1.rkt" "lab3.rkt") '("lab5.rkt") '("lab4.rkt")))
((commit "modificacion")(zonas  '("lab1.rkt") '("lab2.rkt") '() '("lab4.rkt")))
((commit "modificacion 2")(zonas  '("lab1.rkt") '() '() '("lab4.rkt")))

;Funcion push
(push (zonas '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '()))
(push (zonas '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt")))
(push (zonas '("lab1.rkt") '("lab2.rkt") '() '("lab4.rkt")))

;Funcion zonas->string
(display (zonas->string (zonas '("lab1.rkt") '("lab2.rkt") '("lab3.rkt") '("lab4.rkt"))))
(display (zonas->string (zonas '("tetris.rkt" "sudoku.c" "asteroid.rkt") '("tetris.rkt" "sudoku.c") '() '())))
(display (zonas->string (zonas '("tetris.rkt" "sudoku.c" "asteroid.rkt") '("tetris.rkt" "sudoku.c") '("tetris.rkt" "sudoku.c") '("tetris.rkt"))))




! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: math prettyprint sequences backtrack kernel arrays ;
IN: nqueens

! renvoie la liste [ 0 .. n-1]
: getList ( n -- seq )
    iota [ ] map ;

! renvoie la liste privee de elt
: getListWithoutN ( seq elt -- seq )
     over empty?
     [
          2drop { }
     ]
     [ 
         2dup [ first ] dip = 
         [
             [ 1 tail ] dip getListWithoutN
         ]
         [
             [ dup first swap 1 tail ] dip getListWithoutN swap prefix
         ]
         if
     ]
     if ;

! renvoie la liste seq1 privée des éléments de seq2
: diffList ( seq1 seq2 -- seq3 )
    swap [ getListWithoutN ] reduce ;

! position de la reine : p = (x, y) avec x, y entre 0 et 7
! ligne sur laquelle on veut placer la prochaine reine : i
! Renvoie la liste des positions sur la ligne i prises par la reine placée en p
! Renvoie  {(x - (i -y)), (x + (i - y)), x} 
: getTakenAreas ( i p -- seq )
    first2 [ swap ] dip - 2dup 2dup drop [ [ + ] [ - ] 2bi* ] dip 3array ;

! filtre les positions impossibles en ne gardant que celles entre 0 et 7
: filterTakenAreas ( p n i -- seq )
    rot getTakenAreas swap [ < ] curry filter [ 0 >= ] filter ;

! Renvoie la liste des positions possibles pour la ligne i en fonctions des reines sur les lignes 0 .. i-1
: getPossiblePos ( n i seq -- seq )
    -rot over [ [ filterTakenAreas ] 2curry map ] dip getList [ diffList ] reduce ;

! Nbe de reines et taille de l'échiquier : n
! Numéro de ligne : i (i entre 0 et 7)
! Liste des reines déjà posées : seq

! !!! Cette fonction ne compile pas !!!
! Initialisation : 8 0 { }
! L'idée était de récupérer la liste des positions possibles pour la ligne i, d'utiliser l'opérateur amb sur cette dernière et ajouter à la liste  des reines le premier élément de cette liste.
! Si la liste des positions possibles est vide et qu'on a i < n-1, on utilise fail pour tester la postion suivante, sinon on appelle récursivement la fonction pour les paramètres n i+1 listeReine si i < n-1.
! Le but était de récupérer avec bag-of l'ensemble des combinaisons valides.
: nqueens ( n i seq -- seq )
    3dup over [ getPossiblePos amb [ swap ] dip swap 2array suffix ] dip swap 3dup 3dup empty? [ 1 - < ] dip and [ 3drop fail ] [ dup empty? [ { } ] [ [ 1 - ] dip nqueens ] ] ; 

! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: io math math.parser kernel sequences ;
IN: hanoi
: move ( a b -- str ) [ number>string ] bi@ " vers " glue ;

: other ( a b -- o ) + 6 swap - ;

: partial ( a b -- a b' ) 2dup drop other ; 

: hanoi ( d a n -- ) 
    3dup 0 > [ = not ] dip and ! dup 0 >
    [
        [ 2dup partial ] dip 1 - 3dup hanoi [ [ move print ] 2dip swap partial ] dip hanoi 
    ]

    [
        3drop
    ]
    if ;

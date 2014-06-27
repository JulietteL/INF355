! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test hanoi io.streams.string ;
IN: hanoi.tests

[ "1 vers 2" ] [ 1 2 move ] unit-test
[ "" ] [ [ 1 1 3 hanoi ] with-string-writer ] unit-test
[ "" ] [ [ 1 1 0 hanoi ] with-string-writer ] unit-test

[ "1 vers 3\n" ] [ [ 1 3 1 hanoi ] with-string-writer ] unit-test


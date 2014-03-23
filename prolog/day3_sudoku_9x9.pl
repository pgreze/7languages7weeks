%% ---
%%  Excerpted from "Seven Languages in Seven Weeks",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/btlang for more book information.
%%---

/*
Test with:

| ?- sudoku([5, 3, _,    _, 7, _,    _, _, _,
        6, _, _,    1, 9, 5,    _, _, _,
        _, 9, 8,    _, _, _,    _, _, _,

        8, _, _,    _, 6, _,    _, _, 3,
        4, _, _,    8, _, 3,    _, _, 1,
        7, _, _,    _, 2, _,    _, _, 6,

        _, 6, _,    _, _, _,    2, 8, _,
        _, _, _,    4, 1, 9,    _, _, 5,
        _, _, _,    _, 8, _,    _, 7, 9], Solution).

gprolog gave me :

Solution = [5,3,_#0(1..2:4),
_#17(2:6),7,_#34(2:6:8),
_#51(1:4:6:8..9),_#68(1..2:4:6),_#85(2:8),
6,_#102(4:7),_#119(2:4:7),
1,9,5,
_#136(3..4:8),_#153(2..4),_#170(2:7..8),
_#187(1..2),9,8,
_#204(2..3:6),4,_#238(2:6),
_#255(1:3:5..6),_#272(1..3:5..6),_#289(2:7),
8,_#306(1:5),_#323(1:5:9),
7,6,_#357(1:4),
_#374(4..5),_#391(2:4..5),3,
4,2,6,
8,5,3,
7,9,1,
7,_#493(1:5),_#510(1:3:5),
9,2,_#544(1:4),
_#561(4..5:8),_#578(4..5),6,
_#595(1:9),6,_#612(1:9),
5,3,7,
2,8,4,
_#697(2..3),_#714(7..8),_#731(2..3:7),
4,1,9,
_#748(3:6),_#765(3:6),5,
_#782(1..3),_#799(1:4..5),_#816(1..5),
_#833(2:6),8,_#850(2:6),
_#867(1:3:6),7,9]

:(

But with :
| ?- sudoku([1, _, _,    _, 7, _,    5, _, 2,
        _, _, 6,    8, 3, _,    _, _, 1,
        _, 5, _,    _, _, 4,    6, 3, _,

        _, 3, 7,    _, 9, 1,    _, _, _,
        6, _, 8,    _, _, _,    7, _, 4,
        _, _, _,    6, 8, _,    1, 5, _,

        _, 6, 1,    7, _, _,    _, 8, _,
        8, _, _,    _, 4, 9,    3, _, _,
        3, _, 4,    _, 6, _,    _, _, 9], Solution).

[1,8,3, 9,7,6, 5,4,2]
[2,4,6, 8,3,5, 9,7,1]
[7,5,9, 2,1,4, 6,3,8]

[5,3,7, 4,9,1, 8,2,6]
[6,1,8, 3,5,2, 7,9,4]
[4,9,2, 6,8,7, 1,5,3]

[9,6,1, 7,2,3, 4,8,5]
[8,2,5, 1,4,9, 3,6,7]
[3,7,4, 5,6,8, 2,1,9]

*/
valid([]).
valid([Head|Tail]) :- 
    fd_all_different(Head), 
    valid(Tail).

sudoku(Puzzle, Solution) :-
        Solution = Puzzle,
        Puzzle = [S11, S12, S13, S14, S15, S16, S17, S18, S19,
                  S21, S22, S23, S24, S25, S26, S27, S28, S29,
                  S31, S32, S33, S34, S35, S36, S37, S38, S39,
                  S41, S42, S43, S44, S45, S46, S47, S48, S49,
                  S51, S52, S53, S54, S55, S56, S57, S58, S59,
                  S61, S62, S63, S64, S65, S66, S67, S68, S69,
                  S71, S72, S73, S74, S75, S76, S77, S78, S79,
                  S81, S82, S83, S84, S85, S86, S87, S88, S89,
                  S91, S92, S93, S94, S95, S96, S97, S98, S99],

        fd_domain(Solution, 1, 9),

        Row1 = [S11, S12, S13, S14, S15, S16, S17, S18, S19],
        Row2 = [S21, S22, S23, S24, S25, S26, S27, S28, S29],
        Row3 = [S31, S32, S33, S34, S35, S36, S37, S38, S39],
        Row4 = [S41, S42, S43, S44, S45, S46, S47, S48, S49],
        Row5 = [S51, S52, S53, S54, S55, S56, S57, S58, S59],
        Row6 = [S61, S62, S63, S64, S65, S66, S67, S68, S69],
        Row7 = [S71, S72, S73, S74, S75, S76, S77, S78, S79],
        Row8 = [S81, S82, S83, S84, S85, S86, S87, S88, S89],
        Row9 = [S91, S92, S93, S94, S95, S96, S97, S98, S99],

        Col1 = [S11, S21, S31, S41, S51, S61, S71, S81, S91],
        Col2 = [S12, S22, S32, S42, S52, S62, S72, S82, S92],
        Col3 = [S13, S23, S33, S43, S53, S63, S73, S83, S93],
        Col4 = [S14, S24, S34, S44, S54, S64, S74, S84, S94],
        Col5 = [S15, S25, S35, S45, S55, S65, S75, S85, S95],
        Col6 = [S16, S26, S36, S46, S56, S66, S76, S86, S96],
        Col7 = [S17, S27, S37, S47, S57, S67, S77, S87, S97],
        Col8 = [S18, S28, S38, S48, S58, S68, S78, S88, S98],
        Col9 = [S19, S29, S39, S49, S59, S69, S79, S89, S99],

        Square1 = [S11, S12, S13, S21, S22, S23, S31, S32, S33],
        Square2 = [S14, S15, S16, S24, S25, S26, S34, S35, S36],
        Square3 = [S17, S18, S19, S27, S28, S29, S37, S38, S39],
        Square4 = [S41, S42, S43, S51, S52, S53, S61, S62, S63],
        Square5 = [S44, S45, S46, S54, S55, S56, S64, S65, S66],
        Square6 = [S47, S48, S49, S57, S58, S59, S67, S68, S69],
        Square7 = [S71, S72, S73, S81, S82, S83, S91, S92, S93],
        Square8 = [S74, S75, S76, S84, S85, S86, S94, S95, S96],
        Square9 = [S77, S78, S79, S87, S88, S89, S97, S98, S99],

        
        valid([Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9,
               Col1, Col2, Col3, Col4, Col5, Col6, Col7, Col8, Col9,
               Square1, Square2, Square3, Square4, Square5, Square6, Square7, Square8, Square9]),

        write('\n'), write(Row1), 
        write('\n'), write(Row2), 
        write('\n'), write(Row3), 
        write('\n'), write(Row4), 
        write('\n'), write(Row5), 
        write('\n'), write(Row6), 
        write('\n'), write(Row7), 
        write('\n'), write(Row8), 
        write('\n'), write(Row9).
        


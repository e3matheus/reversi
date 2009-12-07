% Reversi MiniMax % 

% 1 | 2 | 3 
% ---+---+--- 
% 4 | 5 | 6 
% ---+---+---
% 7 | 8 | 9 
% :- dynamic([board/1]). 

nextplayer(1,2). 
nextplayer(2,1).

inverse_mark(x,o). 
inverse_mark(o,x). 

player_mark(1,x).
player_mark(2,o). 

opponent_mark(1,o). 
opponent_mark(2,x). 

blank_mark(e).

maximizing(x). 

minimizing(o). 

run :- hello, play(1), goodbye. 
run :- goodbye. 

hello :-  initialize, 
          nl, 
          nl, 
          nl, 
          write('La Vieja MiniMax.'),
          read_players, 
          output_players. 

initialize :- randomize, 
              asserta(board([ [e,e,e,e,e,e,e,e], 
                              [e,e,e,e,e,e,e,e],
                              [e,e,e,e,e,e,e,e],
                              [e,e,e,x,o,e,e,e],
                              [e,e,e,o,x,e,e,e],
                              [e,e,e,e,e,e,e,e],
                              [e,e,e,e,e,e,e,e],
                              [e,e,e,e,e,e,e,e]
                            ]) ). 

goodbye :-  board(B), 
            nl, 
            nl,
            write('Juego terminado: '), 
            output_winner(B), 
            retract(board(_)),
            retract(player(_,_)), 
            playagain(V), 
            !, 
            V = s, 
            !, 
            run. 

playagain(V) :- nl, 
                nl, 
                write('Otra vez (s/n)? '), 
                read(V), 
                (V = 's' ; V = 'n'), 
                !.

playagain(V) :- nl, 
                nl, 
                write('¿Cuál parte de "s" o "n" no entendiste?'),                           
                playagain(V). 

read_players :- nl, 
                nl, 
                write('¿Cuántos jugadores humanos? '), 
                read(N), 
                set_players(N). 

set_players(0) :- asserta(player(1,computadora)), 
                  asserta(player(2,computadora)), 
                  !.

set_players(1) :- nl, 
                  write('¿Juegas con "x" o con "o" (X juega primero)? '),                     
                  read(M), 
                  human_playing(M), 
                  !. 

set_players(2) :- asserta(player(1, humano)), 
                  asserta(player(2, humano)), 
                  !. 

set_players(_) :- nl, 
                  write('Ingresa 0, 1 o 2 jugadores.'), 
                  read_players. 

human_playing(M) :- M = x, asserta( player(1, humano) ), asserta( player(2, computadora)), !. 
human_playing(M) :- M = o, asserta( player(1, computadora) ), asserta( player(2, humano) ), !. 
human_playing(_) :- nl, write('Ingresa "x" u "o".'), set_players(1). 

play(P) :- board(B), !, output_board(B), !, \+ game_over(P, B), !, make_move(P, B), !, nextplayer(P, P2), !, play(P2), !. 

searchB([F|_], M, 1, NC) :- square(F, NC, M).
searchB([_|R], M, NF, NC) :- T is NF-1, searchB(R, M, T, NC).

cuentaB([F|R], M, T) :- cuenta(F, 8, M, TF), cuentaS(R, M, R), T is R + TF.
cuentaB([F|_], M, T) :- cuenta(F, 8, M, T).

cuentaF(_,0,_,0).
cuentaF(F, C, M, T) :- square(F, C, M), cuentaF(F,C-1,M,TF), T is TF + 1.
cuentaF(F, C, M, T) :- cuentaF(F,C-1,M,T).

square([M,_,_,_,_,_,_,_],1,M).
square([_,M,_,_,_,_,_,_],2,M).
square([_,_,M,_,_,_,_,_],3,M).
square([_,_,_,M,_,_,_,_],4,M).
square([_,_,_,_,M,_,_,_],5,M).
square([_,_,_,_,_,M,_,_],6,M).
square([_,_,_,_,_,_,M,_],7,M).
square([_,_,_,_,_,_,_,M],8,M).

win(B, M) :- cuentaB(B, M, T1), inverse_mark(M,O), cuentaB(B, O, T2), T1 > T2, TOT is T1 + T2, TOT = 64. 

game_over(_, B) :- blank_mark(E), \+ searchB(B, E, _, _). 

make_move(P, B) :- player(P, Type), make_move2(Type, P, B, B2), retract( board(_) ), asserta( board(B2) ). 

make_move2(humano, P, B, B2) :- nl, 
                                nl, 
                                write('El Jugador '), 
                                write(P), 
                                write(' marca en la fila? '), 
                                read(F), 
                                write('columna? '), 
                                read(C),
                                player_mark(P, M),
                                valido(B, M, F, C),
                                modReversi(B, M, F, C, B2),
                                !.
make_move2(humano, P, B, B2) :- nl, nl, write('Claaaro... seguuuuro... Intenta de nuevo.'), make_move2(humano,P,B,B2).

valido(B, M, F, C) :- blank_mark(E), 
                      searchB(B, E, F, C),
                      validReversi(B, M, F, C), !.

validReversi(B, M, F, C) :- inverse_mark(M, I), !,
                            dir(D), 
                            existDir(B, D, M, I, F, C).

existDir(B, D, M, I, F, C) :- getPos(D, F, C, NF, NC),
                              searchB(B, I, NF, NC), 
                              searchDBI(B, D, I, NF, NC, TF, TC, NF1, NC1), 
                              getPos(D, NF1, NC1, NF2, NC2),
                              searchB(B, M, NF2, NC2),!.

modReversi(B, M, F, C, B2) :- set_board(B, M, F, C, B1, 1),
                              inverse_mark(M, I), 
                              modi(B1, M, I, F, C, [ul, uc, ur, cl, cr, dl, dc, dr], 1, B2).

modiAll(B, [NF], [NC], M, I, [ul, uc, ur, cl, cr, dl, dc, dr], 1, B2):-  
    modi(B, M, I, NF, NC, [ul, uc, ur, cl, cr, dl, dc, dr], 1, B1).
modiAll(B, [NF|TF], [NC|TC], M, I, [ul, uc, ur, cl, cr, dl, dc, dr], 1, B2):-  
    modi(B, M, I, NF, NC, [ul, uc, ur, cl, cr, dl, dc, dr], 1, B1),
    modiAll(B1, TF, TC, M, I, [ul, uc, ur, cl, cr, dl, dc, dr], 1, B2).

modi(B, M, I, F, C, [D], 8, B2):-  modifyDir(B, D, M, I, F, C, B2).
modi(B, M, I, F, C, [D|R], Cont, B3):- modifyDir(B, D, M, I, F, C, B2),
                                       CT is Cont+1,
                                       modi(B2, M, I, F, C, R, CT, B3).

modifyDir(B, D, M, I, F, C, B2) :- getPos(D, F, C, NF, NC),
                                   searchB(B, I, NF, NC), 
                                   searchDBI(B, D, I, NF, NC, TF, TC, NF1, NC1), 
                                   getPos(D, NF1, NC1, NF2, NC2),
                                   searchB(B, M, NF2, NC2),
                                   set_list_values(B, M, [NF|TF], [NC|TC], B2).
modifyDir(B, _, _, _, _, _, B).

set_list_values(B, M, [F|RF], [C|RC], B3) :- set_board(B, M, F, C, B2, 1), set_list_values(B2, M, RF, RC, B3).
set_list_values(B, M, F, C, B2) :- set_board(B, M, F, C, B2, 1).

searchDBI(B, D, I, NF, NC, [NF1| TFY], [NC1| TCY], NF2, NC2) :- getPos(D, NF, NC, NF1, NC1),
                                                                searchB(B, I, NF1, NC1), 
                                                                searchDBI(B, D, I, NF1, NC1, TFY, TCY, NF2, NC2).
searchDBI(_, _, _, NF, NC, NF, NC, NF, NC).

dir(ul).
dir(uc).
dir(ur).
dir(cl).
dir(cr).
dir(dl).
dir(dc).
dir(dr).

getPos(ul, F, C, NF, NC) :- NF is F-1, NC is C-1.
getPos(uc, F, C, NF, C) :-  NF is F-1.
getPos(ur, F, C, NF, NC) :- NF is F-1, NC is C+1.
getPos(cl, F, C, F, NC) :-  NC is C-1.
getPos(cr, F, C, F, NC) :- NC is C+1.
getPos(dl, F, C, NF, NC) :- NF is F+1, NC is C-1.
getPos(dc, F, C, NF, C) :- NF is F+1.
getPos(dr, F, C, NF, NC) :- NF is F+1, NC is C+1.

set_board([F|RF], M, NF, C, [B2|RF], NF) :- set_nth(F, C, M, B2, 1),!.
set_board([F|RF], M, NF, C, [F|B2], Cont) :- CT is Cont+1, set_board(RF, M, NF, C, B2, CT).

set_nth([],_,_,[],_).
set_nth([_|T],N,V,[V|T],N). 
set_nth([H|T],N,V,[H|T1],C) :- C \= N, C1 is C+1, set_nth(T,N,V,T1,C1), !. 

output_players :- nl, player(1, V1), write('Jugador 1 es '), write(V1), nl, player(2, V2), write('Jugador 2 es '), write(V2), !. 

output_winner(B) :- win(B,x), write('Gana X.'), !. 
output_winner(B) :- win(B,o), write('Gana O.'), !.
output_winner(_) :- write('The cake is a lie.'). 

output_board :- board(B), output_board(B), !. 
output_board(B) :- nl, nl, write('    1   2   3   4   5   6   7   8 '), nl, write('   ---+---+---+---+---+---+---+---+'), o_board(B, 1).

o_board([UF], 8) :- nl, output_row(UF, 8, 1), nl,  write('   ---+---+---+---+---+---+---+---+'), !.
o_board([F|RF], C) :-  nl, output_row(F, C, 1), nl, write('   ---+---+---+---+---+---+---+---+'), C1 is C + 1, o_board(RF, C1),!.

output_row(F, _, 8) :- output_square(F, 8), !.
output_row(F, NF, 1) :- write(' '), write(NF), write(' '), output_square(F,1), write('|'),  C1 is 2, output_row(F, NF, C1).
output_row(F, NF, C) :- output_square(F,C), write('|'),  C1 is C+1, output_row(F, NF, C1).
 
output_square(F,S) :- square(F,S,M), write(' '), output_square2(S,M), write(' '), !. 
output_square2(_, E) :- blank_mark(E), write(' '), !. 
output_square2(_, M) :- write(M), !.

/*
output_value(D,S,U) :- D == 1, nl, write('Square '), write(S), write(', utility: '), write(U), !. 
output_value(D,S,U). 

move(B,N,M,B2) :- set_nth(B,N,M,B2).
set_nth(L, N, V, L2) :- set_nth(L, N, V, L2, 1). 

make_move2(computadora, P, B, B2) :- nl, nl, write('Pensando...'), player_mark(P, M), minimax(0, B, M, S, _), move(B,S,M,B2), nl, nl, write('Yo marco '), write(M), write(' en '), write(S), write('.').

moves(B,L) :- \+ win(B,x), \+ win(B,o), blank_mark(E), findall(N, square(B,N,E), L), L \= []. 

utility(B,U) :- win(B,x), U = 1, !.
utility(B,U) :- win(B,o), U = (-1), !. 
utility(_,U) :- U = 0. 

% minimax(_,[e,e,e,e,e,e,e,e,e],_,S,_) :- 
% random(1,9,S), !.

minimax(D,B,M,S,U) :- D2 is D + 1, moves(B,L), !, best(D2,B,M,L,S,U), !.
minimax(_,B,_,_,U) :- utility(B,U). 

best(D,B,M,[S1],S,U) :- move(B,S1,M,B2), inverse_mark(M,M2), !, minimax(D,B2,M2,_S,U), S = S1, !, output_value(D,S,U), !. 

best(D,B,M,[S1|T],S,U) :- move(B,S1,M,B2), inverse_mark(M,M2), !, minimax(D,B2,M2,_S,U1), best(D,B,M,T,S2,U2), output_value(D,S1,U1), better(D,M,S1,U1,S2,U2,S,U).

better(_,M,S1,U1,_2,U2, S,U) :- maximizing(M), U1 > U2, S = S1, U = U1, !. 
better(D,M,S1,U1,S2,U2, S,U) :- minimizing(M), U1 < U2, S = S1, U = U1, !. 
better(D,M,S1,U1,S2,U2, S,U) :- U1 == U2, random(1,10,R), better2(D,R,M,S1,U1,S2,U2,S,U), !. 
better(D,M,S1,U1,S2,U2, S,U) :- S = S2, U = U2, !. 

better2(D,R,M,S1,U1,S2,U2, S,U) :- R < 6, S = S1, U = U1, !. 
better2(D,R,M,S1,U1,S2,U2, S,U) :- S = S2, U = U2, !. 
*/


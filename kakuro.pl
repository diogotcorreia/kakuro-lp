:- [codigo_comum].

% combinacoes_soma/4
combinacoes_soma(N, Els, Soma, Combs) :-
  setof(X, (combinacao(N, Els, X), sum_list(X, Soma)), Combs).

% permutacoes_soma/4
permutacoes_soma(N, Els, Soma, Perms) :-
  combinacoes_soma(N, Els, Soma, Combs),
  !, % se o combinacoes_soma nao falhar, nao eh necessario voltar a tentar
  setof(X, Comb ^ (member(Comb, Combs), permutation(X, Comb)), Perms).

% soma_na_direcao/3
soma_na_direcao([_, S], h, S).
soma_na_direcao([S, _], v, S).

% espaco_fila/3
% entrar no predicado auxiliar espaco_fila/4
espaco_fila(L, espaco(N, EL), D) :-
  espaco_fila(L, espaco(N, EL), espaco(N, []), D).

% condicao de paragem: chegar ao final da lista
espaco_fila([], espaco(N, EL), espaco(N, EL)) :- 
  length(EL, Length),
  Length > 0.

% condicao de paragem: encontrar um elemento que nao eh variavel
espaco_fila([P | _], espaco(N, EL), espaco(N, EL)) :- 
  is_list(P),
  nonvar(N),
  length(EL, Length),
  Length > 0.

espaco_fila([P | R], espaco(N, L), espaco(N, RE)) :-
  \+is_list(P),
  append(RE, [P], Nova_RE),
  espaco_fila(R, espaco(N, L), espaco(N, Nova_RE)).

% espaco_fila/4
espaco_fila([P | R], espaco(N, L), espaco(N, []), D) :-
  is_list(P),
  soma_na_direcao(P, D, N),
  espaco_fila(R, espaco(N, L), espaco(N, [])).

espaco_fila([_ | R], espaco(N, L), espaco(N, []), D) :-
  espaco_fila(R, espaco(N, L), espaco(N, []), D).

% espacos_fila/3
espacos_fila(D, Fila, Espacos) :-
  bagof(X, espaco_fila(Fila, X, D), Espacos).

% espacos_puzzle/2
espacos_puzzle(Puzzle, Espacos) :-
  bagof(X, M ^ (member(M, Puzzle), espacos_fila(h, M, X)), Espacos_H),
  mat_transposta(Puzzle, Puzzle_T),
  bagof(Y, M ^ (member(M, Puzzle_T), espacos_fila(v, M, Y)), Espacos_V),
  append(Espacos_H, Espacos_V, Espacos_notflattened),
  flatten(Espacos_notflattened, Espacos).

% membro_igual/2
% Auxiliar - semelhante ao predicado member, mas nao unifica os elementos
membro_igual(X, [P | R]) :-
  X == P;
  membro_igual(X, R).

% disjuntas/2
% Auxiliar - verifica se duas listas sao dijuntas, isto eh, se nao tem nenhum
% elemento em comum
disjuntas([], _) :- !.
disjuntas(_, []) :- !.

disjuntas([P | _], L) :-
  membro_igual(P, L),
  !, fail.

disjuntas([_ | R], L) :- disjuntas(R, L).

% espaco_com_posicoes_comuns/2
% Auxiliar - verifica se dois espacos diferentes tem posicoes comuns
espaco_com_posicoes_comuns(espaco(N1, L1), espaco(N2, L2)) :-
  espaco(N1, L1) \== espaco(N2, L2),
  \+(disjuntas(L1, L2)).

% espacos_com_posicoes_comuns/3
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
  include(espaco_com_posicoes_comuns(Esp), Espacos, Esps_com).

% permutacoes_soma_espaco/2
% Auxiliar - utilizada no maplist do permutacoes_soma_espacos
permutacoes_soma_espaco(espaco(N, L), Perms_soma) :-
  length(L, Length_L),
  permutacoes_soma(Length_L, [1,2,3,4,5,6,7,8,9], N, Perms),
  Perms_soma = [espaco(N, L), Perms].

% permutacoes_soma_espacos/2
permutacoes_soma_espacos(Espacos, Perms_soma) :-
  maplist(permutacoes_soma_espaco, Espacos, Perms_soma).

% membro_espaco_perms_soma/2
% Auxiliar - verifica se um espaco com perms_soma esta na lista Esps_com
membro_espaco_perms_soma(Esps_com, [Esp, _]) :-
  membro_igual(Esp, Esps_com).

% permutacao_impossivel/4
% Auxiliar - tem sucesso se a permutacao for impossivel (invalida) entre
% dois espacos que teem posicoes em comum
permutacao_impossivel(espaco(_, [P1 | _]), Perm, N, [espaco(_, L2), Perms]) :- 
  membro_igual(P1, L2),
  append(Perms, L),
  nth0(N, Perm, Number),
  !,
  \+ member(Number, L).

permutacao_impossivel(espaco(EN1, [_ | R1]), Perm, N, [espaco(EN2, R2), P2]) :-
  Next_N is N + 1,
  permutacao_impossivel(espaco(EN1, R1), Perm, Next_N, [espaco(EN2, R2), P2]).

% permutacao_possivel_espaco/4
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
  espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
  include(membro_espaco_perms_soma(Esps_com), Perms_soma, Perms_soma_com),
  permutacoes_soma_espaco(Esp, [_, Esp_perms]),
  !,
  member(Perm, Esp_perms),
  include(permutacao_impossivel(Esp, Perm, 0), Perms_soma_com, L),
  length(L, Length_L),
  Length_L == 0.

% permutacoes_possiveis_espaco/4
permutacoes_possiveis_espaco(Espacos, Perms_soma, espaco(N, Pos), Perms_poss) :-
  bagof(X, permutacao_possivel_espaco(X, espaco(N, Pos), Espacos, Perms_soma), Perms),
  Perms_poss = [Pos, Perms].

% permutacoes_possiveis_espacos/2
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
  permutacoes_soma_espacos(Espacos, Perms_soma),
  bagof(X, Esp ^ (member(Esp, Espacos), permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, X)), Perms_poss_esps).

% numeros_comuns/2
numeros_comuns(Lst_Perms, Numeros_comuns) :-
  numeros_comuns(Lst_Perms, 1, Numeros_comuns).

% numeros_comuns/3
% Auxiliar - processo iterativo de numeros_comuns/2
numeros_comuns([P | _], N, []) :-
  Prev_N is N - 1,
  length(P, Prev_N),
  !.

numeros_comuns(Lst_Perms, N, [(N, Numero) | Numeros_comuns]) :-
  maplist(nth1(N), Lst_Perms, Lst_Numeros),
  list_to_set(Lst_Numeros, Set_Numeros),
  length(Set_Numeros, Length),
  Length == 1,
  !,
  Next_N is N + 1,
  nth1(1, Lst_Numeros, Numero),
  numeros_comuns(Lst_Perms, Next_N, Numeros_comuns).

numeros_comuns(Lst_Perms, N, Numeros_comuns) :-
  Next_N is N + 1,
  numeros_comuns(Lst_Perms, Next_N, Numeros_comuns).

% atribui_comuns_espacos/1
% Auxiliar - encontrar os numeros comuns e de seguida unifica-los
atribui_comuns_espacos([Espaco, Perms]) :-
  numeros_comuns(Perms, Numeros_comuns),
  atribui_comuns_espacos(Espaco, Numeros_comuns).

% atribui_comuns_espacos/2
% Auxiliar - efetuar recursao e unificar os numeros comuns
atribui_comuns_espacos(_, []).

atribui_comuns_espacos(Espaco, [(N, Numero) | R]) :-
  nth1(N, Espaco, Numero),
  atribui_comuns_espacos(Espaco, R).

% atribui_comuns/1
atribui_comuns(Perms_Possiveis) :-
  maplist(atribui_comuns_espacos, Perms_Possiveis).

% permutacao_espaco_possivel/2
% Auxiliar - verifica se uma permutacao eh possivel para um determinado espaco
permutacao_espaco_possivel([], []).

permutacao_espaco_possivel([P1 | R1], [_ | R2]) :-
  var(P1),
  permutacao_espaco_possivel(R1, R2).

permutacao_espaco_possivel([P1 | R1], [P2 | R2]) :-
  nonvar(P1),
  P1 == P2,
  permutacao_espaco_possivel(R1, R2).

% retira_impossiveis_espaco/2
% Auxiliar - retira impossiveis de um unico espaco
retira_impossiveis_espaco([Espaco, Perms], [Espaco, Novas_Perms]) :-
  include(permutacao_espaco_possivel(Espaco), Perms, Novas_Perms).

% retira_impossiveis/2
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
  maplist(retira_impossiveis_espaco, Perms_Possiveis, Novas_Perms_Possiveis).

% simplifica/2
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
  atribui_comuns(Perms_Possiveis),
  retira_impossiveis(Perms_Possiveis, P),
  Perms_Possiveis \== P,
  simplifica(P, Novas_Perms_Possiveis).

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
  atribui_comuns(Perms_Possiveis),
  retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis),
  Perms_Possiveis == Novas_Perms_Possiveis.

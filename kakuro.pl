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

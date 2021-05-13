% Diogo Torres Correia, ist199211

:- [codigo_comum].

% ------------------------------------------------------------------------------
% combinacoes_soma(N, Els, Soma, Combs)
% Combs eh a lista ordenada de combinacoes N a N dos elementos de Els cuja
% soma eh Soma
% ------------------------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs) :-
  setof(X, (combinacao(N, Els, X), sum_list(X, Soma)), Combs).

% ------------------------------------------------------------------------------
% permutacoes_soma(N, Els, Soma, Perms)
% Perms eh a lista ordenada de permutacoes N a N dos elementos de Els cuja
% soma eh Soma
% ------------------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms) :-
  combinacoes_soma(N, Els, Soma, Combs),
  !, % se o combinacoes_soma nao falhar, nao eh necessario voltar a tentar
  setof(X, Comb ^ (member(Comb, Combs), permutation(X, Comb)), Perms).

% ------------------------------------------------------------------------------
% soma_na_direcao(L, H_V, N)
% N eh o elemento da lista L que corresponde ah direcao H_V
% Quando a direcao eh h, eh o segundo elemento da lista
% Quando a direcao eh v, eh o primeiro elemento da lista
% ------------------------------------------------------------------------------
soma_na_direcao([_, S], h, S).
soma_na_direcao([S, _], v, S).

% ------------------------------------------------------------------------------
% espaco_fila(Fila, Esp, H_V)
% Espaco eh um espaco da fila Fila na direcao H_V
% ------------------------------------------------------------------------------
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

% ------------------------------------------------------------------------------
% espacos_fila(H_V, Fila, Espacos)
% Espacos eh a lista de espacos da fila Fila na direcao H_V
% ------------------------------------------------------------------------------
espacos_fila(D, Fila, Espacos) :-
  bagof(X, espaco_fila(Fila, X, D), Espacos).

% ------------------------------------------------------------------------------
% espacos_puzzle(Puzzle, Espacos)
% Espacos eh a lista de espacos do puzzle Puzzle
% ------------------------------------------------------------------------------
espacos_puzzle(Puzzle, Espacos) :-
  bagof(X, M ^ (member(M, Puzzle), espacos_fila(h, M, X)), Espacos_H),
  mat_transposta(Puzzle, Puzzle_T),
  bagof(Y, M ^ (member(M, Puzzle_T), espacos_fila(v, M, Y)), Espacos_V),
  append(Espacos_H, Espacos_V, Espacos_notflattened),
  flatten(Espacos_notflattened, Espacos).

% ------------------------------------------------------------------------------
% membro_igual(El, Lista)
% Auxiliar - semelhante ao predicado member, mas nao unifica os elementos
% El eh um elemento da lista Lista, pela comparacao usando ==
% ------------------------------------------------------------------------------

membro_igual(X, [P | R]) :-
  X == P;
  membro_igual(X, R).

% ------------------------------------------------------------------------------
% disjuntas(L1, L2)
% Auxiliar - verifica se duas listas sao disjuntas
% L1 eh uma lista que nao tem nenhum elemento em comum com L2,
% sem unificar elementos
% ------------------------------------------------------------------------------
disjuntas([], _) :- !.
disjuntas(_, []) :- !.

disjuntas([P | _], L) :-
  membro_igual(P, L),
  !, fail.

disjuntas([_ | R], L) :- disjuntas(R, L).

% ------------------------------------------------------------------------------
% espaco_com_posicoes_comuns(Espaco1, Espaco2)
% Auxiliar - verifica se dois espacos diferentes tem posicoes comuns
% Espaco1 tem posicoes comuns com Espaco 2
% ------------------------------------------------------------------------------
espaco_com_posicoes_comuns(espaco(N1, L1), espaco(N2, L2)) :-
  espaco(N1, L1) \== espaco(N2, L2),
  \+(disjuntas(L1, L2)).

% ------------------------------------------------------------------------------
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% Esps_com eh uma sublista de Espacos que contem apenas os espacos
% que teem variaveis comuns com o espaco Esp, excluindo Esp
% ------------------------------------------------------------------------------
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
  include(espaco_com_posicoes_comuns(Esp), Espacos, Esps_com).

% ------------------------------------------------------------------------------
% permutacoes_soma_espaco(Espaco, Perms_soma)
% Auxiliar - utilizada no maplist do permutacoes_soma_espacos
% Perms_soma eh a lista de permutacoes possiveis para o espaco Espaco
% ------------------------------------------------------------------------------
permutacoes_soma_espaco(espaco(N, L), Perms_soma) :-
  length(L, Length_L),
  permutacoes_soma(Length_L, [1, 2, 3, 4, 5, 6, 7, 8, 9], N, Perms),
  Perms_soma = [espaco(N, L), Perms].

% ------------------------------------------------------------------------------
% permutacoes_soma_espacos(Espacos, Perms_soma)
% Perms_soma eh lista de listas de 2 elementos cujo primeiro eh o espaco
% e o segundo a lista de permutacoes possiveis nesse espaco
% ------------------------------------------------------------------------------
permutacoes_soma_espacos(Espacos, Perms_soma) :-
  maplist(permutacoes_soma_espaco, Espacos, Perms_soma).

% ------------------------------------------------------------------------------
% membro_espaco_perms_soma(Esps_com, Esp)
% Auxiliar - verifica se um espaco com perms_soma esta na lista Esps_com
% Esp eh um espaco que esta na lista de espacos comuns Esps_com,
% sem unificar variaveis
% ------------------------------------------------------------------------------
membro_espaco_perms_soma(Esps_com, [Esp, _]) :-
  membro_igual(Esp, Esps_com).

% ------------------------------------------------------------------------------
% permutacao_impossivel(Esp, Perm, N, Perms_soma_com)
% Auxiliar - tem sucesso se a permutacao for impossivel (invalida) entre
% dois espacos que teem posicoes em comum
% Perm nao eh valida para o espaco dado Esp, e para a lista de permutacoes
% comuns Perms_soma_com
% ------------------------------------------------------------------------------
permutacao_impossivel(espaco(_, [P1 | _]), Perm, N, [espaco(_, L2), Perms]) :- 
  membro_igual(P1, L2),
  append(Perms, L),
  nth0(N, Perm, Number),
  !,
  \+ member(Number, L).

permutacao_impossivel(espaco(EN1, [_ | R1]), Perm, N, [espaco(EN2, R2), P2]) :-
  Next_N is N + 1,
  permutacao_impossivel(espaco(EN1, R1), Perm, Next_N, [espaco(EN2, R2), P2]).

% ------------------------------------------------------------------------------
% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% Perms eh uma permutacao possivel para o espaco Esp
% ------------------------------------------------------------------------------
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
  espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
  include(membro_espaco_perms_soma(Esps_com), Perms_soma, Perms_soma_com),
  permutacoes_soma_espaco(Esp, [_, Esp_perms]),
  !,
  member(Perm, Esp_perms),
  include(permutacao_impossivel(Esp, Perm, 0), Perms_soma_com, L),
  length(L, Length_L),
  Length_L == 0.

% ------------------------------------------------------------------------------
% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% Perms_poss eh uma lista de dois elementos com uma lista das variaveis
% do espaco Esp e com a lista de permutacoes possiveis para este espaco
% ------------------------------------------------------------------------------
permutacoes_possiveis_espaco(Espacos, Perms_soma, espaco(N, Pos), Perms_poss) :-
  bagof(
    X,
    permutacao_possivel_espaco(X, espaco(N, Pos), Espacos, Perms_soma),
    Perms
  ),
  Perms_poss = [Pos, Perms].

% ------------------------------------------------------------------------------
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Perms_poss_esps eh a lista de permutacoes possiveis para os espacos Espacos
% ------------------------------------------------------------------------------
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
  permutacoes_soma_espacos(Espacos, Perms_soma),
  bagof(X, Esp ^ (
      member(Esp, Espacos), 
      permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, X)
    ), Perms_poss_esps).

% ------------------------------------------------------------------------------
% numeros_comuns(Lst_Perms, Numeros_comuns)
% Numeros_comuns eh a lista de pares (pos, numero), em que todas as listas
% de Lst_Perms teem numero na posicao pos
% ------------------------------------------------------------------------------
numeros_comuns([], []) :- !, fail.

numeros_comuns(Lst_Perms, Numeros_comuns) :-
  numeros_comuns(Lst_Perms, 1, Numeros_comuns).

% ------------------------------------------------------------------------------
% numeros_comuns(Lst_Perms, N, Numeros_comuns)
% Auxiliar - processo iterativo de numeros_comuns/2
% ------------------------------------------------------------------------------
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

% ------------------------------------------------------------------------------
% atribui_comuns_espacos(Perms_Possiveis)
% Auxiliar - encontrar os numeros comuns e de seguida unifica-los
% ------------------------------------------------------------------------------
atribui_comuns_espacos([Espaco, Perms]) :-
  numeros_comuns(Perms, Numeros_comuns),
  atribui_comuns_espacos(Espaco, Numeros_comuns).

% ------------------------------------------------------------------------------
% atribui_comuns_espacos(Espaco, Numeros_comuns)
% Auxiliar - efetuar recursao e unificar os numeros comuns
% ------------------------------------------------------------------------------
atribui_comuns_espacos(_, []).

atribui_comuns_espacos(Espaco, [(N, Numero) | R]) :-
  nth1(N, Espaco, Numero),
  atribui_comuns_espacos(Espaco, R).

% ------------------------------------------------------------------------------
% atribui_comuns(Perms_Possiveis)
% Unifica as variaveis dos espacos das permutacoes possiveis quando estas
% teem numeros comuns
% ------------------------------------------------------------------------------
atribui_comuns(Perms_Possiveis) :-
  maplist(atribui_comuns_espacos, Perms_Possiveis).

% ------------------------------------------------------------------------------
% permutacao_espaco_possivel(Perms1, Perms2)
% Auxiliar - verifica se uma permutacao eh possivel para um determinado espaco
% Tanto Perms1 como Perms2 sao permutacoes compativeis, isto eh,
% nao coincidem em nenhum numero
% ------------------------------------------------------------------------------
permutacao_espaco_possivel([], []).

permutacao_espaco_possivel([P1 | R1], [_ | R2]) :-
  var(P1),
  permutacao_espaco_possivel(R1, R2).

permutacao_espaco_possivel([P1 | R1], [P2 | R2]) :-
  nonvar(P1),
  P1 == P2,
  permutacao_espaco_possivel(R1, R2).

% ------------------------------------------------------------------------------
% retira_impossiveis_espaco(Perms_Possiveis, Novas_Perms_Possiveis)
% Auxiliar - retira impossiveis de um unico espaco
% Novas_Perms_Possiveis eh a lista de permutacoes que apenas inclui as permutacoes
% possiveis de Perms_Possiveis
% ------------------------------------------------------------------------------
retira_impossiveis_espaco([Espaco, Perms], [Espaco, Novas_Perms]) :-
  include(permutacao_espaco_possivel(Espaco), Perms, Novas_Perms).

% ------------------------------------------------------------------------------
% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% Novas_Perms_Possiveis eh o resultado de retirar permutacoes impossiveis
% de Perms_Possiveis
% ------------------------------------------------------------------------------
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
  maplist(retira_impossiveis_espaco, Perms_Possiveis, Novas_Perms_Possiveis).

% ------------------------------------------------------------------------------
% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Novas_Perms_Possiveis eh o resultado de aplicar atribui_comuns e
% retira_impossiveis a Perms_Possiveis ate nao haverem mais alteracoes
% ------------------------------------------------------------------------------
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
  atribui_comuns(Perms_Possiveis),
  retira_impossiveis(Perms_Possiveis, P),
  Perms_Possiveis \== P,
  simplifica(P, Novas_Perms_Possiveis).

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
  atribui_comuns(Perms_Possiveis),
  retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis),
  Perms_Possiveis == Novas_Perms_Possiveis.

% ------------------------------------------------------------------------------
% inicializa(Puzzle, Perms_Possiveis)
% Perms_Possiveis eh a lista de permutacoes simplificada para Puzzle
% ------------------------------------------------------------------------------
inicializa(Puzzle, Perms_Possiveis) :-
  espacos_puzzle(Puzzle, Espacos),
  permutacoes_possiveis_espacos(Espacos, Perms_P),
  simplifica(Perms_P, Perms_Possiveis).

% ------------------------------------------------------------------------------
% mais_que_uma_permutacao(Perm_Possivel)
% Auxiliar - predicado usado para filtrar espacos que teem mais que uma
% permutacao
% Perm_Possivel tem mais que uma permutacao possivel
% ------------------------------------------------------------------------------
mais_que_uma_permutacao([_, L]) :-
  length(L, N),
  N > 1.

% ------------------------------------------------------------------------------
% permutacao_tem_tamanho_menor(Perms_Possiveis, Escolha)
% Auxiliar - verificar se um espaco tem o menor tamanho da lista
% Escolha eh o elemento de Perms_Possiveis que tem uma lista de permutacoes
% com o menor tamanho
% ------------------------------------------------------------------------------
permutacao_tem_tamanho_menor([], _).

permutacao_tem_tamanho_menor([[_, P] | R ], [N, Permutacoes]) :-
  length(P, Length_P),
  length(Permutacoes, Length_Permutacoes),
  Length_P >= Length_Permutacoes,
  permutacao_tem_tamanho_menor(R, [N, Permutacoes]).

% ------------------------------------------------------------------------------
% escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% Escolha eh o elemento de Perms_Possiveis com lista de permutacoes com mais
% que uma permutacao e menor numero de permutacoes entre as que teem
% mais que uma
% ------------------------------------------------------------------------------
escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
  include(mais_que_uma_permutacao, Perms_Possiveis, L),
  length(L, N),
  N > 0,
  !,
  member(Escolha, L),
  permutacao_tem_tamanho_menor(L, Escolha).

% ------------------------------------------------------------------------------
% substituir_espaco(Perm, Perm_Possivel, Escolha)
% Auxiliar - usada no maplist para substituir em Perms_Possiveis, o elemento
% Escolha pelo elemento [Esp, [Perm]].
% Escolha eh uma lista de listas que contem o espaco de Perm e a sua lista
% de permutacoes se o espaco for igual ao espaco de Perm_Possivel, ou a
% lista de permutacoes de Perm_Possivel caso contrario.
% ------------------------------------------------------------------------------
substituir_espaco([Esp, Lst_Perms], [Esp, _], Escolha) :-
  Escolha = [Esp, Lst_Perms].

substituir_espaco([Esp, _], [Esp2, Lst_Perms], Escolha) :-
  Esp \== Esp2,
  Escolha = [Esp2, Lst_Perms].

% ------------------------------------------------------------------------------
% experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
% Novas_Perms_Possiveis eh o resultado de substituir em Perms_Possiveis
% o espaco correspondente ao espaco de Escolha por Escolha, unificando
% o espaco de Escolha com uma permutacao de Escolha
% ------------------------------------------------------------------------------
experimenta_perm([Esp, Lst_Perms], Perms_Possiveis, Novas_Perms_Possiveis) :-
  member(Perm, Lst_Perms),
  Esp = Perm,
  maplist(
    substituir_espaco([Esp, [Perm]]),
    Perms_Possiveis,
    Novas_Perms_Possiveis
  ).

% ------------------------------------------------------------------------------
% permutacao_terminada(Perm_possivel)
% Auxiliar - eh verdadeiro se a lista Perms tiver tamanho 1
% A lista de permutacoes de Perm_possivel tem tamanho 1
% ------------------------------------------------------------------------------
permutacao_terminada([_, Perms]) :- length(Perms, 1).

% ------------------------------------------------------------------------------
% resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% Novas_Perms_Possiveis eh o resultado de resolver o puzzle representado
% por Perms_Possiveis
% ------------------------------------------------------------------------------
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
  escolhe_menos_alternativas(Perms_Possiveis, Escolha),
  experimenta_perm(Escolha, Perms_Possiveis, N_Perms_Possiveis),
  simplifica(N_Perms_Possiveis, N2_Perms_Possiveis),
  \+ maplist(permutacao_terminada, N2_Perms_Possiveis),
  resolve_aux(N2_Perms_Possiveis, Novas_Perms_Possiveis).

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
  escolhe_menos_alternativas(Perms_Possiveis, Escolha),
  experimenta_perm(Escolha, Perms_Possiveis, N_Perms_Possiveis),
  simplifica(N_Perms_Possiveis, Novas_Perms_Possiveis),
  maplist(permutacao_terminada, Novas_Perms_Possiveis).

% ------------------------------------------------------------------------------
% resolve(Puzzle)
% Unifica as variaveis em Puzzle de forma a inicializar e a resolver o Puzzle
% ------------------------------------------------------------------------------
resolve(Puzzle) :-
  inicializa(Puzzle, Perms_Possiveis),
  resolve_aux(Perms_Possiveis, _).

:- [codigo_comum].

combinacoes_soma(N, Els, Soma, Combs) :-
  setof(X, (combinacao(N, Els, X), sum_list(X, Soma)), Combs).

permutacoes_soma(N, Els, Soma, Perms) :-
  combinacoes_soma(N, Els, Soma, Combs),
  !,
  % TODO why doesn't setof work here?
  findall(X, (member(Comb, Combs), permutation(X, Comb)), Perms_Unsorted),
  sort(Perms_Unsorted, Perms).

% Transpose function
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% KenKen fd constraints and rules
fd_row_constraint(N, L) :-
    length(L, N), % make sure length of L matches N
    fd_domain(L, 1, N), % make sure each element in L is a number 1 to N
    fd_all_different(L). % make sure each element in L is different from each other

fd_sum(_, 0, []).
fd_sum(T, S, [[ROW|COL]|TAIL]):-
    nth(ROW, T, L), % check if Row'th argument of T is L
    nth(COL, L, Cval), % check if Col'th argument of L is X
    fd_sum(T, TSum, TAIL),
    S #= Cval+TSum.

fd_prod(_, 1, []).
fd_prod(T, P, [[ROW|COL]|TAIL]) :-
    nth(ROW, T, L),
    nth(COL, L, Cval),
    fd_prod(T, TProd, TAIL),
    P #= Cval*TProd. 

fd_sub(T, D, [ROW|COL], [ROW2|COL2]) :-
    nth(ROW, T, L),
    nth(COL, L, Cval),
    nth(ROW2, T, R2val),
    nth(COL2, R2val, C2val),
    (D #= Cval-C2val; D #= C2val-Cval). % D is X-XX or XX-X

fd_div(T, Q, [ROW|COL], [ROW2|COL2]):-
    nth(ROW, T, L),
    nth(COL, L, Cval),
    nth(ROW2, T, R2val),
    nth(COL2, R2val, C2val),
    (Cval #= Q*C2val; C2val #= Q*Cval). % X is Q*XX or XX is Q*X

fd_cage(T, +(S, L)):-
    fd_sum(T, S, L).
fd_cage(T, *(P, L)):-
    fd_prod(T, P, L).
fd_cage(T, -(D, J, K)):-
    fd_sub(T, D, J, K).
fd_cage(T, /(Q, J, K)):-
    fd_div(T, Q, J, K).

kenken(N, C, T):-
    length(T, N),
    maplist(fd_row_constraint(N), T), % row constraints
    transpose(T, TRANS), % transpose to check columns
    maplist(fd_row_constraint(N), TRANS), % column constraints
    maplist(fd_cage(T), C), % apply fd cage constraints
    maplist(fd_labeling, T).


% Plain KenKen constraints and rules
plain_row_constraint(N, L) :-
    findall(C, between(1, N, C), Hz), !,
    permutation(Hz, L).

plain_sum(_, 0, []).
plain_sum(T, S, [[ROW|COL]| TAIL]):-
    nth(ROW, T, L),
    nth(COL, L, Cval),
    plain_sum(T, Tsum, TAIL),
    S is Cval+Tsum.

plain_prod(_, 1, []).
plain_prod(T, P, [[ROW|COL]|TAIL]):-
    nth(ROW, T, L),
    nth(COL, L, Cval),
    plain_prod(T, Tprod, TAIL),
    P is Cval*Tprod.

plain_sub(T, D, [ROW|COL], [ROW2|COL2]):-
    nth(ROW, T, L),
    nth(COL, L, Cval),
    nth(ROW2, T, R2val),
    nth(COL2, R2val, C2val),
    (D is Cval-C2val; D is C2val-Cval).

plain_div(T, Q, [ROW|COL], [ROW2|COL2]):-
    nth(ROW, T, L),
    nth(COL, L, Cval),
    nth(ROW2, T, R2val),
    nth(COL2, R2val, C2val),
    (Cval is Q*C2val; C2val is Q*Cval).

plain_cage(T, +(S, L)) :-
    plain_sum(T, S, L).
plain_cage(T, *(P, L)) :-
    plain_prod(T, P, L).
plain_cage(T, -(D, J, K)) :-
    plain_sub(T, D, J, K).
plain_cage(T, /(Q, J, K)) :-
    plain_div(T, Q, J, K).


plain_kenken(N, C, T) :-
    length(T,N),
    maplist(plain_row_constraint(N), T),
    transpose(T, TRANS),
    maplist(plain_row_constraint(N), TRANS),
    maplist(plain_cage(T), C).

% test case in spec
kenken_testcase(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).

% second test case in spec (used for stats measurement)
kenken_testcase1(
  4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ]
).



%%%%%%%%%%%%%% Performance Analysis %%%%%%%%%%%%%

% Measuring kenken performance
% ?- statistics,
%    fd_set_vector_max(255), kenken_testcase1(N,C), kenken(N,C,T),
%    statistics.

% Memory               limit         in use            free

%  trail  stack      16383 Kb            4 Kb        16379 Kb
%  cstr   stack      16383 Kb            9 Kb        16374 Kb
%  global stack      32767 Kb            6 Kb        32761 Kb
%  local  stack      16383 Kb            4 Kb        16379 Kb
%  atom   table      32768 atoms      1801 atoms     30967 atoms

%  Times              since start      since last

%   user   time       0.007 sec       0.001 sec
%   system time       0.005 sec       0.000 sec
%   cpu    time       0.012 sec       0.001 sec
%   real   time      92.948 sec       0.001 sec


 

% Measuring plain_kenken performance
% ?- statistics,
%    fd_set_vector_max(255), kenken_testcase1(N,C), plain_kenken(N,C,T),
%    statistics.

% Memory               limit         in use            free

%  trail  stack      16383 Kb            0 Kb        16383 Kb
%  cstr   stack      16384 Kb            0 Kb        16384 Kb
%  global stack      32767 Kb            7 Kb        32760 Kb
%  local  stack      16383 Kb            9 Kb        16374 Kb
%  atom   table      32768 atoms      1799 atoms     30969 atoms

% Times              since start      since last

%  user   time       0.055 sec       0.050 sec
%  system time       0.003 sec       0.000 sec
%  cpu    time       0.058 sec       0.050 sec
%  real   time       8.644 sec       0.050 sec

% kenken used 1 ms of cpu time and real time while plain_kenken used 50ms when solving 
% for the first possible solution in the given ambiguous test case. This reveals that 
% plain_kenken has a much slower performance compared to fd kenken.



%%%%%%%%%%%%%% No-op Kenken %%%%%%%%%%%%%

% No-op kenken runs through all possible permutations of operations for each cage, which
% is the main difference from regular kenken we created above. 

% noop_kenken(N, C, T, O)

% where inputs are:
% N:  a nonnegative integer specifying the number of cells on each side of the KenKen square.
% C:  a list of numeric cage constraints as described in the spec.
% T:  a list of list of integers. T and its members all have length N. This represents the N×N grid.
% O:  a list of list of possible operations. Each list contains a list of operation(s) that can be performed 
%     to satisfy the constraint in its corresponding position.

% noop_kenken_testcase(
%   4,
%   [
%    (24, [[1|1], [2|1], [3|1]]),
%    (12, [[1|2], [1|3]]),
%    (3, [[1|4], [2|4]]),
%    (1, [[2|2], [3|2]),
%    (4, [[2|3]]),
%    (4, [[4|1], [4|2]]),
%    (1, [[3|3], [4|3]]),
%    (7, [[3|4], [4|4]])
%   ], T, O).

% output of running noop_kenken on this test case is:
% N = 4

% C = [24, [[1|1], [2|1], [3|1]]],
%     [12, [[1|2], [1|3]]],
%     [3, [[1|4], [2|4]]],
%     [1, [[2|2], [3|2]],
%     [4, [[2|3]]],
%     [4, [[4|1], [4|2]]],
%     [1, [[3|3], [4|3]]],
%     [7, [[3|4], [4|4]]]

% T = [[2, 4, 3, 1],
%     [3, 1, 4, 2],
%     [4, 2, 1, 3],
%     [1, 3, 2, 4]]

% O = [[*],
%      [*],
%      [+],
%      [-],
%      [],
%      [+],
%      [-],
%      [+]]












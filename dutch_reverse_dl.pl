dutch(L, RWB) :-
    distribute(L, R, W, B),
    append(W, B, WB),
    append(R, WB, RWB).

distribute([], [], [], []).
distribute([r(H)|T], [r(H)|R], W, B) :- distribute(T, R, W, B).
distribute([w(H)|T], R, [w(H)|W], B) :- distribute(T, R, W, B).
distribute([b(H)|T], R, W, [b(H)|B]) :- distribute(T, R, W, B).

dutch_dl(L, RWB-[]) :-
    distr_dl(L, RWB-WB, WB-B, B-[]).

distr_dl([r(H)|T], [r(H)|R]-R1, W, B) :-distr_dl(T, R-R1, W, B).
distr_dl([w(H)|T], R, [w(H)|W]-W1, B) :-distr_dl(T, R, W-W1, B).
distr_dl([b(H)|T], R, W, [b(H)|B]-B1) :-distr_dl(T, R, W, B-B1).
distr_dl([], R-R, W-W, B-B).

dutch_rev_flag(L, BWR-[]) :-
    distr_re(L, BWR-WR, WR-R, R-[]).

distr_re([b(H)|T], [b(H)|B]-B1, W, R) :-distr_re(T, B-B1, W, R).
distr_re([w(H)|T], B, [w(H)|W]-W1, R) :-distr_re(T, B, W-W1, R).
distr_re([r(H)|T], B, W, [r(H)|R]-R1) :-distr_re(T, B, W, R-R1).

distr_re([], B-B, W-W, R-R).

dutch_dl_re(L, RWB-[]) :-
    distr_dl_re(L, RWB-WB, WB-B, B-[]).

distr_dl_re([r(H)|T], R-R1, W, B) :-distr_dl_re(T, R-[r(H)|R1], W, B).
distr_dl_re([w(H)|T], R, W-W1, B) :-distr_dl_re(T, R, W-[w(H)|W1], B).
distr_dl_re([b(H)|T], R, W, B-B1) :-distr_dl_re(T, R, W, B-[b(H)|B1]).
distr_dl_re([], R-R, W-W, B-B).


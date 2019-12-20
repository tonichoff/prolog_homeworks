color(house(C, _, _, _, _), C).

nation(house(_, N, _, _, _), N).

pet(house(_, _, P, _, _), P).

drink(house(_, _, _, D, _), D).

tobacco(house(_, _, _, _, T), T).

first(X, [X, _, _, _, _]).

middle(X, [_, _, X, _, _]).

neighbour(X, Y, [X, Y, _, _, _]).
neighbour(X, Y, [_, X, Y, _, _]).
neighbour(X, Y, [Y, X, _, _, _]).
neighbour(X, Y, [_, _, X, Y, _]).
neighbour(X, Y, [_, Y, X, _, _]).
neighbour(X, Y, [_, _, _, X, Y]).
neighbour(X, Y, [_, _, Y, X, _]).
neighbour(X, Y, [_, _, _, Y, X]).

right(X, Y, [Y, X, _, _, _]).
right(X, Y, [_, Y, X, _, _]).
right(X, Y, [_, _, Y, X, _]).
right(X, Y, [_, _, _, Y, X]).

member_(H, [H|_]).
member_(X, [_|T]):-member_(X, T).

solve_([H|T]):-H, solve_(T).
solve_([]).

solve_puzzle(puzzle(Clues, Queries, Solution), Solution):-
    solve_(Clues),
    solve_(Queries).

queries(Houses, [member_(Q1, Houses),
                 drink(Q1, water),
                 nation(Q1, N1),
                 member_(Q2, Houses),
                 pet(Q2, zebra),
                 nation(Q2, N2)],
                [['Drink water', N1],
                 ['Pet zebra', N2]]).

clues(Houses, [nation(H1, englishman),
               color(H1, red),
               member_(H1, Houses),
               nation(H2, hispanic),
               pet(H2, dog),
               member_(H2, Houses),
               color(H3, green),
               drink(H3, coffee),
               member_(H3, Houses),
               nation(H4, ukrainian),
               drink(H4, tea),
               member_(H4, Houses),
               right(H5G, H5W, Houses),
               color(H5G, green),
               color(H5W, white),
               tobacco(H6, old_gold),
               pet(H6, snails),
               member_(H6, Houses),
               color(H7, yellow),
               tobacco(H7, kool),
               member_(H7, Houses),
               middle(H8, Houses),
               drink(H8, milk),
               nation(H9, norwegian),
               first(H9, Houses),
               neighbour(H10, H10N, Houses),
               pet(H10, fox),
               tobacco(H10N, chesterfield),
               neighbour(H11, H11N, Houses),
               pet(H11, horse),
               tobacco(H11N, kool),
               tobacco(H12, lucky_strike),
               drink(H12, juice),
               member_(H12, Houses),
               nation(H13, japanese),
               tobacco(H13, parliament),
               member_(H13, Houses),
               nation(H14, norwegian),
               neighbour(H14, H14N, Houses),
               color(H14N, blue)]).

houses([house(_, _, _, _, _),
        house(_, _, _, _, _),
        house(_, _, _, _, _),
        house(_, _, _, _, _),
        house(_, _, _, _, _)]).

init_puzzle(puzzle(Clues, Queries, Solution)):-
    houses(Structure),
    clues(Structure, Clues),
    queries(Structure, Queries, Solution).

find_houses(S):- init_puzzle(puzzle(C, Q, S)), solve_puzzle(puzzle(C, Q, S), S).

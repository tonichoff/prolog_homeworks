name_(friend(X, _, _), X).
country(friend(_, Y, _), Y).
sport(friend(_, _, Z), Z).
better(X, Y, [X, Y, _]).
better(X, Y, [X, _, Y]).
better(X, Y, [_, X, Y]).
first(X, [X, _, _]).

friends(test, [friend(_,_,_),
               friend(_,_,_),
               friend(_,_,_)]).

clues(test, Friends, [better(F1C1, F2C1, Friends),
                      name_(F1C1, mike),
                      sport(F1C1, basketball),
                      country(F2C1, usa),
                      better(F1C2, F2C2, Friends),
                      name_(F1C2, lee),
                      country(F1C2, korea),
                      sport(F2C2, tennis),
                      first(F1C3, Friends),
                      sport(F1C3, cricket)]).

queries(test, Friends, [member_(Q1, Friends),
                        country(Q1, australia),
                        name_(Q1, Name),
                        member_(Q2, Friends),
                        name_(Q2, richard),
                        sport(Q2, Sport)],
                       [['The australian is ', Name],
                        ['Richard likes ', Sport]]).

member_(H, [H|_]).
member_(X, [_|T]):-member_(X, T).

init_puzzle(PuzzleName, puzzle(Clues, Queries, Solution)):-
    friends(PuzzleName, Friends),
    clues(PuzzleName, Friends, Clues),
    queries(PuzzleName, Friends, Queries, Solution).

solve_puzzle(puzzle(Clues, Queries, Solution), Solution):-solve_(Clues), solve_(Queries).

solve_([H|T]):-H, solve_(T).
solve_([]).

find_friends(Solution):-
    init_puzzle(test, puzzle(Clues, Queries, Solution)),
    solve_puzzle(puzzle(Clues, Queries, Solution), Solution).























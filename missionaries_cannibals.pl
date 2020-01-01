:- use_module(library(clpfd)).

legal_coast(coast(MisCount, CanCount)) :-
    MisCount >= 0,
    MisCount =< 3,
    CanCount >= 0,
    CanCount =< 3,
    ((MisCount >= CanCount, MisCount > 0); MisCount = 0).

legal(state(_, Left, Right)) :- legal_coast(Left), legal_coast(Right).

update_people_helper(
    move(MisMoved, CanMoved),
    coast(MisLeft, CanLeft),
    coast(MisRight, CanRight),
    coast(MisNewLeft, CanNewLeft),
    coast(MisNewRight, CanNewRight)) :-
    MisNewLeft is MisLeft -  MisMoved,
    CanNewLeft is CanLeft - CanMoved,
    MisNewRight is MisRight + MisMoved,
    CanNewRight is CanRight + CanMoved.

update_people(Move, left, Left, Right, NewLeft, NewRight) :-
    update_people_helper(Move, Left, Right, NewLeft, NewRight).
update_people(Move, right, Left, Right, NewLeft, NewRight) :-
    update_people_helper(Move, Right, Left, NewRight, NewLeft).

update_coast(left, right).
update_coast(right, left).

update(state(Coast, Left, Right), Move, state(NewCoast, NewLeft, NewRight)) :-
    update_coast(Coast, NewCoast),
    update_people(Move, Coast, Left, Right, NewLeft, NewRight).

find_move(_, _, move(1, 0)).
find_move(_, _, move(1, 1)).
find_move(_, _, move(0, 1)).
find_move(_, _, move(2, 0)).
find_move(_, _, move(0, 2)).

move(state(left, Left, Right), Move) :- find_move(Left, Right, Move).
move(state(right, Left, Right), Move) :- find_move(Right, Left, Move).

value(state(_, _, coast(MisCount, CanCount)), Value) :-  Value is MisCount + CanCount.

final_state(state(right, coast(0, 0), coast(3, 3))).

initial_state(state, state(left, coast(3, 3), coast(0, 0))).

/*  Hill climbing framework for problem solving*/

/*
    solve_hill_climb(State,History,Moves) :-
    Moves is the sequence of moves to reach a desired final state
    from the current State, where History are the states
    visited previously.
*/

solve_hill_climb(State, _, []) :-
    final_state(State).

solve_hill_climb(State, History, [Move | Moves]) :-
    hill_climb(State, Move),
    update(State, Move, State1),
    legal(State1),
    not(member(State1, History)),
    solve_hill_climb(State1, [State1 | History], Moves).

hill_climb(State, Move) :-
    findall(M, move(State, M), Moves),
    evaluate_and_order(Moves, State, [], MVs),
    member((Move, _), MVs).

/*
    evaluate_and_order(Moves, State, SoFar, OrderedMVs) :-
    All the Moves from the current State are evaluated and
    ordered as OrderedMVs. SoFar is an accumulator for
    partial computations.
*/
evaluate_and_order([Move | Moves], State, MVs, OrderedMVs) :-
    update(State, Move, State1),
    value(State1, Value),
    insert((Move, Value), MVs, MVs1),
    evaluate_and_order(Moves, State, MVs1, OrderedMVs).

evaluate_and_order([], _, MVs, MVs).

insert(MV, [], [MV]).
insert((M, V), [(M1, V1) | MVs], [(M, V), (M1, V1) | MVs]) :- V >= V1.
insert((M, V), [(M1, V1) | MVs], [(M1, V1) | MVs1]) :- V < V1, insert((M, V), MVs, MVs1).

/*  Testing the Framework */
test_hill_climb(Problem, Moves)  :-
    initial_state(Problem, State),
    solve_hill_climb(State, [State], Moves).

solve_puzzle_hc(X) :- test_hill_climb(state, X).

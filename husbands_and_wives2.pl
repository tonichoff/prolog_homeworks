initial_state(state, state(left, [h1, h2, h3, w1, w2, w3], [])).
final_state(state(right, [], [h1, h2, h3, w1, w2, w3])).

precedes(h1, h2).
precedes(h1, h3).
precedes(h1, w1).
precedes(h1, w2).
precedes(h1, w3).
precedes(h2, h3).
precedes(h2, w1).
precedes(h2, w2).
precedes(h2, w3).
precedes(h3, w1).
precedes(h3, w2).
precedes(h3, w3).
precedes(w1, w2).
precedes(w1, w3).
precedes(w2, w3).

insert_(X, [], [X]).
insert_(X, [H | T], [X, H | T]) :- precedes(X, H).
insert_(X, [H | T], [H | T1]) :-
    precedes(H, X),
    insert_(X, T, T1).

move(state(left, Left, _), Move) :- move_helper(Left, Move).
move(state(right, _, Right), Move) :- move_helper(Right, Move).

move_helper(List, [Person]) :- member(Person, List).
move_helper(List, [FirstPerson, SecondPerson]) :-
    select_person(FirstPerson, List, NewList),
    select_person(SecondPerson, NewList, _),
    not(illegal_move([FirstPerson, SecondPerson])).

select_person(X, [X|T], T).
select_person(X, [_|T], T1) :- select_person(X, T, T1).

illegal_move([h1, w2]).
illegal_move([h1, w3]).
illegal_move([h2, w1]).
illegal_move([h2, w3]).
illegal_move([h3, w1]).
illegal_move([h3, w2]).

update(state(Coast, Left, Right), Move, state(NewCoast, NewLeft, NewRight)) :-
    update_coast(Coast, NewCoast),
    update_persons(Move, Coast, Left, Right, NewLeft, NewRight).

update_coast(left, right).
update_coast(right, left).

update_persons(Move, left, Left, Right, NewLeft, NewRight) :-
    update_persons_helper(Move, Left, Right, NewLeft, NewRight).
update_persons(Move, right, Left, Right, NewLeft, NewRight) :-
    update_persons_helper(Move, Right, Left, NewRight, NewLeft).

update_persons_helper([], From, To, From, To).
update_persons_helper([H|T], From, To, NewFrom, NewTo) :-
    select(H, From, TempFrom),
    insert_(H, To, TempTo),
    update_persons_helper(T, TempFrom, TempTo, NewFrom, NewTo).

legal(state(_, Left, Right)) :-
    legal_coast(Left),
    legal_coast(Right).

legal_coast(Coast) :-
    length(Coast, Length),
    Length < 2.

legal_coast(Coast) :-
    length(Coast, Length),
    Length >= 2,
    wives(Coast, Wives),
    length(Wives, WivesCount),
    (Length = WivesCount; check_wife_has_husband(Wives, Coast)).

check_wife_has_husband([], _).
check_wife_has_husband([H | T], Coast) :-
    couple(Husband, H),
    member(Husband, Coast),
    check_wife_has_husband(T, Coast).

couple(h1, w1).
couple(h2, w2).
couple(h3, w3).

wives(Coast, Wives) :- wives_helper(Coast, [], Wives).

wives_helper([], In, In).
wives_helper([H | T], In, [H | B]) :-
    couple(_, H),
    wives_helper(T, In, B).

wives_helper([H | T], In, B) :-
    not(couple(_, H)),
    wives_helper(T, In, B).

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

value(state(_, _, Right), Value) :- length(Right, Value).

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

solve_puzzle_hc(X, L) :- test_hill_climb(state, X), length(X, L).






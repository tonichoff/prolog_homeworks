bird(X) :- penguin(X).
bird(X) :- hawk(X).

can_fly(X) :- not(cannot_fly(X)), bird(X).

cannot_fly(X) :- penguin(X).

penguin(lesha).
hawk(nastya).

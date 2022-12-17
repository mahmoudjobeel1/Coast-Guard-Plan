max(X,Y,X) :- X >= Y,!.
max(X,Y,Y) :- X < Y.

max(4,5,Max).


parent(pam,bob).
parent(tom,bob).
parent(tom,liz).
parent(bob,ann).
parent(bob,pat).
parent(pat,jim).

parent(bob,X).

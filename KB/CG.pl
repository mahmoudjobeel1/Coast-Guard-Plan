:-include('./KB/KB.pl').
:- set_prolog_stack(global, limit(100 000 000 000)).
:- set_prolog_stack(trail,  limit(20 000 000 000)).
:- set_prolog_stack(local,  limit(2 000 000 000)).
:-set_prolog_flag(answer_write_options,[max_depth(0)]).


fluent(location(X,Y)):- agent_loc(X,Y).
fluent(ships(X)):- ships_loc(X).
fluent(holding(nothing)).

%facts, unlike fluents, don't change
fact(station(X,Y)):- station(X,Y).
fact(capacity(C)):- capacity(C).
fact(grid(N, M)):- grid(N, M).
fact(traverse(up,-1,0)).
fact(traverse(down,1,0)).
fact(traverse(left,0,-1)).
fact(traverse(right,0,1)).

% s0, the initial situation, is the (ordered) set
% of fluents
s0(Situation) :-
    setof(S, fluent(S), Situation).

% Take a list of Actions and execute them
execute_process(S1, [], S1, s0). % Nothing to do
execute_process(S1, [Action|Process], S2, S) :-
    poss(Action, S1), % Ensure valid Process
    result(S1, Action, Sd, Type),
    % print(Action),
    % nl,
    % print(S1),
    % nl,
    % print(Sd),
    % nl,
    execute_process(Sd, Process, S2, NxtS),
    S = result(Type,NxtS).

% Does a fluent hold (is true) in the Situation?
% This is the query mechanism for Situations
% Use-case 1: check a known fluent
holds(Fluent, Situation) :-
    ground(Fluent), ord_memberchk(Fluent, Situation), !.
% Use-case 2: search for a fluent
holds(Fluent, Situation) :-
    member(Fluent, Situation).

% Utility to replace a fluent in the Situation
replace_fluent(S1, OldEl, NewEl, S2) :-
    ord_del_element(S1, OldEl, Sd),
    ord_add_element(Sd, NewEl, S2).

remove(X,[X|T],T).
remove(X,[H|T],[H|Out]):-remove(X,T,Out).
% Lots of actions to declare here...
% Still less code than writing out the
% graph we're representing
% 
% CG's Action Repertoire :
%  - traverse(Origin, Destination)
%  - pickup(Item)
%  - drop(Item)
test(X):-
    X>0.

isValid(X,Y):-
    grid(R,C),
    X>=0,
    X<R,
    Y>=0,
    Y<C.

add(X,Y,Sum):-
    Sum is X + Y.

poss(traverse(Dir,Dx, Dy), S) :-
    fact(traverse(Dir,Dx, Dy)),
    % If agent is in X and Y
    holds(location(X, Y), S),
    add(X, Dx, X1),
    add(Y, Dy, Y1),
    isValid(X1,Y1).

poss(pickup(X,Y), S) :-
    % If agent is in the same place X, Y and not
    % holding anything
    holds(location(X, Y), S),
    ShipsToPick = [X,Y],
    holds(location(X, Y), S),
    holds(ships(List), S),
    member(ShipsToPick, List),
    (capacity(C),
    C =:= 2;
    holds(holding(nothing), S)).
poss(drop(X,Y,PickedShips), S) :-
    % Can drop something if holding it
    % Can't drop nothing!
    holds(location(X, Y), S),
    station(X,Y),
    holds(location(X, Y), S),
    holds(holding(PickedShips), S),
    dif(PickedShips, nothing).




result(S1, traverse(Dir,Dx,Dy), S2, Dir) :-
    % agent moves
    fact(traverse(Dir,Dx, Dy)),
    holds(location(X, Y), S1),
    add(X, Dx, X1),
    add(Y, Dy, Y1),
    number(X1),
    number(Y1),
    replace_fluent(S1, location(X, Y),
                   location(X1, Y1), S2).
result(S1, pickup(X, Y), S2, Type) :-
    % agent is holding X
    holds(location(X, Y), S1),
    holds(ships(List), S1),
    remove( [X,Y],List, NewShips),
    Type = pickup,
    replace_fluent(S1, ships(List),
                   ships(NewShips), Sd),
    (holds(holding(nothing),S1),
    replace_fluent(Sd, holding(nothing),
                   holding( [X,Y]), S2));
    (holds(holding(Y), S1),
    replace_fluent(Sd, holding(Y),
                   holding([ [X,Y]|Y]), S2)).
result(S1, drop(X,Y,PickedShips), S2, Type) :-
    % agent is no-longer holding X,
    % its location is not changed
    Type = drop,
    holds(location(X, Y), S1),
    holds(holding(PickedShips), S1),
    replace_fluent(S1, holding(PickedShips),
                   holding(nothing), S2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


goalSituation(ships(X)):- X = [].
goalSituation(location(X, Y)) :- fact(station(X, Y)).
goalSituation(holding(nothing)).

% The Goal Situation is the (ordered) set of fluents that
% describe a goal
goal_situation(S) :-
    setof(G, goalSituation(G), S).

% Test to see if Situation satifies the Goal
% Note that the Situation can contain fluents
% not described in Goal
reached_goal(GoalSituation, Situation) :-
    ord_subtract(GoalSituation, Situation, []). % [] -> no goals not in Situation


dfs(Process, S) :-
    s0(S0),
    goal_situation(GoalSituation),

    % generate a solution
    execute_process(S0, Process, Result, S),
    % test solution
    reached_goal(GoalSituation, Result).

ids(X,L, S) :-
    (call_with_depth_limit(dfs(X, S),L,R),
        number(R));
    (call_with_depth_limit(dfs(X, S),L,R),
        R=depth_limit_exceeded,
        L1 is L+1,
        ids(X,L1,S)).

goal(S):- 
    ids(S1,1,S).


% getOutput([],"s0").
% getOutput([traverse(Dir,_,_)|L], Out):-
%     getOutput(L,Out1),
%     Out = result(Dir, Out1).
% getOutput([pickup(_, _)|L], Out):-
%     getOutput(L,Out1),
%     Out = result(pickup, Out1).
% getOutput([drop(_,_,_)|L], Out):-
%     getOutput(L,Out1),
%     Out = result(drop, Out1).



print_s([]).
print_s([H|L]):-
    write(H),
    write(" "),
    print_s(L).

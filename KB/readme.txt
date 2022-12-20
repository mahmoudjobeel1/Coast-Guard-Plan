These are two examples of a possible knowledge base that you can test your code with.
You consult a knowledge base by putting the following line at the beginning of your code:
:- include('KB.pl').
or
:- include('KB2.pl').


[holding(nothing),ships_loc([[2,2],[1,2]]),agent_loc(2,2)]

 poss(drop(1,1,[1,2]]), [holding([1,2]]),ships_loc([[2,2]]),agent_loc(1,1)]).  

     fact(traverse(Dir,Dx, Dy)),
execute_process([agent_loc(1,1)], [up()], S2).

execute_process([agent_loc(1,1)], Action, S2).


    print_s(S0),
    nl,
    print_s(GoalSituation),
    nl,
    print_s(Process),
    nl,
    print_s(Result),
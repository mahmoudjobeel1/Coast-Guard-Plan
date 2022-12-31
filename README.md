# Coast Guard Plan
The Coast Guard problem is a problem where we have a grid (3 x 3) or (4 x 4) represents a sea area and there are 1 or 2 ships each has 1 passenger, our agent (Rescue Boat) task is to try to investigate the grid, reach the ships positions and  save people by taking them to Stations. Our agent is a blind one, it doesn’t know in the beginning where it should go, but it has a KB(knowledge base) which informs it of the grid objects positions.

*******************************************

## Test Case:
grid(4,4).
agent_loc(0,2).
ships_loc([[1,2], [3,2]]).
station(1,1).
capacity(2).

## Result:
result(drop,result(left,result(pickup,result(right,result(drop,result(left,result(up,result(up,result(pickup,result(down,result(down,result(down,s0))))))))))))
result(drop,result(left,result(pickup,result(right,result(drop,result(left,result(up,result(up,result(pickup,result(down,result(down,result(down,s0))))))))))))
others

## Time:
 28.43 s


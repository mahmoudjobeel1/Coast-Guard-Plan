# Coast Guard Plan
The Coast Guard problem is a problem where we have a grid (3 x 3) or (4 x 4) represents a sea area and there are 1 or 2 ships each has 1 passenger, our agent (Rescue Boat) task is to try to investigate the grid, reach the ships positions and  save people by taking them to Stations. Our agent is a blind one, it doesn’t know in the beginning where it should go, but it has a KB(knowledge base) which informs it of the grid objects positions.

*******************************************

## Test Case:
grid(3,3).
agent_loc(0,1).
ships_loc([[2,2],[1,2]]).
station(1,1).
capacity(1).

![alt text](https://i.postimg.cc/CKZjVmnP/KB-example.png)


## Result:
- sult(drop,result(left,result(pickup,result(right,result(drop,result(left,result(up,result(pickup,result(right,result(down,result(down,s0)))))))))))
- sult(drop,result(left,result(pickup,result(right,result(drop,result(up,result(left,result(pickup,result(right,result(down,result(down,s0)))))))))))
- others

## Time:
3.46 s


<a href = "https://github.com/Abdelrahman-Khater">
 </a>
<a href = "https://github.com/Abdelrahman-Khater">
  <img src = "https://avatars.githubusercontent.com/u/61662389?v=4"/>
</a>





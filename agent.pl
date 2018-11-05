% pathsearch.pl

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% This file provides code for insert_legs(), head_member() and build_path()
% used by bfsdijkstra(), ucsdijkstra(), greedy() and astar().

% insert_legs(Generated, Legs, Generated1).
% insert new legs into list of generated legs,
% by repeatedly calling insert_one_leg()

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).











% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.

solve(Start, Solution, G, N)  :-
    %consult(pathsearch), % insert_legs(), head_member(), build_path()
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N).

% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    goal_search(Node),
    build_path([[Node,Pred]|Expanded], Path).

% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).

% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % 9 x x       x x x
% 8   x x       x x x
% 7     x x       x x
% 6 x     x x x O O x
% 5 x x     x x
% 4 x x x   x x x
% 3 x x x x O   x x
% 2 x x x x x     x x
% 1 x x x x x x     x
%   1 2 3 4 5 6 7 8 9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Search pettern!
% s(Node, SuccessorNode, Cost)
% if it's not land then we need a stone cost is 100.
% if we can have a detour, first detour
% otherwise cost is 1
s([X,Y], [D,Y], 100):-
  D is X-1,
  X-1 > 0,
  not(land_or_dropped(D,Y)).

s([X,Y], [D,Y], 1):-
  D is X-1,
  X-1 > 0,
  land_or_dropped(D,Y).

s([X,Y], [X,D], 100):-
  D is Y-1,
  Y-1 >0,
  not(land_or_dropped(X,D)).

s([X,Y], [X,D], 1):-
  D is Y-1,
  Y-1 >0,
  land_or_dropped(X,D).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 s([X,Y], [D,Y], 100):-
  D is X+1,
  X+1 < 10,
  not(land_or_dropped(D,Y)).

 s([X,Y], [D,Y], 1):-
 D is X+1,
  X+1 < 10,
  land_or_dropped(D,Y).

 s([X,Y], [X,D], 100):-
 D is Y+1,
  Y+1 < 10,
 not(land_or_dropped(X,D)).

 s([X,Y], [X,D], 1):-
 D is Y+1,
  Y+1 < 10,
  land_or_dropped(X,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%using pathsearch to find intentions
%initial_intentions(Intentions)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% from path find
% where we need drop stone




  is_goal([X,Y]):-
    not(land(X,Y)).



  get_ini(A,[A|_]):-
    is_goal(A).
  get_ini(A,[_|T]):-
    get_ini(A,T).

%form start point to monster find a path cost minimum stone
% and build intents

  initial_intentions(Intentions):-
    monster(M,N),     %initial where monster is!
    assert(goal_search([M,N])),
    retractall(goal_search(_)),
    assert(goal_search([M,N])),
    solve([1,1],Path,_,_),
    reverse(Path,P),
    findall([goal(X,Y),[]],(get_ini(A,P),A = [X,Y]),I)
    ,Intentions = intents(I,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% trigger(Percepts, Goals)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trigger([], []).
trigger([stone(X, Y)|Tail], [goal(X, Y)|Goals]) :-
trigger(Tail, Goals).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% incorporate_goals(Goals, Intentions,Intentions1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








% if the smallest path not to go through water,cost must<100,
valid_goal(X,Y,N):-
  retractall(goal_search(_)),
  assert(goal_search([X,Y])),
  agent_at(X,Y),!,
  N=0.

valid_goal(X,Y,N):-
retractall(goal_search(_)),
assert(goal_search([X,Y])),
agent_at(X0,Y0),
solve([X0,Y0],_,N,_),
N<100.



%basic sitiation Goals =[]
incorporate_goals(Goals, Intentions,Intentions1):-
 Goals=[],
 Intentions1 = Intentions.

%sitiation if already in list
incorporate_goals(Goals, Intentions,Intentions1):-
 Intentions = intents(_,In),
 Goals=[goal(X,Y)|Tails],
 member([goal(X,Y),_],In),
 incorporate_goals(Tails, Intentions,Intentions1).

 %sitiation no valid path
 incorporate_goals(Goals, Intentions,Intentions1):-
  Intentions = intents(_,Inpick),
  Goals=[goal(X,Y)|Tails],
  not(member([goal(X,Y),_],Inpick)),
  not(valid_goal(X,Y,_)),
  incorporate_goals(Tails, Intentions,Intentions1).

%sitiation have valid path and if the picklist is []
incorporate_goals(Goals, Intentions,Intentions1):-
 Intentions = intents(Drop,Inpick),
 Goals=[goal(X,Y)|Tails],
 not(member([goal(X,Y),_],Inpick)),
 Inpick=[],
 valid_goal(X,Y,_),
 Intentions2=intents(Drop,[[goal(X,Y),[]]]),
  incorporate_goals(Tails, Intentions2,Intentions1).

% sitiation have valid path and found lenth is < first goal in list
incorporate_goals(Goals, Intentions,Intentions1):-
 Intentions = intents(Drop,Inpick),
 Goals=[goal(X,Y)|Tails],
 not(member([goal(X,Y),_],Inpick)),
 Inpick=[[goal(X1,Y1),_]|_],
 valid_goal(X,Y,N),
 valid_goal(X1,Y1,N2),
 N<N2,
 Intentions2=intents(Drop,[[goal(X,Y),[]]|Inpick]),
  incorporate_goals(Tails, Intentions2,Intentions1).

%sitiation have valid path and found lenth is not < first goal in list  continue to try the next

incorporate_goals(Goals, Intentions,Intentions1):-
 Intentions = intents(Drop,Inpick),
 Goals=[goal(X,Y)|Tails],
 not(member([goal(X,Y),_],Inpick)),
 not(Inpick=[]),
 Inpick=[[goal(X1,Y1),Plan]|Tail],
 valid_goal(X,Y,N),
 valid_goal(X1,Y1,N2),
 not(N<N2),
 Intentions2=intents(Drop,Tail),
  incorporate_goals([goal(X,Y)], Intentions2,Intentions3),
  Intentions3=intents(Drop2,Inpick2),
  Intentions4=intents(Drop2,[[goal(X1,Y1),Plan]|Inpick2]),
  incorporate_goals(Tails, Intentions4,Intentions5),
  Intentions1=Intentions5.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_action(Intentions, Intentions1, Action)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% path_to_plan(Path,Plan):-
% using path make a plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if this plan is for drop
path_to_plan(Path,Plan):-
  Path=[[X,Y]|Path_tail],
  Path_tail=[],
  agent_stones(1),
  Plan=[drop(X,Y)].

%if this plan is for pick
path_to_plan(Path,Plan):-
  Path=[[X,Y]|Path_tail],
  Path_tail=[],
  not(agent_stones(1)),
  Plan=[pick(X,Y)].

%if the move step for plan
path_to_plan(Path,Plan):-
  Path=[[X,Y]|Path_tail],
  not(Path_tail=[]),
  Plan1=move(X,Y),
  path_to_plan(Path_tail,Plan2),
  Plan=[Plan1|Plan2].



% around point()
around_point((X,Y),(X,B)):-
  B is Y+1,
  B<10.
around_point((X,Y),(X,B)):-
  B is Y-1,
  B>0.
around_point((Y,X),(B,X)):-
  B is Y+1,
  B<10.
around_point((Y,X),(B,X)):-
  B is Y-1,
  B>0.


  % using search for a path and make a plan
  make_plan(X,Y,Plan):-
    agent_at(X0,Y0),
    not(distance((X0,Y0),(X,Y),0)),
    retractall(goal_search(_)),
    assert(goal_search([X,Y])),

    solve([X0,Y0],P,_,_),
    reverse(Path_in,P),
    Path_in=[_|Path],
    path_to_plan(Path,Plan).

make_plan(X,Y,Plan):-
  agent_at(X0,Y0),
  distance((X0,Y0),(X,Y),0),
  around_point((X,Y),(X_next,Ynext)),
  land_or_dropped(X_next,Ynext),!,
  Path=[[X_next,Ynext],[X,Y]],
  path_to_plan(Path,Plan).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_action(Intentions, Intentions1, Action)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if we have a stone and we already have a plan for this drop point
get_action(Intentions, Intentions1, Action):-
  agent_stones(1),
  Intentions = intents(Drop,Pick),
  Drop=[[Drop_goal,Plan_ori]|Tail],
  not(Plan_ori=[]),
  Plan_ori=[Action|Plan_left],
  applicable(Action),
  New_drop=[[Drop_goal,Plan_left]|Tail],
  Intentions1=intents(New_drop,Pick).


%if we have a stone and we do not have a plan for this drop point
get_action(Intentions, Intentions1, Action):-
  agent_stones(1),
  Intentions = intents(Drop,Pick),
  Drop=[[goal(X,Y),Plan_ori]|Tail],
  Plan_ori=[],
  make_plan(X,Y,Plan),
  Plan=[Action|Plan_left],
  applicable(Action),
  New_drop=[[goal(X,Y),Plan_left]|Tail],
  Intentions1=intents(New_drop,Pick).


%if we do not have a stone we need make a plan for pick it
get_action(Intentions, Intentions1, Action):-
  not(agent_stones(1)),
  Intentions = intents(Drop,Pick),
  Pick=[[goal(X,Y),_]|Tail],
  make_plan(X,Y,Plan),
  Plan=[Action|Plan_left],
  applicable(Action),
  New_pick=[[goal(X,Y),Plan_left]|Tail],
  Intentions1=intents(Drop,New_pick).

%if we do not have a stone and no stone we can pick then stay
get_action(Intentions, Intentions1, Action):-
  not(agent_stones(1)),
  Intentions = intents(_,Pick),
  Pick=[],
  Intentions1 = Intentions,
  agent_at(X,Y),
  Action =move(X,Y).












%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  update_intentions(Observation, Intentions, Intentions1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_intentions(at(_,_),Intention,Intention).

update_intentions(dropped(X,Y),intents([[goal(X,Y)|_]|Intention1],[]),intents(Intention1,[])).
update_intentions(dropped(X,Y),intents([[goal(X,Y)|_]|Intention1],[T|H]),intents(Intention1,[T|H])).

update_intentions(picked(X,Y),intents([],[[goal(X,Y)|_]|Intention1]),intents([],Intention1)).
update_intentions(picked(X,Y),intents([H|T],[[goal(X,Y)|_]|Intention1]),intents([H|T],Intention1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

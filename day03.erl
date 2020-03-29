-module(day03).
-export([
  startA/0,
  startB/0
]).
-include_lib("eunit/include/eunit.hrl").

read_input(Filename) ->
  {ok, DataBin} = file:read_file(Filename),
  [Wire1, Wire2] = string:tokens(binary_to_list(DataBin), "\n"),
  [Wire1, Wire2].

minimum([]) -> io:format("can not find minimum of empty list~n");
minimum(List) -> lists:min(List).

positions([], Acc, _, _, _) -> Acc;
positions([Step | Steps], Acc, X, Y, L) ->
  {NewPoss, NewX, NewY, NewL} = positions(X, Y, L, Step),
  positions(Steps, lists:foldl(fun ({TX, TY, TL}, TA) -> maps:put({TX, TY}, TL, TA) end, Acc, NewPoss), NewX, NewY, NewL).
positions(X, Y, L, [$U|Num]) -> {[{X, Y+NewY, L+NewY} || NewY <- lists:seq(1, list_to_integer(Num))], X, Y+list_to_integer(Num), L+list_to_integer(Num)};
positions(X, Y, L, [$D|Num]) -> {[{X, Y-NewY, L+NewY} || NewY <- lists:seq(1, list_to_integer(Num))], X, Y-list_to_integer(Num), L+list_to_integer(Num)};
positions(X, Y, L, [$L|Num]) -> {[{X+NewX, Y, L+NewX} || NewX <- lists:seq(1, list_to_integer(Num))], X+list_to_integer(Num), Y, L+list_to_integer(Num)};
positions(X, Y, L, [$R|Num]) -> {[{X-NewX, Y, L+NewX} || NewX <- lists:seq(1, list_to_integer(Num))], X-list_to_integer(Num), Y, L+list_to_integer(Num)}.

startA() ->
  Wires = read_input(atom_to_list(?MODULE) ++ ".txt"),
  WirePaths = processMinDist(Wires),
  WirePaths.

processMinDist([Wire1, Wire2]) ->
  Pos1s = positions(string:tokens(Wire1, ","), maps:new(), 0, 0, 0),
  Pos2s = positions(string:tokens(Wire2, ","), maps:new(), 0, 0, 0),
  WirePaths = minimum(findIntersection(Pos1s, Pos2s)),
  WirePaths.

% Make a list from the map of positions in wire 1, see if that point is a key in the map of positions in wire 2. Find the Manhattan distance if point exists.
findIntersection(Positions1, Positions2) -> [abs(X)+abs(Y) || {{X, Y}, _} <- maps:to_list(Positions1), maps:is_key({X, Y}, Positions2)].

startB() ->
  Wires = read_input(atom_to_list(?MODULE) ++ ".txt"),
  WirePaths = processSteps(Wires),
  WirePaths.

processSteps([Wire1, Wire2]) ->
  Pos1s = positions(string:tokens(Wire1, ","), maps:new(), 0, 0, 0),
  Pos2s = positions(string:tokens(Wire2, ","), maps:new(), 0, 0, 0),
  WirePaths = minimum(findSteps(Pos1s, Pos2s)),
  WirePaths.

findSteps(Positions1, Positions2) -> [S1+maps:get(K, Positions2) || {K, S1} <- maps:to_list(Positions1), maps:is_key(K, Positions2)].

givenA_test() ->
  ?assertEqual(6, processMinDist(["R8,U5,L5,D3", "U7,R6,D4,L4"])),
  ?assertEqual(159, processMinDist(["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"])),
  ?assertEqual(135, processMinDist(["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])).

givenB_test() ->
  ?assertEqual(30, processSteps(["R8,U5,L5,D3", "U7,R6,D4,L4"])),
  ?assertEqual(610, processSteps(["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"])),
  ?assertEqual(410, processSteps(["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])).
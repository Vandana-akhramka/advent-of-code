-module(partA).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

start() ->
  InputMassList = read_input("input.txt"),
  rocketFuel(InputMassList).

read_input(File) ->
  {ok, DataBin} = file:read_file(File),
  MassList = [list_to_integer(StringInt) || StringInt <- string:tokens(binary_to_list(DataBin), "\n")],
  MassList.

calculateFuel(Mass) ->
  Fuel = (Mass div 3) - 2,
  Fuel.

rocketFuel(InputMassList) -> rocketFuel(InputMassList, 0).
rocketFuel([], Acc) -> Acc;
rocketFuel([H | T], Acc) -> rocketFuel(T, Acc + calculateFuel(H)).

given_test() ->
  ?assertEqual(2, rocketFuel([12])),
  ?assertEqual(2, rocketFuel([14])),
  ?assertEqual(654, rocketFuel([1969])),
  ?assertEqual(33583, rocketFuel([100756])).

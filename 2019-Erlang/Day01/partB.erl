-module(partB).
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
rocketFuel([H | T], Acc) -> rocketFuel(T, Acc + calculateTotalFuel(H)).

calculateTotalFuel(ModuleMass) -> calculateTotalFuel(ModuleMass, 0).
calculateTotalFuel(ModuleMass, Acc) when ModuleMass =< 0 -> Acc;
calculateTotalFuel(ModuleMass, Acc) ->
  MoreFuelNeeded = calculateFuel(ModuleMass),
  if MoreFuelNeeded =< 0 -> Acc;
    true -> calculateTotalFuel(MoreFuelNeeded, Acc + MoreFuelNeeded)
  end.

given_test() ->
  ?assertEqual(2, rocketFuel([14])),
  ?assertEqual(966, rocketFuel([1969])),
  ?assertEqual(50346, rocketFuel([100756])).
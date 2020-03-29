-module(day01).
-export([
  startA/0,
  startB/0
]).
-include_lib("eunit/include/eunit.hrl").

read_input(File) ->
  {ok, DataBin} = file:read_file(File),
  MassList = [list_to_integer(StringInt) || StringInt <- string:tokens(binary_to_list(DataBin), "\n")],
  MassList.

calculateFuel(Mass) ->
  Fuel = (Mass div 3) - 2,
  Fuel.

startA() ->
  InputMassList = read_input(atom_to_list(?MODULE) ++ ".txt"),
  rocketFuel(InputMassList).

rocketFuel(InputMassList) -> rocketFuel(InputMassList, 0).
rocketFuel([], Acc) -> Acc;
rocketFuel([H | T], Acc) -> rocketFuel(T, Acc + calculateFuel(H)).

startB() ->
  InputMassList = read_input(atom_to_list(?MODULE) ++ ".txt"),
  rocketFuelB(InputMassList).

rocketFuelB(InputMassList) -> rocketFuelB(InputMassList, 0).
rocketFuelB([], Acc) -> Acc;
rocketFuelB([H | T], Acc) -> rocketFuelB(T, Acc + calculateTotalFuel(H)).

calculateTotalFuel(ModuleMass) -> calculateTotalFuel(ModuleMass, 0).
calculateTotalFuel(ModuleMass, Acc) when ModuleMass =< 0 -> Acc;
calculateTotalFuel(ModuleMass, Acc) ->
  MoreFuelNeeded = calculateFuel(ModuleMass),
  if MoreFuelNeeded =< 0 -> Acc;
    true -> calculateTotalFuel(MoreFuelNeeded, Acc + MoreFuelNeeded)
  end.

givenA_test() ->
  ?assertEqual(2, rocketFuel([12])),
  ?assertEqual(2, rocketFuel([14])),
  ?assertEqual(654, rocketFuel([1969])),
  ?assertEqual(33583, rocketFuel([100756])).

givenB_test() ->
  ?assertEqual(2, rocketFuel([14])),
  ?assertEqual(966, rocketFuel([1969])),
  ?assertEqual(50346, rocketFuel([100756])).
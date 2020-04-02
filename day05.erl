-module(day05).
-export([
  start/0
]).
-include_lib("eunit/include/eunit.hrl").

-record(state, {
  position = 1,
  inputProgram = [],
  prevOutput = undefined,
  prevOutputPos = undefined
}).

read_input(File) ->
  {ok, DataBin} = file:read_file(File),
  IntCode = [list_to_integer(StringInt) || StringInt <- string:tokens(binary_to_list(DataBin), ",$\n")],
  IntCode.

transitionOpcode(StateData) ->
  CurrentOpcode = decodeOpcode(lists:nth(StateData#state.position, StateData#state.inputProgram)),
  case CurrentOpcode of
    01 -> program1(StateData);
    02 -> program2(StateData);
    03 -> program3(StateData);
    04 -> program4(StateData);
    05 -> program5(StateData);
    06 -> program6(StateData);
    07 -> program7(StateData);
    08 -> program8(StateData);
    99 -> io:format("Ouput diagnostic code ~p~n",[StateData#state.prevOutput]), StateData;
    _ ->  error
  end.

decodeOpcode(Instruction) ->
  InstructionString = integer_to_list(Instruction),
  InstructionLength = length(InstructionString),
  Opcode = case InstructionLength of
             1 -> InstructionString;
             _ -> lists:sublist(InstructionString, InstructionLength-1, 2)
           end,
  list_to_integer(Opcode).

program1(StateData) ->
  CurrPos = StateData#state.position,
  CurrentOpcode = lists:nth(CurrPos, StateData#state.inputProgram),
  [P1, P2] = decodeParameter(CurrentOpcode, 2),
  A = findParamVal(StateData#state.inputProgram, CurrPos+1, P1),
  B = findParamVal(StateData#state.inputProgram, CurrPos+2, P2),
  Cpos = lists:nth(CurrPos+3, StateData#state.inputProgram),
  C = A + B,
  First = lists:sublist(StateData#state.inputProgram, Cpos),
  Last = lists:nthtail(Cpos+1, StateData#state.inputProgram),
  UpdatedList = UpdatedList = First ++ [C] ++ Last,
  NewState = StateData#state{
    inputProgram = UpdatedList,
    position = CurrPos+4
  },
  transitionOpcode(NewState).

program2(StateData) ->
  CurrPos = StateData#state.position,
  CurrentOpcode = lists:nth(CurrPos, StateData#state.inputProgram),
  [P1, P2] = decodeParameter(CurrentOpcode, 2),
  A = findParamVal(StateData#state.inputProgram, CurrPos+1, P1),
  B = findParamVal(StateData#state.inputProgram, CurrPos+2, P2),
  Cpos = lists:nth(CurrPos+3, StateData#state.inputProgram),
  C = A * B,
  First = lists:sublist(StateData#state.inputProgram, Cpos),
  Last = lists:nthtail(Cpos+1, StateData#state.inputProgram),
  UpdatedList = UpdatedList = First ++ [C] ++ Last,
  NewState = StateData#state{
    inputProgram = UpdatedList,
    position = CurrPos+4
  },
  transitionOpcode(NewState).

program3(StateData) ->
  {ok, [Term|_]} = io:fread("Enter input> ","~ts"),
  UserInput = list_to_integer(Term),
  CurrPos = StateData#state.position,
  Parameter = lists:nth((CurrPos+1), StateData#state.inputProgram),
  First = lists:sublist(StateData#state.inputProgram, Parameter),
  UpdatedItem = [UserInput],
  Last = lists:nthtail(Parameter+1, StateData#state.inputProgram),
  UpdatedList = First ++ UpdatedItem ++ Last,
  NewState = StateData#state{
    inputProgram = UpdatedList,
    position = CurrPos+2
  },
  transitionOpcode(NewState).

program4(StateData) ->
  if StateData#state.prevOutput =/= undefined andalso StateData#state.prevOutput =/= 0 -> io:format("Ouput Error position ~p~n",[StateData#state.prevOutputPos]);
  true -> ok
  end,
  CurrPos = StateData#state.position,
  CurrentOpcode = lists:nth(CurrPos, StateData#state.inputProgram),
  [P1] = decodeParameter(CurrentOpcode, 1),
  A = findParamVal(StateData#state.inputProgram, CurrPos+1, P1),
  io:format("Ouput ~p~n",[A]),
  NewState = StateData#state{
    position = CurrPos+2,
    prevOutput = A,
    prevOutputPos = CurrPos
  },
  transitionOpcode(NewState).

program5(StateData) ->
  CurrPos = StateData#state.position,
  CurrentOpcode = lists:nth(CurrPos, StateData#state.inputProgram),
  [P1, P2] = decodeParameter(CurrentOpcode, 2),
  A = findParamVal(StateData#state.inputProgram, CurrPos+1, P1),
  NewInstruction = case A of
                     0 -> CurrPos+3;
                     _ -> 1+findParamVal(StateData#state.inputProgram, CurrPos+2, P2)
                   end,
  NewState = StateData#state{
    position = NewInstruction
  },
  transitionOpcode(NewState).

program6(StateData) ->
  CurrPos = StateData#state.position,
  CurrentOpcode = lists:nth(CurrPos, StateData#state.inputProgram),
  [P1, P2] = decodeParameter(CurrentOpcode, 2),
  A = findParamVal(StateData#state.inputProgram, CurrPos+1, P1),
  NewInstruction = case A of
                     0 -> 1+findParamVal(StateData#state.inputProgram, CurrPos+2, P2);
                     _ -> CurrPos+3
                   end,
  NewState = StateData#state{
    position = NewInstruction
  },
  transitionOpcode(NewState).

program7(StateData) ->
  CurrPos = StateData#state.position,
  CurrentOpcode = lists:nth(CurrPos, StateData#state.inputProgram),
  [P1, P2] = decodeParameter(CurrentOpcode, 2),
  A = findParamVal(StateData#state.inputProgram, CurrPos+1, P1),
  B = findParamVal(StateData#state.inputProgram, CurrPos+2, P2),
  Cpos = lists:nth(CurrPos+3, StateData#state.inputProgram),
  C = if
        A < B -> 1;
        true -> 0
      end,
  First = lists:sublist(StateData#state.inputProgram, Cpos),
  Last = lists:nthtail(Cpos+1, StateData#state.inputProgram),
  UpdatedList = UpdatedList = First ++ [C] ++ Last,
  NewState = StateData#state{
    inputProgram = UpdatedList,
    position = CurrPos+4
  },
  transitionOpcode(NewState).

program8(StateData) ->
  CurrPos = StateData#state.position,
  CurrentOpcode = lists:nth(CurrPos, StateData#state.inputProgram),
  [P1, P2] = decodeParameter(CurrentOpcode, 2),
  A = findParamVal(StateData#state.inputProgram, CurrPos+1, P1),
  B = findParamVal(StateData#state.inputProgram, CurrPos+2, P2),
  Cpos = lists:nth(CurrPos+3, StateData#state.inputProgram),
  C = if
        A == B -> 1;
        true -> 0
      end,
  First = lists:sublist(StateData#state.inputProgram, Cpos),
  Last = lists:nthtail(Cpos+1, StateData#state.inputProgram),
  UpdatedList = UpdatedList = First ++ [C] ++ Last,
  NewState = StateData#state{
    inputProgram = UpdatedList,
    position = CurrPos+4
  },
  transitionOpcode(NewState).

decodeParameter(Instruction, Num) ->
  InstructionString = integer_to_list(Instruction),
  InstructionLength = length(InstructionString),
  L = case InstructionLength of
             1 -> [0 || _ <- lists:seq(1,Num)];
             2 -> [0 || _ <- lists:seq(1,Num)];
             _ -> RevParams = lists:sublist(InstructionString, InstructionLength-2),
               Params = lists:reverse(RevParams),
               getParamMode(Params, Num)
                 end,
  L.

getParamMode(Parameters, Num) ->
  getParamMode(Parameters, Num, []).

getParamMode(_L, 0, Acc) -> Acc;
getParamMode([], Num, Acc) -> getParamMode([], Num-1, Acc ++ [0]);
getParamMode(Params, Num, Acc) ->
  getParamMode(string:substr(Params, 2, length(Params)-1), Num-1, Acc ++ [list_to_integer(string:substr(Params, 1, 1))]).

findParamVal(List, Pos, ParamMode) ->
  case ParamMode of
    0 -> ValPos = lists:nth(Pos, List),
         lists:nth(ValPos+1, List);
    1 -> lists:nth(Pos, List)
  end.

start() ->
  IntCode = read_input(atom_to_list(?MODULE) ++ ".txt"),
  StateData = #state{
    inputProgram = IntCode
  },
  transitionOpcode(StateData).

testProgram3(StateData, Term) ->
  CurrPos = StateData#state.position,
  Parameter = lists:nth((CurrPos+1), StateData#state.inputProgram),
  First = lists:sublist(StateData#state.inputProgram, Parameter),
  UpdatedItem = [Term],
  Last = lists:nthtail(Parameter+1, StateData#state.inputProgram),
  UpdatedList = First ++ UpdatedItem ++ Last,
  NewState = StateData#state{
    inputProgram = UpdatedList,
    position = CurrPos+2
  },
  transitionOpcode(NewState).

testFuncWithInput(List, Input) ->
  StateData = #state{
    inputProgram = List
  },
  Output = testProgram3(StateData, Input),
  Output#state.prevOutput.

testFuncWithoutInput(List) ->
  StateData = #state{
    inputProgram = List
  },
  Output = transitionOpcode(StateData),
  Output#state.prevOutput.

givenA_test() ->
  ?assertEqual(1, testFuncWithInput([3,0,4,0,99], 1)),
  ?assertEqual(undefined, testFuncWithoutInput([1002,4,3,4,33])),
  ?assertEqual(undefined, testFuncWithoutInput([1101,100,-1,4,0])).

givenB_test() ->
  ?assertEqual(0, testFuncWithInput([3,9,8,9,10,9,4,9,99,-1,8], 7)),
  ?assertEqual(1, testFuncWithInput([3,9,8,9,10,9,4,9,99,-1,8], 8)),
  ?assertEqual(0, testFuncWithInput([3,9,8,9,10,9,4,9,99,-1,8], 9)),
  ?assertEqual(1, testFuncWithInput([3,9,7,9,10,9,4,9,99,-1,8], 7)),
  ?assertEqual(0, testFuncWithInput([3,9,7,9,10,9,4,9,99,-1,8], 8)),
  ?assertEqual(0, testFuncWithInput([3,9,7,9,10,9,4,9,99,-1,8], 9)),
  ?assertEqual(0, testFuncWithInput([3,3,1108,-1,8,3,4,3,99], 7)),
  ?assertEqual(1, testFuncWithInput([3,3,1108,-1,8,3,4,3,99], 8)),
  ?assertEqual(0, testFuncWithInput([3,3,1108,-1,8,3,4,3,99], 9)),
  ?assertEqual(1, testFuncWithInput([3,3,1107,-1,8,3,4,3,99], 7)),
  ?assertEqual(0, testFuncWithInput([3,3,1107,-1,8,3,4,3,99], 8)),
  ?assertEqual(0, testFuncWithInput([3,3,1107,-1,8,3,4,3,99], 9)),
  ?assertEqual(0, testFuncWithInput([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 0)),
  ?assertEqual(1, testFuncWithInput([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 88)),
  ?assertEqual(0, testFuncWithInput([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 0)),
  ?assertEqual(1, testFuncWithInput([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 88)),
  ?assertEqual(999, testFuncWithInput([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 7)),
  ?assertEqual(1000, testFuncWithInput([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 8)),
  ?assertEqual(1001, testFuncWithInput([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 9)).

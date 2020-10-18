%% @author chris
%% @doc @todo Add description to heatTransfer.


-module(heatTransferRestartErrors).



-export([start/0,  processPoints/5, handleMsg/1, startTimer/0]).

%% hier muss ich die processe vorher starten 
%% jedem place gehört ein stück distarray
%% die äusseren ränder müssen den anderen mitgeteilt werden, weil die die brauchen zum rechnen


startTimer() ->
	timer:tc(heatTransferRestartErrors, start, []).

start () ->
	NUMBER_OF_LOOPS = 5,
	SIZE_OF_GRID = 20,
	PLACES = 10,
	INDEX_STEP_SIZE = SIZE_OF_GRID*SIZE_OF_GRID div PLACES,

	createDataTable(SIZE_OF_GRID * SIZE_OF_GRID), %% square table
	createProcessTable(PLACES),
	printData(1, SIZE_OF_GRID * SIZE_OF_GRID),

	StartVal = 1,

	spawnProcesses(PLACES, StartVal, INDEX_STEP_SIZE, self(), NUMBER_OF_LOOPS),

	handleMsg(PLACES).



diffuseHeat (_ ,0) -> io:fwrite("All Points processed \n");

diffuseHeat (Start, Loop) ->
	%% get values from table
	if Start > 21 ->
	   if Start < 380 ->
			First = ets:lookup_element('dataTable', Start +1, 2),
			Sec = ets:lookup_element('dataTable', Start -1, 2),
			Up = ets:lookup_element('dataTable', Start - 21, 2),
			Down = ets:lookup_element('dataTable', Start + 21, 2),
		
			%% compute new heat value
			NewHeatVal = (First + Sec + Up + Down) / 4,
			Insert = {2, NewHeatVal},
			ets:update_element('dataTable', Start, Insert);
	   	true -> {ok} 
	   end;
	 true -> {ok}
	end,
	diffuseHeat(Start+1, Loop -1).

processPoints (Start,End ,Pid ,Kill ,0) -> Pid ! {result, self()};

processPoints (Start, End, Pid, Kill, Iters)  ->
	if Kill == 1 ->
		exit("boom");
	   true -> {ok}
	end,
	diffuseHeat(Start, (End - Start)),
	processPoints(Start, End, Pid, Kill, Iters -1).
	
spawnProcesses (0, _, _,_,_) ->
	io:fwrite("All Processes spawned \n");

spawnProcesses (ProcessCount, Start, StepSize, PidMsg, Iters) ->
	if 1==1 ->
		if ProcessCount rem 2 == 0 ->
			PidNew = spawn(heatTransferRestartErrors, processPoints, [Start, Start+StepSize-1, PidMsg, 1, Iters]);
		   true ->
			 PidNew = spawn(heatTransferRestartErrors, processPoints, [Start, Start+StepSize-1, PidMsg, 0, Iters])
		end;
	   true -> 
		   PidNew = spawn(heatTransferRestartErrors, processPoints, [Start, Start+StepSize-1, PidMsg, 0, Iters])
	end,
	erlang:monitor(process, PidNew),
	Insert = {Start, Start+StepSize, PidNew},
	%% add process and index values to table
	ets:insert('processTable', [Insert]),
	spawnProcesses(ProcessCount -1, Start+StepSize, StepSize, PidMsg, Iters).

handleMsg (0) -> printData(1, 20 * 20);

handleMsg (ProcessCount) ->
	io:format("Startet msg handling ~n"),
	receive 
		%% got a result msg
		{'result', Pid} ->
			io:format("Got result from Process ~p ~n", [Pid]),
			handleMsg(ProcessCount -1);
		{'DOWN', Ref, process,Pid2, Reason} ->
			if Reason == 'normal' ->
				io:format("Worker: ~p down ~p ~n", [Pid2, Reason]),
				handleMsg (ProcessCount);
			true ->
				io:format("Worker: ~p down ~p ~n", [Pid2, Reason]),
				%% Code needed to restart process
				%% ----------------------------------------------------
				% get process indexes using pid and restart it
				[StartIndex, EndIndex] = lists:nth(1, ets:match('processTable', {'$1','$2',Pid2})),
				PidNew = spawn(heatTransferRestartErrors, processPoints, [StartIndex, EndIndex-1, self(), 0, 5]),
				Insert = {StartIndex, EndIndex, PidNew},
				%% overwrite process and index values in table
				ets:insert('processTable', [Insert]),
				%% ---------------------------------------------------
				handleMsg(ProcessCount)
			end;
		%% default case
		_ -> 
			io:format("DEFAULT - MSG UNKNOWN  ~n"),
			handleMsg(ProcessCount)
		%% timeout after x microseconds 
	after 30000 ->
		io:format("timeout in getMessages !!! ~n~n")
	end.


createDataTable (Size) ->
	Data = ets:new( 'dataTable',  [ordered_set, public, named_table, {keypos,1}, {heir,none}, {write_concurrency,false}, {read_concurrency,false}] ),
	apply(random, seed, tuple_to_list(now())),
	createData(1, Size),
	Data.

createData (_, 0) -> io:fwrite("Data created \n");

%% create HeatValues from 0 - 100 into the data table 
createData (Index, Size) ->
	HeatValue = random:uniform() * 100,
	Insert = {Index, HeatValue},
	if Index < 21 ->
		Insert2 = {Index, 1.0},
		ets:insert('dataTable', Insert2);
		true ->
			ets:insert('dataTable', Insert)
	end,
	createData(Index + 1, Size -1).

printData (_, 0) -> io:fwrite("Data printed \n");

%% print data with array like style
printData (Index, Size) ->
	Data = ets:lookup_element('dataTable', Index, 2),
	io:fwrite(" ~8.2f ", [Data]),
	if Index rem 20 == 0 ->
		   io:fwrite("\n");
		true ->
			io:fwrite("")
	end,
	printData(Index + 1, Size -1).

createProcessTable (ProcessCount) ->
	DataP = ets:new( 'processTable',  [set, public, named_table, {keypos,1}, {heir,none}, {write_concurrency,false}, {read_concurrency,false}] ),
	DataP.



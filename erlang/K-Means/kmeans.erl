%% @author chris
%% @doc @todo Add description to 'ResKTest'.


-module('kmeans').

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, startProc/2, runTimer/0]).

runTimer () ->
	timer:tc(kmeans, start, []).

start () ->
	ITERATIONS = 100,
	PROCESSES = 10,
	POINT_COUNT = 15,

	ets:new( 'processTable',  [set, private, named_table, {keypos,1}, {heir,none}, {write_concurrency,false}, {read_concurrency,false}] ),

	createKMeansTables:createPointTable(),
	createKMeansTables:createCentralCluster(),


	spawnProcesses(PROCESSES, ets:tab2list(points)),

	iterate(ITERATIONS, POINT_COUNT).

spawnProcesses (0, _) -> io:fwrite(("Processes spawned \n"));

spawnProcesses (ProcessCount, Points) ->
	if 0==1 ->
		if ProcessCount rem 2 == 0 ->
			PidNew = spawn(kmeans, startProc, [Points, self()]);
		   true ->
			 PidNew = spawn(kmeans, startProc, [Points, self()])
		end;
	   true -> 
		   PidNew = spawn(kmeans, startProc, [Points, self()])
	end,
	erlang:monitor(process, PidNew),
	Insert = {PidNew},
	%% add process and index values to table
	ets:insert('processTable', [Insert]),
	spawnProcesses(ProcessCount -1, Points ).	

startProc (Points, Pid) ->
	%% wait on new cluster values or a restart msg
	receive 
		%% clusters arrived write in local storage
		{'newClusters', [NewClusterList, Start, End]} ->
			First = lists:nth(1, NewClusterList),
			Second = lists:nth(2, NewClusterList),
			%% start calc local clusters in processes and send result back
			LocalClusters = calcLocalClusters(First, Second, Points, Start, End),
			io:format("New local Clusters: ~p~n", [LocalClusters]),
			Pid ! {'result', LocalClusters},
			startProc (Points, Pid);
		%% default case
		_ -> 
			io:format("DEFAULT - MSG UNKNOWN  ~n")
		%% timeout after x microseconds 
		after 30 ->
			io:format("timeout in getMessages startProc !!! ~n~n")
	end.

calcLocalClusters (FirstC, SecondC, Points, Start, End) ->
	for_points(FirstC, SecondC, Points, Start, End, [], [])
.


for_points(FirstC, SecondC, Points, Start, End, FirstCN, SecCN)  when Start < End +1 ->
	{FirstX, FirstY} = element(2, FirstC),
	{SecX, SecY} = element(2, SecondC),
	{X,Y} = lists:nth(Start, Points),
	%% calc dis value
	ED11 = X - FirstX,
	ED12 = Y - FirstY,
	D1 = ED11*ED11 + ED12*ED12,
	ED21 = X - SecX,
	ED22 = Y - SecY,
	D2 = ED21 * ED21 + ED22 * ED22,
	%% update
	if
		D1 < D2 ->
			NewFC = lists:append(FirstCN, [X,Y]),
			for_points(FirstC, SecondC, Points, Start +1, End, NewFC, SecCN);
		true ->
			NewSC = lists:append(SecCN, [X,Y]),
			for_points(FirstC, SecondC, Points, Start +1, End, FirstCN, NewSC)
	end;

for_points(FirstC, SecondC, Points, Start, End, FirstCN, SecCN) -> 
	[FirstCN, SecCN]
	.

iterate (0, _) -> io:fwrite(("ITERATIONS END \n"));

iterate (ITERS, POINT_COUNT) ->
	
	io:format("ITERATIONS RUN  ~p \n", [ITERS]),
	ProcInfo = ets:tab2list(processTable),
	%% send the current clusters to the processes
	%% triggers the calc of new clusters
	NumAvail = lists:foldl(fun(X, Sum) -> 1 + Sum end, 0, ProcInfo), % number of available procs
    	Div = POINT_COUNT div NumAvail, % share for each proc
    	Rem = POINT_COUNT rem NumAvail, % extra share for proc0
 	Start = 1, %next point to be processed
	End = Start + Div, % if place0 ...+rem
	sendDataToProcs(NumAvail, ProcInfo, POINT_COUNT, Start, Div),
	
	waitOnProcesses(NumAvail),
	myLib:calcCentralClusters(),
	iterate(ITERS -1, POINT_COUNT).

sendDataToProcs(0,_,_,_,_) -> {ok};

sendDataToProcs (Procs, ProcInfo, PointCount, Start, Div) ->
	%% go through the procList and send the data to the procs
	element(1,lists:nth(Procs,ProcInfo)) ! {'newClusters' , [ets:tab2list('centralClusters'), Start, Start+Div]},
	sendDataToProcs(Procs-1, ProcInfo, PointCount, Start+Div, Div)
	.


waitOnProcesses(0) -> io:fwrite(("All procs returned \n"));

waitOnProcesses(Procs) ->
	receive 
		%% got a result msg, sum up and go further with one process less
		{'result', ClusterList} ->
			io:format("New Clusters: ~p~n", [ClusterList]),
			waitOnProcesses(Procs -1);	
		%% on error delete the process from the processtable
		{'DOWN', Ref, process, Pid2, Reason} ->
			if Reason == 'normal' ->
				io:format("Worker: ~p down ~p ~n", [Pid2, Reason]),
				waitOnProcesses(Procs -1);
			true ->
				io:format("Worker: ~p down ~p ~n", [Pid2, Reason]),
				ets:delete(processTable, Pid2),
				waitOnProcesses(Procs -1)
			end;
		%% default case
		_ -> 
			io:format("DEFAULT - MSG UNKNOWN  ~n"),
			waitOnProcesses(Procs -1)
		%% timeout after x microseconds 
		after 3000 ->
			io:format("timeout in getMessages !!! ~n~n")
	end.


	

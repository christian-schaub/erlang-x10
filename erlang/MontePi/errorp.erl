
-module (errorp).

-define (KILL_STEP, 3).

%% this functions can be called outside module e.g. the shell
 
-export ([run/2, runTimer/2, process_points/3]).


%% run with specified number of processes and points
%% points will be split to processes
 
run (ProcessCount, Points) ->
	PointsPerProcess = Points div ProcessCount,
	createWorker (ProcessCount, PointsPerProcess),
	getMessages (ProcessCount, Points, PointsPerProcess,0).


%% run the simulation with the erlang built-in timer function
%% code: timer:tc(pimc, run, [1, 10000]).

runTimer (ProcessCount, Points) ->
	timer:tc(errorp, run, [ProcessCount, Points]).

%% create required processes
%% each gets the Pid of the current process to send back the result
%% register(Name, Pid)	Associates the name Name, an atom, with the process Pid
%% erlang:monitor(Process, Pid) Starts monitoring the spawned Process

createWorker (0, PointsPerProcess) ->
	io:format("Started all processes.~n~n");

createWorker (ProcessCount, PointsPerProcess) ->
	Pid = self(),
	if ProcessCount rem 2 == 0 ->
		PidNew = spawn(errorp, process_points, [PointsPerProcess, Pid, 1]);
	   true ->
		 PidNew = spawn(errorp, process_points, [PointsPerProcess, Pid, 0])
	end,  
	erlang:monitor(process, PidNew),
	io:format("Worker created: ~p PID ~p~n", [ProcessCount, PidNew]),
	createWorker(ProcessCount - 1, PointsPerProcess).
 
%% Wait for the result msg of the processes
%% First the total sum is calculated, then the value of Pi. 

%% Wait for other msg like "shutdown"

getMessages (0, Points, PointsPerProcess, Sum) ->
	Pi = (Sum / Points) * 4,
	Error = abs(math:pi() - Pi),
	io:format("Pi: ~p~n", [Pi]),
	io:format("Error %: ~p~n", [Error*100]);

getMessages (ProcessCount, Points, PointsPerProcess, Sum) ->
%	io:format("Waiting for messages ~n"),
	receive
		%% got a result msg, sum up and go further with one process less
		{'result', Count} ->
			io:format("Sum current: ~p~n", [Sum]),
			getMessages (ProcessCount -1, Points, PointsPerProcess, Sum + Count);
		%% doing nothing on error, go further receiving messages 
		{'DOWN', Ref, process, Pid2, Reason} ->
			if Reason == 'normal' ->
				io:format("Worker: ~p down ~p ~n", [Pid2, Reason]),
				getMessages (ProcessCount, Points, PointsPerProcess, Sum);
			true ->
				io:format("Worker: ~p down ~p ~n", [Pid2, Reason]),
				getMessages (ProcessCount -1, Points - PointsPerProcess, PointsPerProcess, Sum)
			end;
		{'badarith', Reason} ->
			io:format("Worker: down ~p ~n", [Reason]),
			getMessages (ProcessCount, Points, PointsPerProcess, Sum);	
		%% default case
		_ -> 
			io:format("DEFAULT - MSG UNKNOWN  ~n"),
			getMessages (ProcessCount, Points, PointsPerProcess,Sum)
		%% timeout after x microseconds 
		after 3000000 ->
			io:format("timeout in getMessages !!! ~n~n")
	end.



%% Setup processing of the points
%% Seed the random number generator
%% call check_points_in_circle 
%% send the result to the monitor process.
 
process_points(Points, Pid, Kill) ->
	if Kill == 1 ->
		exit("boom");
	   true -> {ok}
	end,
	io:format("Processing Points count: ~p~n", [Points]),
	apply(random, seed, tuple_to_list(now())),
	Count = check_points_in_circle(Points, 0),
	%% send the result to the monitor process
	Pid ! {result, Count}.
 
 
%% Each point is checked whether it's in the circle. If yes its added to the count.

check_points_in_circle(0, Count) -> Count;
 
check_points_in_circle(Points, Count) ->
	X = (random:uniform() * 2) - 1,
	Y = (random:uniform() * 2) - 1,
	IsInnerCircle = X*X + Y*Y,
	if
		IsInnerCircle < 1 ->
			check_points_in_circle(Points - 1, Count + 1);
		true ->
			check_points_in_circle(Points - 1, Count)
	end.

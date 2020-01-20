station(a).
station(b).
station(c).
station(d).
station(e).
station(f).
station(g).
station(h).
station(i).
station(j).
station(k).
station(l).
station(m).
station(n).
station(o).
station(p).
station(q).

line(red).
line(green).
line(blue).
line(purple).
line(yellow).

stop(red,1,a).
stop(red,2,c).
stop(red,3,e).
stop(red,4,i).
stop(red,5,m).
stop(red,6,q).

stop(green,1,g).
stop(green,3,c).
stop(green,5,h).
stop(green,7,p).
stop(green,2,b).
stop(green,4,e).
stop(green,6,l).

stop(blue,6,k).
stop(blue,1,d).
stop(blue,5,j).
stop(blue,3,i).
stop(blue,2,h).
stop(blue,4,m).

stop(purple,5,n).
stop(purple,4,j).
stop(purple,3,i).
stop(purple,2,l).
stop(purple,1,o).

stop(yellow,5,g).
stop(yellow,3,i).
stop(yellow,7,n).
stop(yellow,1,o).
stop(yellow,6,k).
stop(yellow,4,f).
stop(yellow,2,l).
stop(yellow,8,q).

ttime(red,1,2,2).
ttime(red,2,3,3).
ttime(red,3,4,1).
ttime(red,4,5,2).
ttime(red,5,6,2).

ttime(green,1,2,2).
ttime(green,2,3,1).
ttime(green,3,4,2).
ttime(green,4,5,2).
ttime(green,5,6,3).
ttime(green,6,7,2).

ttime(blue,1,2,3).
ttime(blue,2,3,2).
ttime(blue,3,4,3).
ttime(blue,4,5,4).
ttime(blue,5,6,2).

ttime(purple,1,2,2).
ttime(purple,2,3,2).
ttime(purple,3,4,3).
ttime(purple,4,5,2).

ttime(yellow,1,2,1).
ttime(yellow,2,3,1).
ttime(yellow,3,4,2).
ttime(yellow,4,5,1).
ttime(yellow,5,6,1).
ttime(yellow,6,7,1).
ttime(yellow,7,8,2).


% station appears on two different stops where the line is different
multiple_lines(S) :- station(S),stop(X,_,S),stop(Y,_,S),X\=Y.



% s1 and s2 are stations, s1 is a start stop (1), s2 is an end stop (max Y)
termini(L,S1,S2) :- station(S1),station(S2),stop(L,1,S1),stop(L,Y,S2),\+ not_end_stop(L,Y).

% checks if not max station for that line to allow calculation of max (end of a line)
not_end_stop(L,SN) :- stop(L,SN,_),stop(L,S2,_),S2>SN.



% base case, has number of recursed relations is line length, tail is blank
get_stops(L,[_|T],I) :- lineLength(L,I), T = [].
% next stop in list is next stop on line (L), recurses until line end
get_stops(L,[X|T],I) :- stop(L,N1,X),stop(L,N2,Y), first(T,Y), N2 is N1+1,I2 is I+1, get_stops(L,T,I2).

% shorthand func
list_stops(L,List) :- get_stops(L,List,1).

% gets length of a line by grabbing the num of the end stop
lineLength(L,Length) :- stop(L,Length,_), \+ not_end_stop(L,Length).



% base case, S1 and S2 are next to eachother
segmentTime(L,S1,S2,T) :- stop(L,N1,S1),stop(L,N2,S2), N1 is N2-1, ttime(L,N1,N2,T).
% gets next station and distance, recursively adds to next station's times
segmentTime(L,S1,S2,T) :- stop(L,N1,S1),Nnext is N1+1, stop(L,Nnext,Snext), ttime(L,N1,Nnext,TCur), 
                          segmentTime(L,Snext,S2,Tnext), T is Tnext+TCur.


% base case, if final station is same as end station of segment and remaining segments empty
% segmentTime is repeated as otherwise T is a singleton and isn't evaluated
get_path(S1,S2,[Seg|Path],Visited,PreL) :- Seg = segmentTime(L,S1,S2,T),segmentTime(L,S1,S2,T), Path =[],
                                       stop(L,N1,S1),stop(L,N2,S2),N2>N1,
                                       stationsBetween(L,S1,S2,New),noMatches(Visited,New),
                                       PreL \= L.

% finds a segment on not previously visited stations, forbids line repeats, recurses with new visits
get_path(S1,S2,[Seg|Path],Visited,PreL) :- Seg = segmentTime(L,S1,Snext,T), segmentTime(L,S1,Snext,T),
                                       stop(L,N1,S1),stop(L,N2,Snext),N2>N1,
                                       stop(L,Nlast,Slast), Nlast is N2-1,
                                       stationsBetween(L,S1,Slast,New),noMatches(Visited,New),
                                       PreL \= L,
                                       concat(Visited,New,NewVis),
                                       get_path(Snext,S2,Path,NewVis,L).
                                       
% shorthand for get_path with empty visits and non-existent starting line
path(S1,S2,Path) :- get_path(S1,S2,Path,[],hs2).

% gets stations between S1,S2 (inclusive) on a line as a list, lines only go forward so +1 works
stationsBetween(L,S1,X,[X|List]) :- stop(L,N,S1),stop(L,N,X),List=[].
stationsBetween(L,X,S2,[X|List]) :- stop(L,N1,X),Ni is N1+1, stop(L,Ni,Si), stationsBetween(L,Si,S2,List).

% gets easiest path by checking it isn't of greater length than others
easiest_path(S1,S2,Path) :- path(S1,S2,Path), \+ not_easiest_path(S1,S2,Path).
not_easiest_path(S1,S2,Path) :- path(S1,S2,Path), path(S1,S2,Path2), 
                                len(Path,L1), len(Path2,L2), L1>L2.

% gets shortest path by checking it doesn't have more stations than others
shortest_path(S1,S2,Path) :- path(S1,S2,Path), \+ not_shortest_path(S1,S2,Path).
not_shortest_path(S1,S2,Path) :- path(S1,S2,Path), path(S1,S2,Path2),
                                 stationTotal(Path,T1),stationTotal(Path2,T2), T1>T2.

% gets the total number of stations on a path by summing stations between segment ends
% end nodes aren't counted except for the very last one, as otherwise they'd be repeated
stationTotal([],1).
stationTotal([Seg|Path],Scount) :- Seg = segmentTime(L,S1,S2,_), stationsBetween(L,S1,S2,Sts),
                                   len(Sts,SL), stationTotal(Path,Scountn),
                                   Scount is (SL-1)+Scountn.

% gets the fastest path by checking it doesn't have a greater time than others
fastest_path(S1,S2,Path) :- path(S1,S2,Path), \+ not_fastest_path(S1,S2,Path).
not_fastest_path(S1,S2,Path) :- path(S1,S2,Path), path(S1,S2,Path2),
                                timeTotal(Path,T1),timeTotal(Path2,T2), T1>T2.

% gets the total time in a path by adding time of each segment recursively
timeTotal([],0).
timeTotal([Seg|Path],Ttotal) :- Seg = segmentTime(L,S1,S2,T), segmentTime(L,S1,S2,T),
                                timeTotal(Path,Tn),Ttotal is T+Tn.


% ----------- HELPERS ------------------

% returns true if no element in X is in Y
noMatches([],_).
noMatches([X|XS],Y) :- \+ in(X,Y), noMatches(XS,Y).

% gets the first element of the list
first([Fst|_],Fst).

% concatenates two lists
concat([],L,L).
concat([X|XS],YS,[X|ZS]) :- concat(XS,YS,ZS).

% checks if element occurs in list by matching head
in(X,[X|_]).
in(X,[_|XS]) :- in(X,XS). 

% gets length of a list by adding 1 until empty
len([],0).
len([_|XS],L) :- len(XS,Ln), L is Ln+1. 


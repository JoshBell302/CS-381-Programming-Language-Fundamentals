% Exercise 1 %

%when(Class, Time)%
when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).
%where(Class, Classroom)%
where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).
%enroll(Student, Class)%
enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

schedule(S,P,T) :- enroll(S,C), where(C,P), when(C,T).
usage(P,T) :- where(C,P), when(C,T).
conflict(X,Y) :- when(X,T), when(Y,T).
meet(A,B) :- enroll(A,AC), enroll(B,BC), where(AC,P), where(BC,P), when(AC,T), when(BC,T).




% Exercise 2 %
rdup([],[]).
rdup([L|P], M) :- not(member(L,P))->(append([L],E,M),rdup(P,E)); rdup(P,M).

flat([],[]).
flat([L|R], F) :- flat(L,A), flat(R,B), append(A,B,F).
flat(L,[L]).

nthMember(1,[X|_],[X]).
nthMember(N,[_|L],X) :- A is N-1, nthMember(A,L,X).
tooBig(X,Y) :- length(Y,Z), X>Z.
project([N],E,L) :- not(tooBig(N,E))->(nthMember(N,E,L)); L = [].
project([N|R], E, L) :- not(tooBig(N,E))->(nthMember(N,E,A), project(R,E,B), append(A,B,L)); L = [].




% Kaung Khant Lin ID: 6540131 542

% Course prerequisite facts got from 65X curriculum
prereq(csx3002, csx3001).
prereq(csx3003, csx3002).
prereq(csx3004, csx3002).
prereq(csx3006, csx3001).
prereq(csx3009, csx3003).
prereq(itx3004, csx3006).
prereq(itx3004, itx3006).  % OR condition (multiple valid prerequisites)
prereq(itx4104, itx3007).
prereq(csx4107, csx3002).
prereq(csx4109, csx3002).

% direct prerequisite (unchanged)
direct_prereq(Course, Req) :- prereq(Course, Req).

% indirect prerequisite (simple recursive definition)
indirect_prereq(Course, Req) :-
    prereq(Course, Req).
indirect_prereq(Course, Req) :-
    prereq(Course, Mid),
    indirect_prereq(Mid, Req).

all_prereqs(Course, Prereqs) :-
    findall(P, indirect_prereq(Course, P), Ps),
    sort(Ps, Prereqs).

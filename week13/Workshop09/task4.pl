% Kaung Khant Lin, ID: 6540131
/* task4.pl

    People:      chayapol, paitoon, thanachai
    Rooms:       vmes1002, vmes1003, vmes1004
    Subjects:    network, uiux, web_development

    Clues:
        - chayapol is NOT in vmes1002
        - the instructor in vmes1004 teaches web_development
        - paitoon teaches network

    Consequence: UI/UX is NOT uniquely determined.
    Possible UI/UX outcomes:
        - uiux taught by thanachai in vmes1002
        - uiux taught by thanachai in vmes1003
        - uiux taught by chayapol  in vmes1003
*/

:- use_module(library(lists)).  % permutation/2

/* Full assignment generator satisfying all clues */
solution(Instructors) :-
    Rooms    = [vmes1002, vmes1003, vmes1004],
    Subjects = [network, uiux, web_development],

    permutation(Rooms,    [Rc, Rp, Rt]),         % rooms for C, P, T
    permutation(Subjects, [Sc, Sp, St]),         % subjects for C, P, T

    Instructors = [
        instructor(chayapol,  Rc, Sc),
        instructor(paitoon,   Rp, Sp),
        instructor(thanachai, Rt, St)
    ],

    Rc \= vmes1002,                               % Chayapol not in VMES1002
    Sp = network,                                 % Paitoon teaches Network
    member(instructor(_, vmes1004, web_development), Instructors).  % VMES1004 is Web

/* Question: who teaches UI/UX and in which room? */
uiux_teacher_room(Who, Room) :-
    solution(Instructors),
    member(instructor(Who, Room, uiux), Instructors).
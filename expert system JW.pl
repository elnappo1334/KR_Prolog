:- use_module(library(jpl)).
start :-
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl,
    sleep(0.4),
    write('********************************************************'),nl,
    sleep(0.2),
    write("##########||| ���������� ������� ���������� �������� |||#################"),nl,
    sleep(0.4),
    write('********************************************************'),nl,
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl,nl,nl,
    interface2.

preference(Client,ring) :- verify(Client," ��� �������� ������ (y/n) ?").
preference(Client,necklace) :- verify(Client," ��� �������� �������� (y/n) ?").
preference(Client,bracelet) :- verify(Client," ��� �������� �������� (y/n) ?").
preference(Client,earrings) :- verify(Client," ��� �������� ������ (y/n) ?").

preference(Client,gold,ring) :- verify(Client," �� ������������� ������ ��� ����� (y/n) ?").
preference(Client,silver,ring) :- verify(Client," �� ������������� ������� ��� ����� (y/n) ?").
preference(Client,diamonds,ring) :- verify(Client," ��� �������� ������ � ������������ (y/n) ?").

preference(Client,gold,necklace) :- verify(Client," �� ������������� ������ ��� �������� (y/n) ?").
preference(Client,silver,necklace) :- verify(Client," �� ������������� ������� ��� �������� (y/n) ?").
preference(Client,gemstones,necklace) :- verify(Client," ��� �������� �������� � ������������ ������� (y/n) ?").

preference(Client,gold,bracelet) :- verify(Client," �� ������������� ������ ��� ��������� (y/n) ?").
preference(Client,silver,bracelet) :- verify(Client," �� ������������� ������� ��� ��������� (y/n) ?").
preference(Client,gemstones,bracelet) :- verify(Client," ��� �������� �������� � ������������ ������� (y/n) ?").

preference(Client,gold,earrings) :- verify(Client," �� ������������� ������ ��� ����� (y/n) ?").
preference(Client,silver,earrings) :- verify(Client," �� ������������� ������� ��� ����� (y/n) ?").
preference(Client,diamonds,earrings) :- verify(Client," ��� �������� ������ � ������������ (y/n) ?").

recommendation(Client, "������� ������ � ������������") :-
    preference(Client,ring),
    preference(Client,gold,ring),
    preference(Client,diamonds,ring).
recommendation(Client, "���������� ������ � ������������") :-
    preference(Client,ring),
    preference(Client,silver,ring),
    preference(Client,diamonds,ring).
recommendation(Client, "������� ������") :-
    preference(Client,ring),
    preference(Client,gold,ring).
recommendation(Client, "���������� ������") :-
    preference(Client,ring),
    preference(Client,silver,ring).

recommendation(Client, "���������� �������� � ������������ �������") :-
    preference(Client,necklace),
    preference(Client,silver,necklace),
    preference(Client,gemstones,necklace).
recommendation(Client, "������� �������� � ������������ �������") :-
    preference(Client,necklace),
    preference(Client,gold,necklace),
    preference(Client,gemstones,necklace).
recommendation(Client, "���������� ��������") :-
    preference(Client,necklace),
    preference(Client,silver,necklace).
recommendation(Client, "������� ��������") :-
    preference(Client,necklace),
    preference(Client,gold,necklace).

recommendation(Client, "������� ������ � ������������") :-
    preference(Client,earrings),
    preference(Client,gold,earrings),
    preference(Client,diamonds,earrings).
recommendation(Client, "���������� ������ � ������������") :-
    preference(Client,earrings),
    preference(Client,silver,earrings),
    preference(Client,diamonds,earrings).
recommendation(Client, "������� ������") :-
    preference(Client,earrings),
    preference(Client,gold,earrings).
recommendation(Client, "���������� ������") :-
    preference(Client,earrings),
    preference(Client,silver,earrings).

recommendation(Client, "���������� ������� � ������������ �������") :-
    preference(Client,bracelet),
    preference(Client,silver,bracelet),
    preference(Client,gemstones,bracelet).
recommendation(Client, "������� ������� � ������������ �������") :-
    preference(Client,bracelet),
    preference(Client,gold,bracelet),
    preference(Client,gemstones,bracelet).
recommendation(Client, "���������� �������") :-
    preference(Client,bracelet),
    preference(Client,silver,bracelet).
recommendation(Client, "������� �������") :-
    preference(Client,bracelet),
    preference(Client,gold,bracelet).

recommendation(_, "����� ��������� ���������, ���������� ��� ���, �� � �� ���� ��� ���������� �����.").

response(Reply) :-
    read(Reply),
    write(Reply),nl.

ask(Client,Question) :-
    write(Client),write(', ����������'),write(Question),
    interface(', ����������',Client,Question),
    write('Loading.'),nl,
    sleep(1),
    write('Loading..'),nl,
    sleep(1),
    write('Loading...'),nl,
    sleep(1), nl.

:- dynamic yes/1,no/1.

verify(Client,S) :-
    (yes(S)
    -> true ;
    (no(S)
    -> fail ;
    ask(Client,S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.

pt(Client):-
    recommendation(Client,Recommendation),
    interface3(Client,', ��� ����� ������� ',Recommendation,'.'),
    write(Client),write(', ��� ����� ������� '),write(Recommendation),write('.'),undo,end.

end :-
    nl,nl,nl,
    sleep(0.7),
    write('********************************************************'),nl,
    sleep(0.4),
    write("################||| ������� �� �������� |||#####################"),nl,
    sleep(0.4),
    write('********************************************************'),nl.

interface(X,Y,Z) :-
    atom_concat(Y,X, FAtom),
    atom_concat(FAtom,Z,FinalAtom),
    jpl_new('javax.swing.JFrame', ['Expert System'], F),
    jpl_new('javax.swing.JLabel',['--- ���������� ������� ���������� �������� ---'],LBL),
    jpl_new('javax.swing.JPanel',[],Pan),
    jpl_call(Pan,add,[LBL],_),
    jpl_call(F,add,[Pan],_),
    jpl_call(F, setLocation, [400,300], _),
    jpl_call(F, setSize, [400,300], _),
    jpl_call(F, setVisible, [@(true)], _),
    jpl_call(F, toFront, [], _),
    jpl_call('javax.swing.JOptionPane', showInputDialog, [F,FinalAtom], N),
    jpl_call(F, dispose, [], _),
    write(N),nl,
    ( (N == yes ; N == y)
    -> assert(yes(Z)) ;
    assert(no(Z)), fail).

interface2 :-
    jpl_new('javax.swing.JFrame', ['Expert System'], F),
    jpl_new('javax.swing.JLabel',['--- ���������� ������� ���������� �������� ---'],LBL),
    jpl_new('javax.swing.JPanel',[],Pan),
    jpl_call(Pan,add,[LBL],_),
    jpl_call(F,add,[Pan],_),
    jpl_call(F, setLocation, [400,300], _),
    jpl_call(F, setSize, [400,300], _),
    jpl_call(F, setVisible, [@(true)], _),
    jpl_call(F, toFront, [], _),
    jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'������! ��� ��� �����? ����������, ������� ���� ���: '], N),
    jpl_call(F, dispose, [], _),
    ( N == @(null)
    -> write('�� �����.'),interface3('�� �����. ','�������','�� ','��������.'),end,fail
    ; write("������, "),write(N),write(", ������� ������ ��������� ��������� ��� ���!"),nl,pt(N)
    ).

interface3(Client,W1,Recommendation,W2) :-
    atom_concat(Client,W1, A),
    atom_concat(A,Recommendation,B),
    atom_concat(B,W2,FinalMessage),
    jpl_new('javax.swing.JFrame', ['Expert System'], F),
    jpl_new('javax.swing.JLabel',['--- ���������� ������� ���������� �������� ---'],LBL),
    jpl_new('javax.swing.JPanel',[],Pan),
    jpl_call(Pan,add,[LBL],_),
    jpl_call(F,add,[Pan],_),
    jpl_call(F, setLocation, [400,300], _),
    jpl_call(F, setSize, [400,300], _),
    jpl_call(F, setVisible, [@(true)], _),
    jpl_call(F, toFront, [], _),
    jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,FinalMessage], N),
    jpl_call(F, dispose, [], _),
    ( N == @(void)
    -> write('')
    ; write("")
    ).

help :-
    write("��� ������� ��������� �������� ����� 'start.' � ������� ������ Enter. ").

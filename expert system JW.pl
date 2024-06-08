:- use_module(library(jpl)).
start :-
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl,
    sleep(0.4),
    write('********************************************************'),nl,
    sleep(0.2),
    write("##########||| ЭКСПЕРТНАЯ СИСТЕМА ЮВЕЛИРНОГО МАГАЗИНА |||#################"),nl,
    sleep(0.4),
    write('********************************************************'),nl,
    sleep(0.4),
    write('-----------------------------------------------------------------'),nl,nl,nl,
    interface2.

preference(Client,ring) :- verify(Client," вам нравятся кольца (y/n) ?").
preference(Client,necklace) :- verify(Client," вам нравятся ожерелья (y/n) ?").
preference(Client,bracelet) :- verify(Client," вам нравятся браслеты (y/n) ?").
preference(Client,earrings) :- verify(Client," вам нравятся серьги (y/n) ?").

preference(Client,gold,ring) :- verify(Client," вы предпочитаете золото для колец (y/n) ?").
preference(Client,silver,ring) :- verify(Client," вы предпочитаете серебро для колец (y/n) ?").
preference(Client,diamonds,ring) :- verify(Client," вам нравятся кольца с бриллиантами (y/n) ?").

preference(Client,gold,necklace) :- verify(Client," вы предпочитаете золото для ожерелий (y/n) ?").
preference(Client,silver,necklace) :- verify(Client," вы предпочитаете серебро для ожерелий (y/n) ?").
preference(Client,gemstones,necklace) :- verify(Client," вам нравятся ожерелья с драгоценными камнями (y/n) ?").

preference(Client,gold,bracelet) :- verify(Client," вы предпочитаете золото для браслетов (y/n) ?").
preference(Client,silver,bracelet) :- verify(Client," вы предпочитаете серебро для браслетов (y/n) ?").
preference(Client,gemstones,bracelet) :- verify(Client," вам нравятся браслеты с драгоценными камнями (y/n) ?").

preference(Client,gold,earrings) :- verify(Client," вы предпочитаете золото для серег (y/n) ?").
preference(Client,silver,earrings) :- verify(Client," вы предпочитаете серебро для серег (y/n) ?").
preference(Client,diamonds,earrings) :- verify(Client," вам нравятся серьги с бриллиантами (y/n) ?").

recommendation(Client, "золотое кольцо с бриллиантами") :-
    preference(Client,ring),
    preference(Client,gold,ring),
    preference(Client,diamonds,ring).
recommendation(Client, "серебряное кольцо с бриллиантами") :-
    preference(Client,ring),
    preference(Client,silver,ring),
    preference(Client,diamonds,ring).
recommendation(Client, "золотое кольцо") :-
    preference(Client,ring),
    preference(Client,gold,ring).
recommendation(Client, "серебряное кольцо") :-
    preference(Client,ring),
    preference(Client,silver,ring).

recommendation(Client, "серебряное ожерелье с драгоценными камнями") :-
    preference(Client,necklace),
    preference(Client,silver,necklace),
    preference(Client,gemstones,necklace).
recommendation(Client, "золотое ожерелье с драгоценными камнями") :-
    preference(Client,necklace),
    preference(Client,gold,necklace),
    preference(Client,gemstones,necklace).
recommendation(Client, "серебряное ожерелье") :-
    preference(Client,necklace),
    preference(Client,silver,necklace).
recommendation(Client, "золотое ожерелье") :-
    preference(Client,necklace),
    preference(Client,gold,necklace).

recommendation(Client, "золотые серьги с бриллиантами") :-
    preference(Client,earrings),
    preference(Client,gold,earrings),
    preference(Client,diamonds,earrings).
recommendation(Client, "серебряные серьги с бриллиантами") :-
    preference(Client,earrings),
    preference(Client,silver,earrings),
    preference(Client,diamonds,earrings).
recommendation(Client, "золотые серьги") :-
    preference(Client,earrings),
    preference(Client,gold,earrings).
recommendation(Client, "серебряные серьги") :-
    preference(Client,earrings),
    preference(Client,silver,earrings).

recommendation(Client, "серебряный браслет с драгоценными камнями") :-
    preference(Client,bracelet),
    preference(Client,silver,bracelet),
    preference(Client,gemstones,bracelet).
recommendation(Client, "золотой браслет с драгоценными камнями") :-
    preference(Client,bracelet),
    preference(Client,gold,bracelet),
    preference(Client,gemstones,bracelet).
recommendation(Client, "серебряный браслет") :-
    preference(Client,bracelet),
    preference(Client,silver,bracelet).
recommendation(Client, "золотой браслет") :-
    preference(Client,bracelet),
    preference(Client,gold,bracelet).

recommendation(_, "набор ювелирных украшений, подходящий для вас, но я не могу его определить точно.").

response(Reply) :-
    read(Reply),
    write(Reply),nl.

ask(Client,Question) :-
    write(Client),write(', подскажите'),write(Question),
    interface(', подскажите',Client,Question),
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
    interface3(Client,', вам может подойти ',Recommendation,'.'),
    write(Client),write(', вам может подойти '),write(Recommendation),write('.'),undo,end.

end :-
    nl,nl,nl,
    sleep(0.7),
    write('********************************************************'),nl,
    sleep(0.4),
    write("################||| СПАСИБО ЗА ВНИМАНИЕ |||#####################"),nl,
    sleep(0.4),
    write('********************************************************'),nl.

interface(X,Y,Z) :-
    atom_concat(Y,X, FAtom),
    atom_concat(FAtom,Z,FinalAtom),
    jpl_new('javax.swing.JFrame', ['Expert System'], F),
    jpl_new('javax.swing.JLabel',['--- ЭКСПЕРТНАЯ СИСТЕМА ЮВЕЛИРНОГО МАГАЗИНА ---'],LBL),
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
    jpl_new('javax.swing.JLabel',['--- ЭКСПЕРТНАЯ СИСТЕМА ЮВЕЛИРНОГО МАГАЗИНА ---'],LBL),
    jpl_new('javax.swing.JPanel',[],Pan),
    jpl_call(Pan,add,[LBL],_),
    jpl_call(F,add,[Pan],_),
    jpl_call(F, setLocation, [400,300], _),
    jpl_call(F, setSize, [400,300], _),
    jpl_call(F, setVisible, [@(true)], _),
    jpl_call(F, toFront, [], _),
    jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Привет! Как вас зовут? Пожалуйста, введите ваше имя: '], N),
    jpl_call(F, dispose, [], _),
    ( N == @(null)
    -> write('Вы вышли.'),interface3('Вы вышли. ','Спасибо','за ','внимание.'),end,fail
    ; write("Привет, "),write(N),write(", давайте найдем идеальное украшение для вас!"),nl,pt(N)
    ).

interface3(Client,W1,Recommendation,W2) :-
    atom_concat(Client,W1, A),
    atom_concat(A,Recommendation,B),
    atom_concat(B,W2,FinalMessage),
    jpl_new('javax.swing.JFrame', ['Expert System'], F),
    jpl_new('javax.swing.JLabel',['--- ЭКСПЕРТНАЯ СИСТЕМА ЮВЕЛИРНОГО МАГАЗИНА ---'],LBL),
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
    write("Для запуска программы наберите слово 'start.' и нажмите кнопку Enter. ").

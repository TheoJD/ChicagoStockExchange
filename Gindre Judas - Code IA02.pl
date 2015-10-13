%lancer_jeu
lancer_jeu :-
	nl, write('      *************************************************'),nl,
	write('      ************* Chicago Stock Exchange ************'),nl,
	write('      *************************************************'),nl,nl,
	menu(Choix), /*r�cuperation de Choix qui vaut 1, 2 ou 3.*/
	mode(Choix), /*initialisation de la partie*/
	rejouer, /*proposition de rejouer*/
	!.
	
%rejouer
rejouer :-
	repeat, /*La question est repos�e tant que l'utilisateur n'a pas rentr� une r�ponse valide (oui, non)*/
	nl,nl,
	write('Voulez-vous rejouer (oui ou non) ? '),
	read(Choix), /* Lecture de la r�ponse de l'utilisateur */
	executer_choix(Choix).
	
%executer_choix
executer_choix(oui) :- lancer_jeu. /*Relance du jeu*/
executer_choix(non). /*Arr�t du jeu*/


%menu
menu(Choix) :-
	repeat, /*La question est repos�e tant que l'utilisateur n'a pas rentr� une r�ponse valide (1, 2 ou 3)*/
	write('******Menu*****'), nl, nl, 
	write(' 1 - Partie Humain VS Humain '), nl,
	write(' 2 - Partie Humain VS Machine '), nl,
	write(' 3 - Partie Machine VS Machine '), nl, nl,
	write('***************'), nl,nl,
	write('Votre choix : '),
	read(Choix), nl, /*lecture de Choix au clavier*/
	integer(Choix), /*contraintes sur la variable Choix*/
	Choix > 0,
	Choix =< 3,
	!.

%mode
mode(N) :-
	plateau_depart(Plateau), /*initialisation du plateau de jeu*/
	read_joueur(N, Joueur), /*choix par l'utilisateur du joueur qui commence (ce pr�dicat d�pend du mode de jeu choisi)*/
	jouer(N, Plateau, Joueur, [PilesFin,BourseFin,PositionFin,ResJ1Fin,ResJ2Fin]), /*lancement du jeu*/
	affiche_plateau([PilesFin,BourseFin,PositionFin,ResJ1Fin,ResJ2Fin]), /*affiche le plateau final*/
	score(BourseFin, ResJ1Fin, S1), /*calcul du score du joueur 1*/
	score(BourseFin, ResJ2Fin, S2), /*calcul du score du joueur 2*/
	afficher_gagnant(N, S1, S2), /*affichage final du gagnant*/
	!.
	
%read_joueur
read_joueur(1, Joueur) :-
	repeat, /*La question est repos�e tant que l'utilisateur n'a pas rentr� une r�ponse valide (ici j1 ou j2)*/
	write('Qui commence ? (j1 ou j2) : '),
	read(Joueur),
	joueur_ok(1, Joueur), /*v�rification de la validit� de l'entr�e au clavier*/
	!.
	
read_joueur(2, Joueur) :-
	repeat,
	write('Qui commence ? (humain ou ordinateur) : '),
	read(Tmp),
	joueur_ok(2, Tmp, Joueur).
	
read_joueur(3, Joueur) :-
	repeat,
	write('Qui commence ? (ordi1 ou ordi2) : '),
	read(Tmp),
	joueur_ok(3, Tmp, Joueur).

%joueur_ok
joueur_ok(1, j1).
joueur_ok(1, j2).
/*pour les modes 2 et 3, on "transforme" le choix du joueur en 'j1' ou j2' pour uniformiser les futurs appels de pr�dicats*/
joueur_ok(2, humain, j1). 
joueur_ok(2, ordinateur, j2).
joueur_ok(3, ordi1, j1).
joueur_ok(3, ordi2, j2).
	
%plateau_depart
plateau_depart([Piles, [[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]], 0, [], []]) :-
	/*cr�ation d'une liste al�atoire de 36 �l�ments pour l'initialisation du plateau*/
	shuffle([ble,ble,ble,ble,ble,ble,riz,riz,riz,riz,riz,riz,cacao,cacao,cacao,cacao,cacao,cacao,cafe,cafe,cafe,cafe,cafe,cafe,sucre,sucre,sucre,sucre,sucre,sucre,mais,mais,mais,mais,mais,mais], Tmp),
	cut4(Tmp, Piles). /*partage de cette liste en 9 listes de 4 �l�ments, constituant les 9 piles de d�part*/

%shuffle
/*ce pr�dicat retourne dans 'Shuffled', la liste 'List' ayant ses �l�ments m�lang�s al�atoirement*/
shuffle(List, Shuffled) :-
  length(List, Len),
  shuffle(Len, List, Shuffled).

shuffle(0, [], []).
 
shuffle(Len, List, [Elem|Tail]) :- 
	random(0, Len, RandInd), 
	nth0(RandInd, List, Elem), 
	select(Elem, List, Rest), 
	!, 
	NewLen is Len - 1, 
	shuffle(NewLen, Rest, Tail). /*appel r�cursif*/
	
%cut4
/*ce pr�dicat regroupe les �l�ments d'une liste en sous-listes de 4 �l�ments*/
cut4([], []) :- !.
cut4([A,B,C,D|Q], [[A, B, C, D] | Q2]) :- cut4(Q, Q2).

%affichages
affiche_plateau([A,B,C,D,E]) :- Pos_affiche is C+1, nl, 
	write('****Plateau****'), nl, nl, 
	write(' Marchandises : '),nl, affiche_marchandises(A,Pos_affiche,1), nl, 
	write(' Bourse : '), affiche_liste(B), nl,
	write(' Reserve joueur 1 : '), affiche_liste(D), nl,
	write(' Reserve joueur 2 : '), affiche_liste(E), nl, nl,
	write('***************'), nl,nl.
	
affiche_marchandises([], _, _):-!.

affiche_marchandises([T|Q],Pos,Pos) :- 
	write('  Trader ->  '), write(Pos), write(' : '), affiche_liste(T), nl, /*On affiche la position du trader � c�t� de la pile o� il se situe*/
	Nbis is Pos+1, 
	affiche_marchandises(Q,Pos,Nbis),!.
	
affiche_marchandises([T|Q],Pos, N) :- 
	write('             '), write(N), write(' : '), affiche_liste(T), nl, 
	Nbis is N+1, 
	affiche_marchandises(Q,Pos,Nbis).

affiche_liste([]).
affiche_liste([X|[]]) :- write(X),!.
affiche_liste([X|R]) :- write(X), write(','), affiche_liste(R).
	
affichage_coup_ordi(Mode, [Joueur,Deplacement,R1,R2]) :-
	write('*****Tour Ordinateur****'),nl,nl,
	affichage_ordi(Mode,Joueur), write(' a deplace le Trader de '), write(Deplacement), write(' cases.'),nl,
	write(' Il a pris "'), write(R1), write('" et a jete "'), write(R2), write('".'),nl,nl,
	write('************************'),nl.

affichage_ordi(2,_):-write(' Ordinateur').
affichage_ordi(3,j1):-write(' Ordi1').
affichage_ordi(3,j2):-write(' Ordi2').

%jouer
jouer(_, [Piles|Q], _, [Piles|Q]) :- 
	length(Piles,Taille),
	Taille < 3, !. /*condition d'arr�t de la partie*/

jouer(1,Plateau, Joueur, PlateauFin) :-
	affiche_plateau(Plateau), /*On affiche le plateau � chaque d�but de tour*/
	read_deplacement(Deplacement,Joueur), /*Le joueur concern� indique de combien de piles il souhaite se d�placer*/
	coup_possible(Plateau, [Joueur,Deplacement,R1Tmp,R2Tmp]), /*On unifie R1Tmp et R2Tmp avec les deux choix que le joueur a suite � son d�placement*/
	read_ressource(R1, R2, R1Tmp, R2Tmp, Joueur), /*Demande � l'utilisateur de choisir entre les deux ressources propos�es*/
	jouer_coup(Plateau, [Joueur,Deplacement,R1,R2], Plateau2), /*On joue le coup et on recup�re le plateau modifi�*/
	joueur_adverse(Joueur,JoueurAd), /*On unifie JoueurAd avec le joueur adverse*/
	jouer(1,Plateau2, JoueurAd, PlateauFin). /*On d�bute un nouveau tour dans le m�me mode pour le joueur adverse*/

jouer(2, Plateau, j1, PlateauFin) :-
	affiche_plateau(Plateau), /*On affiche le plateau � chaque d�but de tour*/
	read_deplacement(Deplacement,humain), /*Le joueur concern� indique de combien de piles il souhaite se d�placer*/
	coup_possible(Plateau, [j1,Deplacement,R1Tmp,R2Tmp]), /*On unifie R1Tmp et R2Tmp avec les deux choix que le joueur a suite � son d�placement*/
	read_ressource(R1, R2, R1Tmp, R2Tmp, humain),nl,nl, /*Demande � l'utilisateur de choisir entre les deux ressources propos�es*/
	jouer_coup(Plateau, [j1,Deplacement,R1,R2], Plateau2), /*On joue le coup et on recup�re le plateau modifi�*/
	jouer(2, Plateau2, j2, PlateauFin). /*On lance le tour de l'ordinateur*/

jouer(2, Plateau, j2, PlateauFin) :-
	affiche_plateau(Plateau), /*On affiche le plateau � chaque d�but de tour*/
	meilleur_coup(Plateau, [j2|Q]), /*L'IA s�lectionne le meilleur coup possible pour ce tour*/
	jouer_coup(Plateau, [j2|Q], Plateau2), /*L'IA joue ce coup*/
	affichage_coup_ordi(2, [j2|Q]), /*Affichage du coup jou� par l'ordinateur*/
	jouer(2, Plateau2, j1, PlateauFin). /*On lance le tour de l'humain*/

jouer(3, Plateau, Joueur, PlateauFin) :-
	affiche_plateau(Plateau), /*On affiche le plateau � chaque d�but de tour*/
	meilleur_coup(Plateau, [Joueur|Q]), /*L'IA s�lectionne le meilleur coup possible pour ce tour*/
	jouer_coup(Plateau, [Joueur|Q], Plateau2), /*L'IA joue ce coup*/
	affichage_coup_ordi(3, [Joueur|Q]), /*Affichage du coup jou� par l'ordinateur*/
	joueur_adverse(Joueur,JoueurAd), /*On unifie JoueurAd avec l'ordinateur adverse*/
	jouer(3, Plateau2, JoueurAd, PlateauFin). /*On lance le tour de l'ordinateur adverse*/

joueur_adverse(j1,j2):-!.
joueur_adverse(j2,j1):-!.

%read_deplacement
read_deplacement(Deplacement, Joueur) :-
	repeat, /*La question est repos�e tant que l'utilisateur n'a pas rentr� une r�ponse valide (un nombre entre 1 et 3)*/
	write(Joueur),write(' - Entrez un deplacement entre 1 et 3 : '),
	read(Deplacement),
	integer(Deplacement),
	Deplacement > 0,
	Deplacement =< 3.

%read_ressource
read_ressource(R1, R2, R1Tmp, R2Tmp, Joueur) :-
	repeat, /*La question est repos�e tant que l'utilisateur n'a pas rentr� une r�ponse valide (R1Tmp ou R2Tmp)*/
	write(Joueur),write(' - Entrez la ressource a garder entre "'), write(R1Tmp), write('" et "'), write(R2Tmp), write('" : '),
	read(R1), /*On lit R1, et R2 sera unifi�e avec la ressource non-choisie*/
	ressource_ok(R1, R2, R1Tmp, R2Tmp). /*Permet d'unifier R1 et R2 avec les ressources � garder et � jeter*/
	
%ressource_ok
ressource_ok(R1, R2, R1, R2) :-!.
ressource_ok(R1, R2, R2, R1).

%jouer_coup
jouer_coup([Piles,Bourse,Pos,Rj1,Rj2], [Joueur, Deplacement, R_gardee, R_jetee], [Piles2,Bourse2,Pos2,New_Rj1,New_Rj2]) :-
	length(Piles,N), /*on r�cup�re la taille de la pile : N*/
	PosTmp is (Pos + Deplacement) mod N, /*on calcule le num�ro de la pile vers laquelle on doit se d�placer*/
	modif_piles(Piles, PosTmp, PilesTmp, N), /*retire les deux ressources concern�es, des piles du plateau*/
	positionner_trader(PilesTmp, PosTmp, Piles2, Pos2, 0), /*retire les piles vides, en repositionnant �ventuellement le trader*/
	decrementer_bourse(Bourse,R_jetee,Bourse2), /*mise � jour des valeurs de la bourse*/
	maj_reserves(Joueur, R_gardee, Rj1, Rj2, New_Rj1, New_Rj2). /*mise � jour des r�serves des joueurs*/
	
maj_reserves(j1, R_gardee, Rj1, Rj2, [R_gardee|Rj1], Rj2).
maj_reserves(j2, R_gardee, Rj1, Rj2, Rj1, [R_gardee|Rj2]).
	
%coup_possible
coup_possible([Piles,B,C_Pos,Resj1,Resj2], [_, Deplacement, R1, R2]) :-
	/*ce pr�dicat v�rifie que le coup est possible et unifie R1 et R2 avec les deux ressources autour de la pile de destination*/
	length(Piles, N),
	Pos is ((C_Pos+Deplacement) mod N), /*calcul de la position d'arriv�e, apr�s le d�placement*/
	prises_possibles([Piles,B,C_Pos,Resj1,Resj2], Pos, L), /*r�cup�re dans L les deux couples (ressource gard�e, ressource jet�e) possibles*/
	member([R1,R2],L), /*unifie R1 et R2 avec l'un de ces deux couples*/
	!.

%prises_possible
prises_possibles([Piles|_], Position, [[A,B],[B,A]]) :- 
	length(Piles, N),
	Pos1 is (Position+N-1) mod N, /*position de la pile � gauche de 'Position'*/
	Pos2 is (Position+N+1) mod N, /*position de la pile � droite de 'Position'*/
	nth0(Pos1, Piles, [A|_]), /*ressource sur le dessus de la pile de gauche*/
	nth0(Pos2, Piles, [B|_]). /*ressource sur le dessus de la pile de droite*/

%modif_piles
modif_piles([],_,[],_).

modif_piles([[_|Q]|Reste1], 1, [Q|Reste2], N) :- /*Sur la pile � droite du Trader, on enl�ve le premi�re ressource de la pile*/
	modif_piles(Reste1, 0, Reste2, N), 
	!.
	
modif_piles([[_|Q]|Reste1], Position, [Q|Reste2], N) :- /*Sur la pile � gauche du Trader, on enl�ve le premi�re ressource de la pile*/
	Position is N-1, /*Indique qu'on est � gauche*/
	Position2 is N-2,
	modif_piles(Reste1, Position2, Reste2, N), 
	!.
	
modif_piles([Pile|Reste1], Position, [Pile|Reste2], N) :- /*Si on est ni � gauche ni � droite de la pile o� se situe le Trader, on ne change pas la pile*/
	Position2 is (Position+N-1) mod N,
	modif_piles(Reste1, Position2, Reste2, N).

%positionner_trader
positionner_trader([], Position, [], Position, _).

positionner_trader([[]|Q1], Position, Q2, Position2, Indice) :- /*On retire les piles vides*/
	Indice < Position, /*On est � gauche d'o� se situe le Trader*/
	Indice2 is Indice + 1,
	positionner_trader(Q1, Position, Q2, PositionTmp, Indice2),
	Position2 is PositionTmp-1, /*Donc, on d�cr�mente la position du Trader vu qu'une pile a �t� supprim�e*/
	!.

positionner_trader([[]|Q1], Position, Q2, Position2, Indice) :- /*On retire les piles vides*/
	Indice > Position, /*On est � droite d'o� se situe le Trader*/
	Indice2 is Indice + 1,
	positionner_trader(Q1, Position, Q2, Position2, Indice2),
	!.
	
positionner_trader([T1|Q1], Position, [T1|Q2], Position2, Indice) :- /*Dans les autres cas, la position du trader ne varie pas*/
	Indice2 is Indice + 1,
	positionner_trader(Q1, Position, Q2, Position2, Indice2).
	
%decrementer_bourse
decrementer_bourse([[Res,Qt1]|Q1], Res, [[Res,Qt2]|Q1]) :- /*S'il s'agit de la ressource jet�e*/
	Qt2 is (Qt1-1), /*On d�cr�mente sa valeur en bourse*/
	!.

decrementer_bourse([T|Q1], Res, [T|Q2]) :- /*On ne modifie pas la valeur des ressources sinon*/
	decrementer_bourse(Q1, Res, Q2).
	
%valeur_ressource
valeur_ressource([[Ressource,Val]|_], Ressource, Val) :- !. /*Retourne la valeur de la ressource*/

valeur_ressource([_|Q], Ressource, Val) :-
	valeur_ressource(Q,Ressource,Val).
	

%score
score(_, [], 0).

score(Bourse, [Ressource|Q], S) :-
	score(Bourse, Q, STmp), /*On calcule le score du reste de la r�serve du joueur*/
	valeur_ressource(Bourse, Ressource, Val), /*On va chercher la valeur de la ressource*/
	S is STmp + Val. /*On ajoute cette valeur au score du joueur*/

%afficher_gagnant
afficher_gagnant(_, S, S) :-
	nl, write('Mes felicitations au deux joueurs, vous etes ex-aequo !\nVotre score est de '), write(S), write('.'), !.
	
afficher_gagnant(1, S1, S2) :-
	S1 > S2,
	nl, write('Mes felicitations au joueur 1, vous etes victorieux !\nVotre score est de '), write(S1), write(' contre '), write(S2), write(' pour votre adversaire.'), !.

afficher_gagnant(1, S1, S2) :-
	S1 < S2,
	nl, write('Mes felicitations au joueur 2, vous etes victorieux !\nVotre score est de '), write(S2), write(' contre '), write(S1), write(' pour votre adversaire.'), !.
	
afficher_gagnant(2, S1, S2) :-
	S1 > S2,
	nl, write('Mes felicitations, vous avez vaincu la machine !\nVotre score est de '), write(S1), write(' contre '), write(S2), write(' pour votre adversaire.'), !.
	
afficher_gagnant(2, S1, S2) :-
	S1 < S2,
	nl, write('La machine vous a malheureusement terrasse !\nVotre score est de '), write(S1), write(' contre '), write(S2), write(' pour votre adversaire.'), !.
	
afficher_gagnant(3, S1, S2) :-
	S1 > S2,
	nl, write('Ordi1 est sorti victorieux de cette partie !\nSon score est de '), write(S1), write(' contre '), write(S2), write(' pour son adversaire.'), !.

afficher_gagnant(3, S1, S2) :-
	S1 < S2,
	nl, write('Ordi2 est sorti victorieux de cette partie !\nSon score est de '), write(S2), write(' contre '), write(S1), write(' pour son adversaire.'), !.
	
%meilleur_coup
meilleur_coup(Plateau, [Joueur|Q]) :-
	coups_possibles(Plateau, ListeCoupsPossibles, Joueur), /*Retourne la liste des coups possibles*/
	attribuer_score(Plateau, ListeCoupsPossibles, ListeScores), /*Donne l'�volution du score du joueur et de l'adversaire en fonction de chaque coup*/
	max_score(ListeScores, MaxScore), /*Retourne l'�volution du score la plus favorable au joueur*/
	index_coup(ListeScores, MaxScore, Index), /*Retourne l'indice de l'�volution maximale du score*/
	nth0(Index, ListeCoupsPossibles, [Joueur|Q]). /*Retourne le coup correspondant qui est le meilleur coup*/
	
%coups_possibles
/*On associe au Joueur et � chacun des trois d�placements les deux couples de prises possibles*/
coups_possibles([Piles,Bourse,Position,ResJ1,ResJ2], [[Joueur,1|A1], [Joueur,1|B1], [Joueur,2|A2], [Joueur,2|B2], [Joueur,3|A3], [Joueur,3|B3]], Joueur) :-
	Pos1 is Position +1,
	Pos2 is Position +2,
	Pos3 is Position +3,
	prises_possibles([Piles,Bourse,Position,ResJ1,ResJ2], Pos1, [A1,B1]),
	prises_possibles([Piles,Bourse,Position,ResJ1,ResJ2], Pos2, [A2,B2]),
	prises_possibles([Piles,Bourse,Position,ResJ1,ResJ2], Pos3, [A3,B3]).
	
%variation_score
variation_score(_, [], 0, _, _).

variation_score(Bourse, [R1|Q], S, R1, R2) :-
	variation_score(Bourse, Q, STmp, R1, R2),
	valeur_ressource(Bourse, R1, Val),
	S is STmp + Val,
	!.

variation_score(Bourse, [R2|Q], S, R1, R2) :-
	variation_score(Bourse, Q, STmp, R1, R2),
	valeur_ressource(Bourse, R2, Val),
	S is STmp + Val,
	!.

variation_score(Bourse, [_|Q], S, R1, R2) :-
	variation_score(Bourse,Q,S,R1,R2).
	
%affecter_score
affecter_score(j1, CurrentScore, S1, S2) :- CurrentScore is S1-S2.
affecter_score(j2, CurrentScore, S1, S2) :- CurrentScore is S2-S1.

%attribuer_score
attribuer_score(_,[],[]).
	
attribuer_score(Plateau, [[Joueur,D,R1,R2]|CoupsRestants], [CurrentScore|ResteScores]) :-
	jouer_coup(Plateau, [Joueur,D,R1,R2], [_,BourseTmp,_,ResJ1Tmp,ResJ2Tmp]),
	variation_score(BourseTmp, ResJ1Tmp, S1, R1, R2),
	variation_score(BourseTmp, ResJ2Tmp, S2, R1, R2),
	affecter_score(Joueur, CurrentScore, S1, S2),
	attribuer_score(Plateau,CoupsRestants,ResteScores).
	
%max_score
max_score([X],X). /*Le max d'une liste � un �l�ment est cet �l�ment*/

max_score([X|Q],X):-
	max_score(Q,Y), /*On calcule le max du reste*/
	X >= Y, /*Si le premier �l�ment est sup�rieur ou �gal au max du reste, c'est le max*/
	!.

max_score([_|Q],X):- 
	max_score(Q,X). /*Sinon c'est le max du reste*/

%index_coup
index_coup([Element|_], Element, 0). /*L'index du premier �l�ment est 0*/

index_coup([_|Q], Element, Index):-
  index_coup(Q, Element, Index1), /*On trouve l'index dans le reste*/
  !,
  Index is Index1+1. %On ajoute 1 pour avoir l'index dans la liste globale
  
  
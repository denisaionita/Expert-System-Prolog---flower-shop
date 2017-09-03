
:-use_module(library(lists)).
:-use_module(library(system)).
:-use_module(library(file_systems)).
:-op(900,fy,not).
:-dynamic fapt/3.
:-dynamic interogat/1.
:-dynamic scop/1.
:-dynamic interogabil/3.
:-dynamic regula/3.
:-dynamic intrebare_curenta/3.
:-dynamic descriere/4.



not(P):-P,!,fail.
not(_).

scrie_lista([]):-nl.

scrie_lista([solutie(Val,FC)|T]) :-
write(Val),write(' cu factorul de certitudine '),write(FC), nl,
scrie_lista(T).

scrie_lista([H|T]) :-
write(H), tab(1),
scrie_lista(T).


             
afiseaza_fapte :-
write('Fapte existente în baza de cunostinte:'),
nl,nl, write(' (Atribut,valoare) '), nl,nl,
listeaza_fapte,nl.

loop_through_list(Stream, [H|T]) :-
    write(Stream, H),
    nl(Stream),
    loop_through_list(Stream, T).
	
loop_through_list(Stream, []).

loop_through_list([H|T]):-
	write('- '),
	write(H),
	nl,
	loop_through_list(T).
	
loop_through_list([]).

loop_through_list_dem(Stream, []).

loop_through_list_dem(Stream,[H|T]):-
	write(Stream,H),
	write(Stream,' '),
	loop_through_list_dem(Stream,T).
	
loop_through_list_dem(Stream,[]).

scrie_fis_ad(Fisout,[H|T]):- open(Fisout,append,Stream),
                       loop_through_list(Stream,[H|T]),
					
                       close(Stream).
					   

scrie_fis_ad(Fisout,[]).

scrie_fis_ad(Fisout,Write):- open(Fisout,append,Stream),
                       write(Stream,Write),
					   nl(Stream),
					   
                       close(Stream).

scrie_fis_dem(Fisout,[H|T]):- open(Fisout,append,Stream),
                       loop_through_list_dem(Stream,[H|T]),
                       close(Stream).
scrie_fis_dem(Fisout,Write):- open(Fisout,append,Stream),
                       write(Stream,Write),
					   nl(Stream),
                      close(Stream).
scrie_fis_dem(Fisout,[]).
					   
genereaza_timestamp :-
	findall(X,descriere(X,_,_,_),Lsol),
	now(Nr),number_chars(Nr,ListNr),atom_chars(Laux,ListNr),atom_concat(Laux,'.txt',L),atom_concat(solutii_posibile_,L,NumeFisier),
	scrie_fis_ad(NumeFisier,Lsol).
	
listeaza_fapte:-     % TREBUIE MODIFICAT PT CERINTA f
fapt(av(Atr,Val),FC,_), 
write('('),write(Atr),write(','),
write(Val), write(')'),
write(','), write(' certitudine '),
FC1 is integer(FC),write(FC1),
nl,fail.
listeaza_fapte. % in loc de true

lista_float_int([],[]).
lista_float_int([Regula|Reguli],[Regula1|Reguli1]):-
(Regula \== utiliz, %cand era de la utiliz regula
Regula1 is integer(Regula);
Regula ==utiliz, Regula1=Regula),
lista_float_int(Reguli,Reguli1).

scrie_in_fisier(Fis,Output) :-
	open(Fis,write,Stream),
	write(Stream,Output),
	nl(Stream),
    close(Stream).
	
	
sterge_fisiere:-
file_member_of_directory('demonstratii', 'demonstratie*', _, FilePath),
delete_file(FilePath), sterge_fisiere.

sterge_fisiere.

%revine in folderul radacina al proiectului si sterge directorul 
sterge_director :-
current_directory(_, 'c:/users/denisa123/desktop/se_proiect/output_flori'),
directory_exists('demonstratii'),
delete_directory('demonstratii').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
create_replace_output_file(FolderFinal) :- 
	current_directory(D),
	(directory_exists('output_flori'),atom_concat(D,'output_flori',WdFolder1),atom_concat(WdFolder1,'/',WdFolder),current_directory(_,WdFolder);
	 make_directory('output_flori'),atom_concat(D,'output_flori',WdFolder1),atom_concat(WdFolder1,'/',WdFolder),current_directory(_,WdFolder)),
	 
	 (directory_exists('demonstratii');
	 make_directory('demonstratii')),
	(directory_exists('fisiere_log'),atom_concat(WdFolder,'fisiere_log',WdFolder2),atom_concat(WdFolder2,'/',FolderFinal);
	 make_directory('fisiere_log'),atom_concat(WdFolder,'fisiere_log',WdFolder2),atom_concat(WdFolder2,'/',FolderFinal))
	.
	 
	 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pornire :-
retractall(interogat(_)),% in interogat am atributele pt care l-am intrb deja pe utilizator 
retractall(fapt(_,_,_)),  %%in fapt avem 3 param: structura atributvaloare: ex: av(anotimp,vara), factorul de certitudine ex: 80 , istoricul:ex: [utiliz] sau o lista cu id-uri de reguli[3,17].
retractall(intrebare_curenta(_,_,_)),
repeat,
write('Introduceti una din urmatoarele optiuni: '),
nl,nl,
write(' (Incarca Consulta Reinitiaza  Afisare_fapte Afis_buchete Cum   Iesire) '),
nl,nl,write('|: '),/*sterge_fisiere,sterge_director,*/ citeste_linie([H|T]),
executa([H|T]), H == iesire.

executa([incarca]) :- 
current_directory(_,'c:/users/denisa123/desktop/se_proiect/' ),
incarca('reguli.txt'),incarca_descriere('descriere.txt'),create_replace_output_file(DirLog),
current_directory(_,'c:/users/denisa123/desktop/se_proiect/output_flori'),genereaza_timestamp,!,nl,
write('Fisierele de reguli si descriere au fost incarcate'),nl, nl,!,nl .

executa([consulta]) :-  current_directory(_,'c:/users/denisa123/desktop/se_proiect/'),scopuri_princ,current_directory(_,'c:/users/denisa123/desktop/se_proiect/'),!. 
  
executa([reinitiaza]) :-   %sterge baza de cun : faptele si atr interogate, apoi consult ca sa ia din nou intrb 
/*current_directory(_,'c:/users/denisa123/desktop/se_proiect/output_flori'),sterge_director,*/
retractall(interogat(_)),
retractall(fapt(_,_,_)),
close_all_streams,!; 
retractall(interogat(_)),
retractall(fapt(_,_,_)),
close_all_streams,! .

executa([afisare_fapte]) :-
afiseaza_fapte,!.

executa([cum|L]) :- cum(L),!.  % pt afis dem scriem: 'cum atribut este valoare...'

executa([afis_buchete]) :- afiseaza_buchetele,nl,!.

executa([iesire]):- /* current_directory(_,'c:/users/denisa123/desktop/se_proiect/output_flori'),directory_exists('demonstratii'), delete_directory('demonstratii'),close_all_streams;*/ close_all_streams,!.

executa([_|_]) :-
write('Comanda incorecta! '),nl.

afiseaza_buchetele:- 
	findall(X,descriere(X,_,_,_),Lsol),
	loop_through_list(Lsol).

meniu_descriere :-
	repeat,
	write('Introduceti una din urmatoarele optiuni: '),
	nl,nl,
	write(' (Descriere  Flori_componente   Revenire) '),
	nl,nl,write('|: |: '),citeste_linie([H|T]),
	executa_descriere([H|T]), H == revenire.

executa_descriere([descriere]):- 
	sorteaza(L),afiseaza_descrieri_solutii(L),nl,!.

executa_descriere([flori_componente]):- 
	sorteaza(L), nl,
	afiseaza_flori_solutii(L),nl,!.

executa_descriere([revenire]).

executa_descriere([_|_]) :-
nl,write('Comanda incorecta! '), nl .

scopuri_princ :-  
	scop(Atr),determina(Atr),
	afiseaza_solutii(L),L\=[],!,
	reverse(L,Linv),current_directory(_,'c:/users/denisa123/desktop/se_proiect/output_flori/demonstratii'),scrie_demonstratii1(Linv),
	current_directory(_, 'c:/users/denisa123/desktop/se_proiect/output_flori/fisiere_log'),
	scrie_fis_ad('log.txt',L),
	scrie_fis_ad('log.txt','=========================='),
	meniu_descriere;
	current_directory(_, 'c:/users/denisa123/desktop/se_proiect/output_flori/fisiere_log'),
	scrie_fis_ad('log.txt','==========================').
% determina vrea sa det valoarea pt atributul scop; afiseaza ne afis sol
scopuri_princ. % caz de oprire

scrie_demonstratii1([solutie(FC,Atr,Val)|T]):- %scriu dem in fisier
	atom_concat(demonstratie,'_',Laux1), atom_concat(Laux1,Val,Laux),atom_concat(Laux,'(max).txt',NumeFisier),tell(NumeFisier),cum(av(Atr,Val)),told,scrie_demonstratii1(T).
	
scrie_demonstratii1([]).
%afiseaza_scop(Atr) 

determina(Atr) :-
realizare_scop(av(Atr,_),_,[scop(Atr)]),!.   %ultimul parametru este istoricul 
determina(_).
 
sorteaza(Lscop) :- %ordoneaza solutiile descrescator dupa fc
    setof(solutie(FC,Atr,Val),Istoric^fapt(av(Atr,Val),FC,Istoric),Laux),
    reverse(Laux,L),
	findall(solutie(FC,tipul_buchetului,Val), member(solutie(FC,tipul_buchetului,Val),L),Lscop).
	
afiseaza_solutii(L) :- %afiseaza solutiile pe ecran, aici le si sortam inainte de afisare
	sorteaza(L),nl,
	(length(L,Length),Length==0,write('Nu exista solutie'), nl,!;
	nl,afiseaza_scop(L),nl ).

afiseaza_descrieri_solutii([solutie(FC,Atr,Val)|T]):-
	descriere(Val,_,_,Ldesc),
    nl,write('Buchet: '),write(Val),nl,
	write(Ldesc),nl, afiseaza_descrieri_solutii(T) .

afiseaza_descrieri_solutii([]).
	
afiseaza_flori_solutii([solutie(FC,Atr,Val)|T]):-
	%findall(X,(descriere(Nume,_,X,_), Nume==Val),Lflori),
	descriere(Val,_,Lflori,_),
	write('Buchet: '),write(Val),nl,
	write('Flori componente:'),nl,
	lista_flori(Lflori),
	write('-----------------------'),nl ,
    afiseaza_flori_solutii(T).
	 
afiseaza_flori_solutii([]).

lista_flori([flori(Nume,Numar)|T]):-
	write('+ '), NR is integer(Numar),write(NR), write(' '), write(Nume),nl, lista_flori(T).
	
lista_flori([]).

afiseaza_scop([solutie(FC,Atr,Val)|T]) :- %daca solutia are fc< 60 nu e considerata solutie si nu se afiseaza; foloseste scrie_scop ca sa afiseze formatat
	FC >= 60,scrie_scop(av(Atr,Val),FC),
	nl, afiseaza_scop(T),fail.
afiseaza_scop(_):- nl.

scrie_scop(av(Atr,Val),FC) :-
transformare(av(Atr,Val), X),
scrie_lista(X),tab(2),
write(' '),
write('factorul de certitudine este '),
FC1 is integer(FC),write(FC1).
/*
afiseaza_scop(Atr) :-
nl,fapt(av(Atr,Val),FC,_),
FC >= 40,scrie_scop(av(Atr,Val),FC),
nl,fail.
afiseaza_scop(_):-nl,nl.

scrie_scop(av(Atr,Val),FC) :-
transformare(av(Atr,Val), X),
scrie_lista(X),tab(2),
write(' '),
write('factorul de certitudine este '),
FC1 is integer(FC),write(FC1).

*/

realizare_scop(not Scop,Not_FC,Istorie) :-  %poate fi apelat si pt testarea premiselor, nu doar pt aflare scop
realizare_scop(Scop,FC,Istorie),
Not_FC is - FC, !.

realizare_scop(av(Atr,_),FC,_) :-
fapt(av(Atr,nu_conteaza), FC, _), ! .%in fapt se memoreaza cunostintele ; verificam daca pt acest scop avem deja val
realizare_scop(Scop,FC,_) :-
fapt(Scop,FC,_), !.

realizare_scop(Scop,FC,Istorie) :-
pot_interoga(Scop,Istorie),
!,realizare_scop(Scop,FC,Istorie).

realizare_scop(Scop,FC_curent,Istorie) :-
fg(Scop,FC_curent,Istorie). %fg primeste scop si calculeaza fc_curent si istoric
        
fg(Scop,FC_curent,Istorie) :-
regula(N, premise(Lista),concluzie(Scop,FC)), %N id-ul regulii; in premise avem structurile av in Lista
demonstreaza(N,Lista,FC_premise,Istorie), %
ajusteaza(FC,FC_premise,FC_nou),% face produsul, ajusteaza fc-ul regulii in fct de noile info
actualizeaza(Scop,FC_nou,FC_curent,N),
FC_curent == 100,!. % daca nu are fc=100, se intoarce si cauta o noua regula 
fg(Scop,FC,_) :- fapt(Scop,FC,_). 

pot_interoga(av(Atr,_),Istorie) :- 
not interogat(av(Atr,_)),%verifica daca nu s-a pus deja intrebarea legata de Atr, interogat retine atr care au deja val
interogabil(Atr,Optiuni,Mesaj), % optiuni si mesajul afisat/intrebarea
interogheaza(Atr,Mesaj,Optiuni,Istorie),nl,
asserta( interogat(av(Atr,_)) ).%,listing(interogat),nl. % introduce in lista la inceput atr care a fost interogat

%%%%% SCRIU IN FISIERUL DE DEMONSTRATII 

scrie_demonstratii([solutie(FC,tipul_buchetului,Nume)]):- 
	atom_concat(demonstratie,'_',Laux1), atom_concat(Laux1,Nume,Laux),atom_concat(Laux,'(max).txt',NumeFisier),cum_dem_fisier(av(tipul_buchetului,Nume),NumeFisier),!.

scrie_demonstratii([solutie(FC,tipul_buchetului,Nume)|T]):-
	atom_concat(demonstratie,'_',Laux1), atom_concat(Laux1,Nume,Laux),atom_concat(Laux,'.txt',NumeFisier),cum_dem_fisier(av(tipul_buchetului,Nume),NumeFisier),scrie_demonstratii(T),!.

scrie_demonstratii([]).

cum_dem_fisier(not Scop,NumeFisier) :- 
	fapt(Scop,FC,Reguli),
	lista_float_int(Reguli,Reguli1),
	FC < -20,transformare(not Scop,PG),
	append(PG,[a,fost,derivat,cu, ajutorul, 'regulilor: '|Reguli1],LL),
	scrie_fis_dem(NumeFisier,LL),afis_reguli_dem_fisier(Reguli,NumeFisier),fail.

cum_dem_fisier(Scop,NumeFisier) :-   
fapt(Scop,FC,Reguli),
lista_float_int(Reguli,Reguli1),
FC > 20,transformare(Scop,PG),
append(PG,[a,fost,derivat,cu, ajutorul, 'regulilor: '|Reguli1],LL),
scrie_fis_dem(NumeFisier,LL),afis_reguli_dem_fisier(Reguli,NumeFisier),
fail.
cum_dem_fisier(_,_).

afis_reguli_dem_fisier([],_).
afis_reguli_dem_fisier([N|X],NumeFisier) :-
afis_regula_dem_fisier(N,NumeFisier),
premisele_dem_fisier(N,NumeFisier),
afis_reguli_dem_fisier(X,NumeFisier).

afis_regula_dem_fisier(N,NumeFisier) :-
regula(N, premise(Lista_premise), concluzie(Scop,FC)), 
NN is integer(N),
scrie_fis_dem(NumeFisier,['rg::',NN]), 
scrie_fis_dem(NumeFisier,' '),
scrie_fis_dem(NumeFisier,' conditii={'),
scrie_lista_premise_fis(Lista_premise,NumeFisier), 
scrie_fis_dem(NumeFisier,'}'),
scrie_fis_dem(NumeFisier,'atunci'),
transformare2(Scop,Scop_tr), 
append(['   '],Scop_tr,L1),
FC1 is integer(FC),append(L1,['fc:',FC1],LL),
scrie_fis_dem(NumeFisier,LL),scrie_fis_dem(NumeFisier,' ').

scrie_lista_premise_fis([],_).
scrie_lista_premise_fis([H|T],NumeFisier) :-
transformare2(H,H_tr),
scrie_fis_dem(NumeFisier,H_tr),
scrie_fis_dem(NumeFisier,' '),
scrie_lista_premise_fis(T,NumeFisier).

premisele_dem_fisier(N,NumeFisier) :-
regula(N, premise(Lista_premise), _),
!, cum_premise_fis(Lista_premise,NumeFisier).
        
cum_premise_fis([],_).
cum_premise_fis([Scop|X],NumeFisier) :-
cum_dem_fisier(Scop,NumeFisier),
cum_premise_fis(X,NumeFisier). 

%%%% ENDOF SCRIEREA IN FIS DE DEMONSTRATII


cum([]) :- write('Scop? '),nl,    %cerem cum atr val
write('|:'),citeste_linie(Linie),nl,
transformare(Scop,Linie), cum(Scop).

cum(L) :- 
transformare(Scop,L),nl, cum(Scop).

cum(not Scop) :- %cazul pt fc negativ 
fapt(Scop,FC,Reguli),% al 3lea par de istoricul
lista_float_int(Reguli,Reguli1),
FC < -20,transformare(not Scop,PG), %daca e fc negativ , apelam transf ca sa afisam noi formatat
append(PG,[a,fost,derivat,cu, ajutorul, 'regulilor: '|Reguli1],LL),
scrie_lista(LL),nl,afis_reguli(Reguli),fail.

cum(Scop) :-   
fapt(Scop,FC,Reguli),
lista_float_int(Reguli,Reguli1),
FC > 20,transformare(Scop,PG),
append(PG,[a,fost,derivat,cu, ajutorul, 'regulilor: '|Reguli1],LL),
scrie_lista(LL),nl,afis_reguli(Reguli),
fail.
cum(_).

afis_reguli([]).
afis_reguli([N|X]) :-
	afis_regula(N),
	premisele(N),
afis_reguli(X).
afis_regula(N) :-
	regula(N, premise(Lista_premise), concluzie(Scop,FC)), %premise(Lista_premise) este lista de av-uri 
	NN is integer(N),
	scrie_lista(['rg::',NN]),  %aici modificam pt F, la afisare MODIFICAT
	scrie_lista([' conditii={']),
	scrie_lista_premise(Lista_premise), %si in pred asta modificam
	scrie_lista(['}','\n']),
	scrie_lista(['atunci','\n']),
	transformare2(Scop,Scop_tr), %afis concluzia
	append(['   '],Scop_tr,L1),
	FC1 is integer(FC),append(L1,['fc:',FC1],LL),
	scrie_lista(LL),nl.

scrie_lista_premise([]).
scrie_lista_premise([H|T]) :-
transformare2(H,H_tr), %definim alta transformare2 unde modificam in functie de regulile noastre DEFINIT
tab(5),scrie_lista(H_tr),
scrie_lista_premise(T).

transformare2(av(A,da),[A]) :- !.
transformare2(not av(A,da), [A,[fals]]) :- !.
%transformare(av(A,nu),[not,A]) :- !.
transformare2(av(A,V),[A,e ,egal,cu,V]).

transformare(av(A,da),[A]):- !.
transformare(not av(A,da), [not,A]) :- !.
%transformare(av(A,nu),[not,A]) :- !.
transformare(av(A,V),[A,este,V]).

premisele(N) :-
regula(N, premise(Lista_premise), _),
!, cum_premise(Lista_premise).
        
cum_premise([]).
cum_premise([Scop|X]) :-
cum(Scop),
cum_premise(X).

interogheaza(Atr,Mesaj,[da,nu],Istorie) :-  
!,write(Mesaj),
append(['('],[da, nu],Opt1),   
append(Opt1,[')'],Opt2),
append(Opt2,[nu_stiu,nu_conteaza],Opt),
scrie_lista(Opt),
nl, %afis intrebarea
de_la_utiliz(X,Istorie,[da,nu,nu_stiu,nu_conteaza]), 
det_val_fc(X,Val,FC),
asserta( fapt(av(Atr,Val),FC,[utiliz]) ),
atom_concat(Atr,=,Aux),atom_concat(Aux,Val,Interogare),
scrie_fis_ad('log.txt',Interogare).

interogheaza(Atr,Mesaj,Optiuni,Istorie) :-
	write(Mesaj),nl,
	citeste_opt(VLista,Optiuni,Istorie),
	assert_fapt(Atr,VLista).

citeste_opt(X,Optiuni,Istorie) :-   
	append(['('],Optiuni,Opt1),   
	append(Opt1,[')'],Opt2),
	append(Opt2,[nu_stiu,nu_conteaza],Opt),
	scrie_lista(Opt),
	de_la_utiliz(X,Istorie,Opt).

de_la_utiliz(X,Istorie,Lista_opt) :-
	repeat,write(': '),citeste_linie(X),
	proceseaza_raspuns(X,Istorie,Lista_opt). %verif daca este in lista de optiuni 

proceseaza_raspuns([de_ce],Istorie,_) :-
	nl,afis_istorie(Istorie),!,fail.

proceseaza_raspuns([X],_,Lista_opt):-   %verif daca este in lista de optiuni
	member(X,Lista_opt).
proceseaza_raspuns([X,fc,FC],_,Lista_opt):-
	member(X,Lista_opt),float(FC).

assert_fapt(Atr,[Val,fc,FC]) :-
	!,asserta( fapt(av(Atr,Val),FC,[utiliz]) ),atom_concat(Atr,=,Aux),atom_concat(Aux,Val,Interogare),
	scrie_fis_ad('log.txt',Interogare).
assert_fapt(Atr,[Val]) :-
	asserta( fapt(av(Atr,Val),100,[utiliz])), atom_concat(Atr,=,Aux),atom_concat(Aux,Val,Interogare),
	scrie_fis_ad('log.txt',Interogare).

det_val_fc([nu],da,-100).  %daca rasp e nu, memoreaza da cu -100 
det_val_fc([nu,FC],da,NFC) :- NFC is -FC.  
det_val_fc([nu,fc,FC],da,NFC) :- NFC is -FC.
det_val_fc([Val,FC],Val,FC).
det_val_fc([Val,fc,FC],Val,FC).
det_val_fc([Val],Val,100).
        
afis_istorie([]) :- nl.
afis_istorie([scop(X)|T]) :-
scrie_lista([scop,X]),!,
afis_istorie(T).
afis_istorie([N|T]) :-
afis_regula(N),!,afis_istorie(T).

demonstreaza(N,ListaPremise,Val_finala,Istorie) :-
dem(ListaPremise,100,Val_finala,[N|Istorie]),!.

dem([],Val_finala,Val_finala,_).
dem([H|T],Val_actuala,Val_finala,Istorie) :-
realizare_scop(H,FC,Istorie),
Val_interm is min(Val_actuala,FC),
Val_interm >= 50,
dem(T,Val_interm,Val_finala,Istorie).
 
actualizeaza(Scop,FC_nou,FC,RegulaN) :-
fapt(Scop,FC_vechi,_), %ia din baza de cun scopul si fc 
combina(FC_nou,FC_vechi,FC), % daca ajung la aceeasi sol pe mai multe cai, combin fc de la cele 2 reguli 
retract( fapt(Scop,FC_vechi,Reguli_vechi) ),
asserta( fapt(Scop,FC,[RegulaN | Reguli_vechi]) ),!.
actualizeaza(Scop,FC,FC,RegulaN) :-
asserta( fapt(Scop,FC,[RegulaN]) ).

ajusteaza(FC1,FC2,FC) :-
X is FC1 * FC2 / 100,
FC is round(X).
combina(FC1,FC2,FC) :-
FC1 >= 0,FC2 >= 0,
X is FC2*(100 - FC1)/100 + FC1,
FC is round(X).
combina(FC1,FC2,FC) :-
FC1 < 0,FC2 < 0,
X is - ( -FC1 -FC2 * (100 + FC1)/100),
FC is round(X).
combina(FC1,FC2,FC) :-
(FC1 < 0; FC2 < 0),
(FC1 > 0; FC2 > 0),
FCM1 is abs(FC1),FCM2 is abs(FC2),
MFC is min(FCM1,FCM2),
X is 100 * (FC1 + FC2) / (100 - MFC),
FC is round(X).



%%%% INCARCA DESCRIERE + REGULI

incarca_descrierea :-
repeat,citeste_descriere(L),
proceseaza(L), L==[end_of_file],nl .


incarca_descriere(F) :-
retractall(descriere(_,_,_,_)),
see(F),incarca_descrierea,seen,!.

citeste_descriere(L):-
 citeste_linie(Lin), 
(Lin==[end_of_file],L=Lin,!;
 Lin=['-','-'|_], L=[],!;
 citeste_descriere(RestLinii),append(Lin,RestLinii,L) ).

incarca(F) :-
retractall(interogat(_)),retractall(fapt(_,_,_)),
retractall(scop(_)),retractall(interogabil(_,_,_)), % in scop memoreaza atr scop, in regula mem regula din fis de intrare ( cum am zis mai sus: nr,premise([av(anotimp,vara),av(la_mare,da)],concluzie(av(loc_concediu,constanta),90),z) )
retractall(regula(_,_,_)),
see(F),incarca_reguli,seen,!.

incarca_reguli :-
repeat,citeste_propozitie(L),
proceseaza(L),L == [end_of_file],nl.

proceseaza([end_of_file]):-!.
proceseaza(L) :-
trad(R,L,[]),assertz(R), !.  %DCG
trad(scop(X)) --> [scop,:,:,X]. %MODIFICAT
%trad(scop(X)) --> [scop,X].
trad(interogabil(Atr,M,P)) --> 
['?','?','?',Atr],lista_optiuni(M),afiseaza(Atr,P).

trad(regula(N,premise(Daca),concluzie(Atunci,F))) --> identificator(N),daca(Daca),atunci(Atunci,F).

%%% INCA UN TRAD PT DESCRIERE    !!!DE COMPLETAT
%trad_desc(descriere(Buchet, Cale,ListaFlori,Desc))--> ['{','buchet',':',Buchet,'}','{','imagine',':',Cale,'}'], lista_flori(ListaFlori).

trad(descriere(Buchet, Cale,ListaFlori,Desc)) --> nume_buchet(Buchet),
												cale(Cale),
												lista_premise_desc(ListaFlori),
												desc(Desc).

trad('Eroare la parsare'-L,L,_).

nume_buchet(Buchet) --> ['{','buchet',':',Buchet, '}'].

lista_premise_desc([Buch]) --> ['-','>'],propoz_desc(Buch), ['}'] . %MODIFICAT
lista_premise_desc([Prima|Celalalte]) --> ['-','>'], propoz_desc(Prima),lista_premise_desc(Celalalte).

propoz_desc(flori(Nume,Numar)) --> [Nume,',',Numar].

cale(Cale) --> ['{','imagine',':',Cale,'}','{','flori',':'].

desc(Desc) --> ['{','descriere',':',Desc,'}'].

%%%% Reguli : 
lista_optiuni(M) --> [raspunsuri,posibile ,:],lista_de_optiuni(M).
lista_de_optiuni([Element]) -->  [Element,intrebare].  %de inlocuit ) INLOCUIT 
lista_de_optiuni([Element|T]) --> [Element,;],lista_de_optiuni(T).  

afiseaza(_,P) -->  [P]. %MODIFICAT
afiseaza(P,P) -->  [].
identificator(N) --> [rg,:,:,N].  %MODIFICAT

daca(Daca) --> [conditii,'=','{'],lista_premise(Daca). 

lista_premise([Daca]) --> propoz(Daca),['}',atunci]. %MODIFICAT
lista_premise([Prima|Celalalte]) --> propoz(Prima),[si],lista_premise(Celalalte).
lista_premise([Prima|Celalalte]) --> propoz(Prima),[','],lista_premise(Celalalte).

atunci(Atunci,FC) --> propoz(Atunci),[fc,':'],[FC].
atunci(Atunci,100) --> propoz(Atunci).

propoz(not av(Atr,da)) --> [Atr,'[','fals',']'].
propoz(av(Atr,Val)) --> [Atr,e,egal,cu,Val].
propoz(av(Atr,da)) --> [Atr].

citeste_linie([Cuv|Lista_cuv]) :-   %get_code citeste codul ascii, calculeaza cuvantul si il pune in Cuv si Car1 e car imediat dupa 
get_code(Car),citeste_cuvant(Car, Cuv, Car1), 
rest_cuvinte_linie(Car1, Lista_cuv). 
      
% -1 este codul ASCII pt EOF

rest_cuvinte_linie(-1, []):-!.    
rest_cuvinte_linie(Car,[]) :-(Car==13;Car==10), !. %13 si 10 \n \l
rest_cuvinte_linie(Car,[Cuv1|Lista_cuv]) :-
citeste_cuvant(Car,Cuv1,Car1),      
rest_cuvinte_linie(Car1,Lista_cuv).

citeste_propozitie([Cuv|Lista_cuv]) :-
get_code(Car),citeste_cuvant(Car, Cuv, Car1), 
rest_cuvinte_propozitie(Car1, Lista_cuv). 
     
rest_cuvinte_propozitie(-1, []):-!.    
rest_cuvinte_propozitie(Car,[]) :-Car==46, !.
rest_cuvinte_propozitie(Car,[Cuv1|Lista_cuv]) :-
citeste_cuvant(Car,Cuv1,Car1),      
rest_cuvinte_propozitie(Car1,Lista_cuv).

citeste_cuvant(-1,end_of_file,-1):-!.
citeste_cuvant(Caracter,Cuvant,Caracter1) :-   
caracter_cuvant(Caracter),!, 
name(Cuvant, [Caracter]),get_code(Caracter1). %name obtine din codul ascii caracterul , get_code ia codul ascii al urm caracter
citeste_cuvant(Caracter, Numar, Caracter1) :-
caracter_numar(Caracter),!,
citeste_tot_numarul(Caracter, Numar, Caracter1). 

citeste_tot_numarul(Caracter,Numar,Caracter1):-
determina_lista(Lista1,Caracter1),
append([Caracter],Lista1,Lista),
transforma_lista_numar(Lista,Numar). %echivalent cu number_codes, creaza numarul

determina_lista(Lista,Caracter1):-
get_code(Caracter), 
(caracter_numar(Caracter),
determina_lista(Lista1,Caracter1),
append([Caracter],Lista1,Lista); 
\+(caracter_numar(Caracter)),
Lista=[],Caracter1=Caracter). 

transforma_lista_numar([],0).
transforma_lista_numar([H|T],N):-
transforma_lista_numar(T,NN), 
lungime(T,L), Aux is exp(10,L),
HH is H-48,N is HH*Aux+NN.

lungime([],0).
lungime([_|T],L):-
lungime(T,L1),
L is L1+1.

tab(N):-N>0,write(' '), N1 is N-1, tab(N1).
tab(0).

% 39 este codul ASCII pt '


citeste_cuvant(Caracter,Cuvant,Caracter1) :-
Caracter==39,!,
pana_la_urmatorul_apostrof(Lista_caractere),
L=[Caracter|Lista_caractere],% daca  a gas al doilea ' inchide lista
name(Cuvant, L),get_code(Caracter1).        

pana_la_urmatorul_apostrof(Lista_caractere):-
get_code(Caracter),
(Caracter == 39,Lista_caractere=[Caracter];
Caracter\==39,
pana_la_urmatorul_apostrof(Lista_caractere1),
Lista_caractere=[Caracter|Lista_caractere1]).

citeste_cuvant(Caracter,Cuvant,Caracter1) :-          
caractere_in_interiorul_unui_cuvant(Caracter),!,              
((Caracter>64,Caracter<91),!,
Caracter_modificat is Caracter+32;
Caracter_modificat is Caracter),                              
citeste_intreg_cuvantul(Caractere,Caracter1),
name(Cuvant,[Caracter_modificat|Caractere]).  %name e echiv            

citeste_intreg_cuvantul(Lista_Caractere,Caracter1) :-
get_code(Caracter),
(caractere_in_interiorul_unui_cuvant(Caracter),
((Caracter>64,Caracter<91),!, 
Caracter_modificat is Caracter+32;
Caracter_modificat is Caracter),
citeste_intreg_cuvantul(Lista_Caractere1, Caracter1),
Lista_Caractere=[Caracter_modificat|Lista_Caractere1]; \+(caractere_in_interiorul_unui_cuvant(Caracter)),
Lista_Caractere=[], Caracter1=Caracter).

citeste_cuvant(_,Cuvant,Caracter1) :-                
get_code(Caracter),       
citeste_cuvant(Caracter,Cuvant,Caracter1). 

caracter_cuvant(C):-member(C,[44,59,58,63,33,46,41,40,91,93,123,125,61,45,62,95,47]).  %trecem codul pt toate caracterele speciale pe care le avem 

% am specificat codurile ASCII pentru , ; : ? ! . ) ] [ ( }{ : = - > _  / FACUT


caractere_in_interiorul_unui_cuvant(C):-
C>64,C<91;C>47,C<58;
C==45;C==95;C>96,C<123.
caracter_numar(C):-C<58,C>=48.

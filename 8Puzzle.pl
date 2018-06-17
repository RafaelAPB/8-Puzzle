/****************************************************/
/*******Projeto LP: ''O Puzzle de 8'' ***************/
/****************************************************/
/*Grupo 67: *****************************************/
/***Nº 80970 Rafael Belchior*************************/
/***Nº 81719 Ines Sequeira***************************/
/****************************************************/


/*************************************************************/	
/************* Solucao Manual*********************************/
														

/* mov_legal/4

O literal mov_legal(C1, M, P, C2) afirma que 
a configuracao C2 e obtida da configuracao C1, 
fazendo o movimento M, com a peca P. 
*/

mov_legal(C1, M, P, C2):-	M = 'c', 							/*Primeiro movimento a testar*/
							pos_zero(C1, Pos_z),				/*Encontra zero da configuracao*/
							pode_cima(Pos_z),					/*Verifica se o movimento e possivel*/
							Pos_peca is Pos_z + 3,				/*Calcula a posicao da peca a mover*/
							procura_peca(C1, Pos_peca, P),		/*Encontra o sitio da peca a mover*/
							move(C1, Pos_z, Pos_peca, P, C2). 	/*Realiza o movimento, a partir da configuracao C1, sabendo a posicao do zero, a posicao da peca a mover e a peca a mover*/

							
mov_legal(C1, M, P, C2):-	M = 'b', 
							pos_zero(C1, Pos_z),
							pode_baixo(Pos_z),
							Pos_peca is Pos_z - 3,
							procura_peca(C1, Pos_peca, P),
							move(C1, Pos_z, Pos_peca, P, C2).
							
mov_legal(C1, M, P, C2):-	M = 'e', 
							pos_zero(C1, Pos_z),
							pode_esq(Pos_z),
							Pos_peca is Pos_z + 1,
							procura_peca(C1, Pos_peca, P), 
							move(C1, Pos_z, Pos_peca, P, C2).

mov_legal(C1, M, P, C2):-	M = 'd', 									
							pos_zero(C1, Pos_z),						
							pode_dir(Pos_z),							
							Pos_peca is Pos_z - 1,						
							procura_peca(C1, Pos_peca, P),				
							move(C1, Pos_z, Pos_peca, P, C2).										
														
/* move/5

Troca a peca a mover pelo zero 
*/

move(C1, Pos_z, Pos_peca, P, C2):- move(C1, Pos_z, Pos_peca, P, C2, 1).
move([], _, _, _, [], _).
move([_|Ri], Pos_z, Pos_peca, P, [E|Rf], Indice):-		Indice =:= Pos_z,!,								/*Compara a posicao do tabuleiro com a posicao do zero*/
														E is P, 										/*Substitui pela peca a mover*/
														IndiceN is Indice + 1,							/*Percorre o resto do tabuleiro ate nao haver mais a percorrer*/
														move(Ri, Pos_z, Pos_peca, P, Rf, IndiceN).
		
move([_|Ri], Pos_z, Pos_peca, P, [E|Rf], Indice):-		Indice =:= Pos_peca, !,							/*Compara a posicao do tabuleiro com a posicao da peca*/
														E is 0,											/*Poe no sitio na peca que foi movida o 0*/
														IndiceN is Indice + 1,
														move(Ri, Pos_z, Pos_peca, P, Rf, IndiceN).
													
move([Ei|Ri], Pos_z, Pos_peca, P, [Ef|Rf], Indice):-	Ef is Ei,										/*Se o elemento Ei nao e zero nem a peca a mover, Ef e igual a Ei*/
														IndiceN is Indice + 1,							/*Segue para o proximo elemento*/
														move(Ri, Pos_z, Pos_peca, P, Rf, IndiceN).


/*pode/1

Verifica quando e possivel mover uma peca, segundo uma determinda direcao,
 para onde o zero se encontra
*/

pode_esq(Pos_z):- Pos_z mod 3 =\= 0. 		     /* so pode haver movimento para a esquerda se o zero nao estiver na posicao mais a direita 
												(nao existe uma peca a direita do zero para se mover para a esq), 
												ou seja a posicao do zero nao pode ser multiplo de 3 */



pode_dir(Pos_z):- (Pos_z - 1) mod 3 =\= 0. 		 /* analogo ao pode_esq */

pode_cima(Pos_z):- Pos_z =< 6.					 /* so pode haver movimento para cima se o zero nao estiver na posicao mais abaixo 
												(nao existe uma peca abaixo do zero para se mover para cima), 
												ou seja a posicao do zero nao maior que 6 (o zero nao pode estar na linha de baixo) */
												
pode_baixo(Pos_z):- Pos_z > 3.					 /* analogo ao pode_cima */


/*procura_peca/3 

Devolve qual a peca que esta na posicao Pos_peca do tabuleiro C 
*/

procura_peca(C, Pos_peca, P):- procura_peca(C, Pos_peca, P, 1).
procura_peca([E|_], Pos_peca, E, Pos_peca):- !.										/*Quando o elemento a considerar for igual ao procurado, a posicao da peca e o indice*/
procura_peca([_|R], Pos_peca, P, Indice):-	IndiceN is Indice + 1,					/*Incrementa o contador da posicao*/
											procura_peca(R, Pos_peca, P, IndiceN).



/* resolve_manual/2

Corresponde a aplicacao da solucao manual.
*/

resolve_manual(Ci, Cf):- 	escreve_transf(Ci, Cf),
							transformacao_possivel(Ci, Cf),				/*Verifica se e possivel resolver o puzzle*/
							resolve_manual(Ci,_, Cf),!.
							
resolve_manual(Cf, _, Cf):- writeln('Parabens!'),!.
resolve_manual(Ci, ConfParcial, Cf):- 	writeln('Qual o seu movimento?'),
										read(M),
										mov_legal(Ci, M, _, ConfParcial),
										nl, escreve_conf(ConfParcial), nl,
										resolve_manual(ConfParcial, _, Cf).	
										
resolve_manual(Ci, _, Cf):- writeln('Movimento ilegal'),
							resolve_manual(Ci, _, Cf).					/*Caso o movimento seja ilegal vai pedir outro movimento*/
								
								
/*************************************************************/	
/************* Solucao cega***********************************/


resolve_cego(Ci, Cf):-	escreve_transf(Ci, Cf), 
			 			transformacao_possivel(Ci,Cf),
						resolve_cego(Ci, Lista_mov, [Ci], Cf),
						escreve_mov(Lista_mov).
						
										
/*resolve_cego/4 
(Ci, Lista_mov , Lista_conf, Cf)
*/

resolve_cego(Cf, [], _, Cf).
resolve_cego(Ci, [[M,P]|Rm], Rc, Cf):-		mov_legal(Ci, M, P, ConfParcial),
											\+ membro(ConfParcial, Rc), 							/* se a conf pertencer a Lista_conf impede o ciclo */
											resolve_cego(ConfParcial,Rm,[ConfParcial|Rc], Cf).

											
/*************************************************************/	
/************* Solucao atraves de procura informada***********/

/*Definicao da estrutura no(C,F,G,H,M)*/
/*
C e a configuracao 
F e o numero de confgs feitas desde o estado incial
G e uma incognita
H e a quant de numeros fora do sitio
M e a lista que contem o movimento e peca feitos pelo estado anterior 
*/

cria_no(C,F,G,H,M, no(C,F,G,H,M)).
c_no(no(C,_,_,_,_),C).
f_no(no(_,F,_,_,_),F).
g_no(no(_,_,G,_,_),G).
h_no(no(_,_,_,H,_),H).
m_no(no(_,_,_,_,M),M).

resolve_info_h(Ci,Cf):- 	escreve_transf(Ci, Cf),
							transformacao_possivel(Ci,Cf),
							conta_h(Ci,Cf,H),				
							cria_no(Ci,H,0,H,[],No),			/*Cria no com determinado H*/				
							resolve_info_h([No],[],Cf).			/*resolve_info_h(ListaAberta,ListaFechados, Cf)*/							

resolve_info_h(ListaA, _, _):-	procura_menor_f(ListaA,No),
								h_no(No,0),					/*Se o h for 0, todas as pecas estao na posicao certa*/
								m_no(No,M),                 /*Arranja os movimentos feitos ate ao momento*/
								inverte(M,Mnovo),			/*E necessario inverter a lista de movimentos porque quando sao acrescentados os movs mais recentes sao colocados no inicio da lista*/
								escreve_mov(Mnovo), !.		/*Imprime a resposta*/

resolve_info_h(ListaA, ListaF, Cf):-	procura_menor_f(ListaA,No),										/*Procura no com menor f da lista de abertos*/
										remove_el(ListaA,No,ListaAInterm),								/*Remove no escolhido da lista de abertos*/
										expande(No,ListaNos, Cf),
										membro_listasAF(ListaNos, ListaA, ListaF, ListaNos_SemRep),		/*Devolve uma lista (ListaNos_SemRep) com os nos de ListaNos que nao se encontram na lista de abertos e na de fechados*/
										junta(ListaNos_SemRep, ListaAInterm, ListaANova),
										resolve_info_h(ListaANova,[No|ListaF], Cf).
										

/************* Auxiliares procura informada********************/


procura_menor_f([E|R],No):- f_no(E,F),							/*Procura no com menor f da lista de abertos*/
							procura_menor_f(R,No,F,E).
procura_menor_f([],No,_,No).
procura_menor_f([E|R],No,F_menor,_):- 	f_no(E,F),						
										F < F_menor, !,
										procura_menor_f(R,No,F,E).
												
procura_menor_f([_|R],No,F_menor,No_menor):- 	procura_menor_f(R,No,F_menor,No_menor).

/*remove/3
Remove o El de lista. El vai ser um no ambito do projeto
*/												
remove_el([],_,[]).															
remove_el([E1|R1],El,L):-	E1 = El, !,
							remove_el(R1,El,L).
remove_el([E1|R1],El,[E1|R2]):-	remove_el(R1,El,R2).

/*expande/3
Devolve uma lista com todos os nos expandidos a partir do No dado 
*/

expande(No,ListaNosExp, Cf) :-  expande(No,ListaNosExp,[],[c,b,e,d], Cf), !.
expande(_,_,_,[],_).
expande(No,[No2|R],Lmov, [M|Rm], Cf) :- c_no(No,C),
										mov_legal(C,M,P,C2)->
										(\+ membro(M,Lmov),
										g_no(No,G),
										Gn is G + 1,
										conta_h(C2,Cf,H),
										m_no(No,M_ant),
										F is Gn + H,
										cria_no(C2,F,Gn,H,[[M,P]|M_ant],No2),
										expande(No,R,[M|Lmov],Rm, Cf));
										expande(No,[No2|R],Lmov, Rm, Cf).


/* membro_listasAF/4

devolve uma lista (ListaNos_SemRep) com os nos de ListaNos que nao se encontram na lista de abertos e na de fechados
*/

membro_listasAF([], _, _, []).
membro_listasAF([No|R1], ListaA, ListaF, [No|R2]):-	\+ membro(No, ListaA),					
						     						\+ membro(No, ListaF), !,						/*Se nao estiver nem na lista de abertos nem na de fechados*/
													membro_listasAF(R1, ListaA, ListaF, R2).
membro_listasAF([_|R], ListaA, ListaF, L_nosN):-	membro_listasAF(R, ListaA, ListaF, L_nosN).


/*conta_h/3
Calcula o H para a configuracao 
*/

conta_h(Ci,Cf,H):- conta_h(Ci,Cf,0,H).
conta_h([],[],H,H).
conta_h([E1|R1],[E2|R2],Ac,H):- E1 =\= 0,				/*Ignorar 0*/
								E1 =\= E2, !,			/*Se nao for o elemento, percorre resto das listas*/
								Acn is Ac + 1, 		
								conta_h(R1,R2,Acn,H).
								
conta_h([_|R1],[_|R2],Ac,H):- conta_h(R1,R2,Ac,H).


/*************************************************************/	
/************* Creditos adicionais****************************/

/*transformacao_possivel/2

 devolve true se e possivel transformar Ci em Cf
 NOTA: Este algoritmo foi retirado da internet. Foi feita uma implementacao
		em Prolog para o puzzle de 8.
 */

transformacao_possivel(Ci,Cf) :-	remove_el(Ci,0,Ci_zero),							/*Zero e ignorado*/					
		    						remove_el(Cf,0,Cf_zero),!,
		  							transformacao_possivel(Ci_zero,Cf_zero,0),!.

transformacao_possivel([],_,Inversoes) :- Inversoes mod 2 =:= 0,!.								/*Se o numero de inversoes for par e possivel transformar uma configuracao noutra*/			
transformacao_possivel(Ci, [E1|R1], Inversoes):-	procura_peca(Ci, Pos_peca, E1),!,			/*Procura a peca que esta no inicio da lista final*/
													Inversoesn is Inversoes + Pos_peca - 1,			/*Verifica-se o numero de pecas atras da peca em questao*/
													remove_el(Ci,E1,Novo_Ci),						/*Tira-se o elemento testado, embora nao seja totalmente necessario*/
													transformacao_possivel(Novo_Ci, R1, Inversoesn),!.
	

/*************************************************************/	
/************* Predicados auxiliares**************************/
/*************************************************************/
/*************************************************************/		

/* escreve_transf/2 

Imprime a configuracao incial e final
Os elementos i correspondem ao primeiro tabuleiro e os elementos f ao segundo
*/

escreve_transf(Ci, Cf):- writeln('Transformacao desejada:'),
						 escreve_transf(Ci, Cf, 1).
						 
escreve_transf([], _, _).
escreve_transf([Ei1,Ei2,Ei3|Ri], [Ef1,Ef2,Ef3|Rf], Linha):- escreve_linha(Ei1, Ei2, Ei3),	/*imprime o primeiro tabuleiro, em tres iteracoes (linha por linha)*/
															ligacao(Linha), 				/*coloca seta*/
															escreve_linha(Ef1, Ef2, Ef3),	/*imprime o segundo tabuleiro*/
															nl,
															LinhaN is Linha + 1,
															escreve_transf(Ri, Rf, LinhaN),!.

/*ligacao/1

Imprime a seta caso a linha seja a segunda
*/

ligacao(Linha):- Linha =:= 2, !,
				 write(' -> ').
ligacao(_):- write('    ').


/*imprime_el/3

Imprime a linha atual
*/				

imprime_el(E1, E2, E3):-	write(' '), write(E1), write(' '),
							write(' '), write(E2), write(' '),
							write(' '), write(E3), write(' ').
							
			
/* escreve_conf/1

Imprime a configuracao dada */

escreve_conf([]).
escreve_conf([E1,E2,E3|R]):-	escreve_linha(E1, E2, E3), nl,
								escreve_conf(R).
								
/*escreve_linha/3

Verifica o que precisa ser impresso (elementos + espaco em branco)
*/				

escreve_linha(E1, E2, E3):-	E1 =:= 0,
							imprime_el(' ', E2, E3).
escreve_linha(E1, E2, E3):-	E2 =:= 0,
							imprime_el(E1, ' ', E3).
escreve_linha(E1, E2, E3):-	E3 =:= 0,
							imprime_el(E1, E2, ' ').
							
escreve_linha(E1, E2, E3):-	imprime_el(E1, E2, E3).


/*pos_zero/2

O literal devolve a posicao em que o zero esta na configuracao
*/

pos_zero(C, Pos):- pos_zero(C, 1, Pos).								/*Variavel auxiliar para indice instanciada a 1*/
pos_zero([P|_], Indice, Pos):-  P =:= 0, !,							
								Pos is Indice.
pos_zero([_|R], Indice, Pos):-	IndiceN is Indice + 1,				/*Se nao houver unificacao acima, incrementa-se o indice e procura-se o zero (usando o resto da lista)*/
								pos_zero(R, IndiceN, Pos).

/*inverte/2*/
inverte([], []).
inverte([P|R], I):- inverte(R, I1), junta(I1, [P], I).
	
						

/*membro/2*/
membro(E, [E|_]).
membro(E, [_|R]):- membro(E, R).

/*junta/3*/
junta([], L, L).
junta([P|R], L, [P|L2]):- junta(R, L, L2).

/*escreve_mov/1*/

escreve_mov([[M, P]|[]]):-  write('mova a peca '),
							write(P),
							escreve_direcao(M),
							writeln('.').
escreve_mov([[M, P]|R]):-   write('mova a peca '),
							write(P),
							escreve_direcao(M), nl,
							escreve_mov(R).

/*escreve_direcao/1*/

escreve_direcao(M):- 	M = 'd',
						write(' para a direita').
escreve_direcao(M):- 	M = 'e',
						write(' para a esquerda').
escreve_direcao(M):- 	M = 'c',
						write(' para cima').
escreve_direcao(M):- 	M = 'b',
						write(' para baixo').
									
/*************************************************************/

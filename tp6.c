#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include <MLV/MLV_all.h>

typedef unsigned long long Distance;

typedef struct ville{
	int x;
	int y;
	char* nom;
}VILLE;

typedef struct abrv{
	int coord_lineaire;	
	struct abrv* fg;
	struct abrv* fd;
}ABRV;

typedef struct individu{
	int* tab_ville;
	int taille_tab;
	Distance longueur_visite;
}INDIVIDU;

typedef struct population{
	INDIVIDU** tab_ind;
	int taille;
	unsigned int generation;
	int ameliorations;
}POPULATION;

int permutation(int tab[], int a, int b, int l){
	int i;
	int temp;
	for (i = 0; i < l; ++i){
		temp = tab[a + i];
		tab[a + i] = tab[b + i];
		tab[b + i] = temp;
	}
	return 1;
}

int mutation(INDIVIDU* indiv, int nb_mutation){
	int l, a, b, i;
	l = 0;
	for(i = 0; i < nb_mutation; ++i){
		l = (rand() % ((indiv->taille_tab) / 2)) + 1;
		a = rand() % ((indiv->taille_tab + 1) - (2 * l));
		b = (rand() % ((indiv->taille_tab + 1) - (a + 2 * l))) + a + l;
		permutation(indiv->tab_ville, a, b, l);
		indiv->longueur_visite = -1;
	}
	return l;
}

INDIVIDU* creerIndiv(int nb_indiv, int nb_ville){
	INDIVIDU* indiv = (INDIVIDU*) malloc(sizeof(INDIVIDU) * nb_indiv);
	if(indiv == NULL)
		return 0;
	int i, j;
	for(i = 0; i < nb_indiv; ++i){
		indiv[i].tab_ville = (int*) malloc(sizeof(int) * nb_ville);
		indiv[i].taille_tab = nb_ville;
		for(j = 0; j < nb_ville; ++j){
			indiv[i].tab_ville[j] = j;
		}
		indiv[i].longueur_visite = -1;
	}
	return indiv;
}

INDIVIDU** creerTabPtrIndiv(int nb_indiv, int nb_ville){
	INDIVIDU** tabPtrIndiv = (INDIVIDU**) malloc(sizeof(INDIVIDU*) * nb_indiv);
	if(tabPtrIndiv == NULL){
		return 0;
	}
	int i;
	for(i = 0; i < nb_indiv; ++i){
		tabPtrIndiv[i] = creerIndiv(1, nb_ville);
	}
	return tabPtrIndiv;
}

int melange(int tab[], int taille){
	int i, temp, index;
	for(i = 0; i < taille; ++i){
		index = rand() % taille;
		temp = tab[i];
		tab[i] = tab[index];
		tab[index] = temp;
	}
	return 1;
}

int melangePop(POPULATION* pop, int debut, int fin){
    int  i;
    for(i = debut; i < fin; ++i){
        melange(pop->tab_ind[i]->tab_ville, pop->tab_ind[i]->taille_tab);
		pop->tab_ind[i]->longueur_visite = -1;
    }
    return 1;
}

POPULATION* creerPopulation(int taille_pop, int nb_ville){
	POPULATION* pop = (POPULATION*) malloc(sizeof(POPULATION));
	if(pop == NULL)
		return 0;
	pop->taille = taille_pop;
	pop->tab_ind = creerTabPtrIndiv(taille_pop, nb_ville);
	return pop;
}

int evaluer_longueur_visite(INDIVIDU* indiv, VILLE tab[], int sqrt_mode){
	int i;
	Distance delta_x, delta_y;
	delta_x = 0;
	delta_y = 0;
	if(indiv->longueur_visite != -1)
		return 1;
	indiv->longueur_visite = 0;
	if(sqrt_mode == 1){
		for(i = 1; i < indiv->taille_tab; ++i){
			delta_x = (tab[indiv->tab_ville[i]]).x - (tab[indiv->tab_ville[i - 1]]).x;
			delta_y = (tab[indiv->tab_ville[i]]).y - (tab[indiv->tab_ville[i - 1]]).y;
			indiv->longueur_visite += sqrt((delta_x * delta_x) + (delta_y * delta_y));
		}
		return 1;
	}
	else{
		for(i = 1; i < indiv->taille_tab; ++i){
			delta_x = (tab[indiv->tab_ville[i]]).x - (tab[indiv->tab_ville[i - 1]]).x;
			delta_y = (tab[indiv->tab_ville[i]]).y - (tab[indiv->tab_ville[i - 1]]).y;
			indiv->longueur_visite += (delta_x * delta_x) + (delta_y * delta_y);
		}
	}
    return 1;
}

int compare(const void* a, const void* b){
	INDIVIDU** pb;
	INDIVIDU** pa;
	pa = (INDIVIDU**) a;
	pb = (INDIVIDU**) b;
	assert((*pa)->longueur_visite != -1);
	assert((*pb)->longueur_visite != -1);
	return (*pa)->longueur_visite - (*pb)->longueur_visite;
}

int copie_individu(INDIVIDU* src, INDIVIDU* dest){
	int i;
	for(i = 0; i < src->taille_tab; ++i){
		dest->tab_ville[i] = src->tab_ville[i];
	}
	return 1;
}

int tri_pop(POPULATION* pop, int debut, int fin, VILLE tab[]){
	int i;
	for(i = 0; i < pop->taille; ++i){
		evaluer_longueur_visite((pop->tab_ind[i]), tab, 0);
	}
	qsort(pop->tab_ind + debut, fin - debut, sizeof(INDIVIDU*), compare);
	return 1;
}

int fusion(POPULATION* pop, int beta){
	INDIVIDU* tab_fusion[beta];
	int i, debut1, debut2;
	debut1 = 0;
	debut2 = beta;
	for(i = 0; i < beta; ++i){
		if(pop->tab_ind[debut1]->longueur_visite < pop->tab_ind[debut2]->longueur_visite){
			tab_fusion[i]= pop->tab_ind[debut1];
			++debut1;
		}
		else{
			tab_fusion[i] = pop->tab_ind[debut2];
			++debut2;
		}
	}
	for(i = 0; i < beta - debut1; ++i){
        pop->tab_ind[i + beta] = pop->tab_ind[i + debut1];
	}
	for(i = 0; i < beta; ++i){
        pop->tab_ind[i] = tab_fusion[i];
	}
	return 1;
}

int tri_partiel(POPULATION* pop, int beta, VILLE tab[]){
	tri_pop(pop, beta, pop->taille, tab);
	fusion(pop, beta);
	return 1;
}

int generation_suivante(POPULATION* pop, int beta, int alpha, int gamma){
	assert((alpha + beta + gamma) == pop->taille);
	int i;
	if(beta < alpha){
		i = alpha;
	}
	else{
		i = beta;
	}
	for(;i < alpha; ++i){
		copie_individu((pop->tab_ind[i - alpha]), (pop->tab_ind[i]));
		mutation((pop->tab_ind[i]), i/10 + 1);
	}
	for(i = alpha;i < alpha + beta; ++i){
		copie_individu((pop->tab_ind[i - alpha]), (pop->tab_ind[i]));
		mutation((pop->tab_ind[i]), 1);
	}
	for(;i < alpha + beta; ++i){
		copie_individu((pop->tab_ind[i - alpha]), (pop->tab_ind[i]));
		mutation((pop->tab_ind[i]), 1);
	}
	melangePop(pop, alpha + beta - 1, pop->taille);
	return 1;
}

int init_char_tab(char* tab, int taille){
	int i;
	for(i = 0; i < taille; ++i){
		tab[i] = '\0';
	}
	return 1;
}

ABRV* creer_abrv(int coord_lin){
	ABRV* arbre;
	arbre = (ABRV*) malloc(sizeof(ABRV));
	if(arbre == NULL){
		fprintf(stderr, "erreur memoire\n");
		exit(1);
	}
	arbre->fg = NULL;
	arbre->fd = NULL;
	arbre->coord_lineaire = coord_lin;
	return arbre;
}

int est_present_abrv(ABRV* arbre, int coord_lin){
	if(arbre == NULL)
		return 0;

	if(arbre->coord_lineaire == coord_lin)
		return 1;

	if(arbre->coord_lineaire < coord_lin)
		return est_present_abrv(arbre->fd, coord_lin);

	return est_present_abrv(arbre->fg, coord_lin);
}

int ajout_ville_abrv(ABRV* arbre, int coord_lin){
	if(arbre == NULL)
		return 0;

	if(arbre->coord_lineaire < coord_lin){
		if(arbre->fd == NULL){
			arbre->fd = creer_abrv(coord_lin);
			return 1;
		}
		else
			return ajout_ville_abrv(arbre->fd, coord_lin);
	}
	else{
		if(arbre->fg == NULL){
			arbre->fg = creer_abrv(coord_lin);
			return 1;
		}
		else
			return ajout_ville_abrv(arbre->fg, coord_lin);
	}
		
}

int freeArbre(ABRV* a){
    if(a == NULL){
        return 1;
    }
    freeArbre(a->fg);
    freeArbre(a->fd);
    free(a);
    return 1;
}

VILLE* creerVille(int nb_ville){
	VILLE* tab = (VILLE*) malloc(sizeof(VILLE) * nb_ville);
	char numero[4];
	init_char_tab(numero, 4);
	int i, x, y, coord_lin;
	ABRV* arbre;
	arbre = creer_abrv(-1);
	for(i = 0; i < nb_ville; ++i){
		do{
			x = rand() % 1000;
			y = rand() % 1000;
			coord_lin = y * 1000 + x;
		}while(est_present_abrv(arbre, coord_lin) == 1);
		ajout_ville_abrv(arbre, coord_lin);
		tab[i].x = x;
        tab[i].y = y;
		tab[i].nom = (char*) malloc(sizeof(char) * 15);
		init_char_tab(tab[i].nom, 15);
		strcpy(tab[i].nom, "nom_ville");
		sprintf(numero,"%d", i);
		strcpy(tab[i].nom + 9, numero);
	}
	freeArbre(arbre);
	return tab;
}

int afficher_ville(VILLE* tab, int taille){
	int i;
	for(i = 0; i < taille; ++i){
		printf("%s %d %d\n", tab[i].nom, tab[i].x, tab[i].y);
	}
	return 1;
}

int afficher_indiv_graph(INDIVIDU* indiv, VILLE* tab){
	int i;
	MLV_clear_window(MLV_COLOR_BLACK);
	MLV_draw_filled_circle(tab[indiv->tab_ville[0]].x, tab[indiv->tab_ville[0]].y, 4,MLV_COLOR_RED);
	for(i = 1; i < indiv->taille_tab; ++i){
		MLV_draw_line(tab[indiv->tab_ville[i - 1]].x, tab[indiv->tab_ville[i - 1]].y, tab[indiv->tab_ville[i]].x, tab[indiv->tab_ville[i]].y, MLV_COLOR_WHITE);
		MLV_draw_filled_circle(tab[indiv->tab_ville[i]].x, tab[indiv->tab_ville[i]].y, 4, MLV_COLOR_BLUE);
	}
	MLV_draw_filled_circle(tab[indiv->tab_ville[i - 1]].x, tab[indiv->tab_ville[i - 1]].y, 4,MLV_COLOR_RED);
	MLV_actualise_window();
	return i;
}

int afficher_indiv(INDIVIDU* indiv, VILLE* tab, int affiche_visite, FILE* out){
	int i;
	if(out == NULL){
		out = stdout;
	}
	if(affiche_visite == 1){
		if(tab != NULL){
			for(i = 0; i < indiv->taille_tab; ++i){
				fprintf(out, "%s ", tab[indiv->tab_ville[i]].nom);
				fprintf(out, "%d ", tab[indiv->tab_ville[i]].x);
				fprintf(out, "%d\n", tab[indiv->tab_ville[i]].y);
			}
		}
		else{
			for(i = 0; i < indiv->taille_tab; ++i){
				fprintf(out, "%d ", indiv->tab_ville[i]);
			}
		}
		fprintf(out, "\n%d villes", indiv->taille_tab);
	}
	int* longueur;
	longueur = (int*) &(indiv->longueur_visite);
	fprintf(out, "\n%d distance\n", longueur[0]);
	return 1;
}

int afficher_pop(POPULATION* pop){
    int i;
    for(i = 0; i < pop->taille; ++i){
        afficher_indiv((pop->tab_ind[i]), NULL, 0, stdout);
    }
    return 1;
}

int free_individu(INDIVIDU* indiv){
    free(indiv->tab_ville);
    free(indiv);
    return 1;
}

int free_pop(POPULATION* pop){
    int i;
    for(i = pop->taille - 1; i >= 0; --i){
        free_individu((pop->tab_ind[i]));
    }
    free(pop->tab_ind);
    free(pop);
    return 1;
}

int free_ville(VILLE* v){
    free(v->nom);
    return 1;
}

int free_tab_ville(VILLE* tab, int taille){
    int i;
    for(i = 0; i < taille; ++i){
        free_ville(&(tab[i]));
    }
    free(tab);
    return 1;
}

int ecrire_individu(POPULATION* pop, VILLE* tab, time_t t,FILE* out){
	assert(pop != NULL);
	time_t mtn;
	if(out == NULL){
		out = fopen("PVC.txt","w");
	}
	afficher_indiv(pop->tab_ind[0], tab, 1, out);
	time(&mtn);
	fprintf(out, "%d generation \n%li sec\n%d ameliorations\n", pop->generation, mtn - t, pop->ameliorations);
	fprintf(out, "%d ratio gen/amelio\n", pop->generation/pop->ameliorations);
	fclose(out);
	return 1;
}

int eval_arg(char** argv, int argc,int* nb_ville, int* nb_indiv, int* alpha, int* beta, int* gamma){
    if(argc == 1 || argc > 3){
        fprintf(stderr, "trop ou pas assez d'argument.\nVeuillez relancer le programme avec des parametres valide");
        return 0;
    }
    *nb_indiv = atoi(argv[1]);
    *nb_ville = atoi(argv[2]);
    if((*nb_indiv) < 3 || (*nb_ville) < 3){
        fprintf(stderr, "Veuillez relancer le programme avec des parametres valide");
        return 0;
    }
    *beta =  (*nb_indiv) / 3;
    *alpha = (*beta) * 2;
    *gamma = (*nb_indiv) - ((*beta) + (*alpha));
	return 1;
}

int main(int argc, char* argv[]){
	MLV_create_window("pvc", "", 1000, 1000);
	time_t tps;
	time(&tps);
	int nb_ville, nb_indiv, alpha, beta, gamma;
    eval_arg(argv, argc, &nb_ville, &nb_indiv, &alpha, &beta, &gamma);

	INDIVIDU* temp;


	srand(0);

	POPULATION* pop;
	pop = creerPopulation(nb_indiv, nb_ville);
	temp = pop->tab_ind[0];

	VILLE* tab;
	tab = creerVille(nb_ville);

	temp = pop->tab_ind[0];


	melangePop(pop, 0, pop->taille);
	tri_pop(pop, 0, pop->taille, tab);

    while(MLV_get_keyboard_state( MLV_KEYBOARD_e ) != MLV_PRESSED && MLV_get_mouse_button_state( MLV_BUTTON_LEFT ) != MLV_PRESSED){
        tri_partiel(pop, beta, tab);
        if(pop->tab_ind[0] != temp){
        	temp = pop->tab_ind[0];
        	afficher_indiv(temp, NULL, 0, stdout);
        	afficher_indiv_graph(temp, tab);
        	pop->ameliorations += 1;
        }
		generation_suivante(pop, beta, alpha, gamma);
		pop->generation += 1;
	}
	pop->tab_ind[0]->longueur_visite = -1;
	evaluer_longueur_visite((pop->tab_ind[0]), tab, 1);
    afficher_indiv(pop->tab_ind[0], tab, 0, stdout);
    ecrire_individu(pop, tab, tps, NULL);
    free_pop(pop);
	free_tab_ville(tab, nb_ville);
	MLV_free_window();
	
	return 0;
}

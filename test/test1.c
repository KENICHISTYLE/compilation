

int wudi;//test pour variable glob
void fun(char k){
int f;
void x;
 return;	
}

//marche pas pour type null
struct s {
 int x;
 char z;
};
/*struct s {int x ;};*/
int funx(int i )
{
  struct s x;
  int x;
 // char x;
  return x ;
}//pour fonction  marche pas -> type de retour <> type de declaration de fonction 
int funy(char x)
{
void xixi;
}// marche

union s {
 int x;
 char x;
};//cas marche pas pour declaration

// cree une erreur car il y a une structure deja declarer
/*
struct s {
 char e;
};
*/

void main (int argc,char** argv) 
{
 // declaration
 int i,j; 
 //int i; pour dupliquer la declaration variable
 int v;
 char* c;
 char k;
 int *toto;
 char test_logique;
 union s un;
 struct s st;  
 struct s * d;
 int ** test_ptr_ptr;
 // qulque operateur
 *toto = (*toto)+i;
  i = i;
  //i = (*test_ptr_ptr*i); marche pour la pointeur de pointeur
  i = (*(&(**(test_ptr_ptr))))*i;
  i = 'i';
  c = &k;
  i = i++;
  j = i--;
  v = i * j;
  v = wudi;
 // v= haha; pour non declaration marche
  //test de l'acces
  un.x = 'r';// cas marche pas
  st.z = 'r';
  //test adr  
  d = &st;
  k = d->z;
 	i = i;
 // appel de fonction
  fun(k);
 //	fun(un); // cas marche pour faut parametre
// un = fun(k); marche pour reture des type
  // fun(c);
  // fun(k,i);
 //test pour putchar
  putchar(1);
 /* test du if et if else*/
 if ( v = i) k = 't';
 if (toto) ;else i--;
 //test des boucles
 for (i=0;i<2;i++){};
 for (;;){};
 while (i == j);
 // test du retour de fonction
 return un ;
}


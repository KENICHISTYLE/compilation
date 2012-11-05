
void fun(char k){
 return;	
}

struct s {
 int x;
 char z;
};

union s {
 int x;
 char z;
};

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
 int v;
 char* c;
 char k;
 union s un;
 struct s st;  
 struct s * d;
 // qulque operateur
  i = 0;
  i = 5;
  c = &k;
  i = i++;
  j = i--;
  v = i * j;
  //test de l'acces
  un.x = 1;
  st.z = 'r';
  //test adr  
  d = &st;
  k = d->z;
 // appel de fonction
  fun(k); 
 //test pour putchar
  putchar(1);
 /* test du if et if else*/
 if ( v = i) k = 't';
 if (1) ;else i--;
 //test des boucles
 for (i=0;i<2;i++){};
 while (i == j);
 // test du retour de fonction
 return ;
}


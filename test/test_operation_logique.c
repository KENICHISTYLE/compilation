
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
 
 union s un;
 // qulque operateur
 
  v = (un&&i);
  
 return ;
}


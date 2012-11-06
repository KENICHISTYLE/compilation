

struct s {
int x;
char z;
};


int main (int argc, char ** argv)
{

struct s ** pp;
struct s *  p;
struct s    v;

p = &v;
pp = &p;
v = *p;
return 0;
}

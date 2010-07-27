#pragma no_other_abandoned

struct A {
 int i;
 char c;
};

int main(){
 struct A a[2];

 a[0].i = 0;
 a[0].c = 'a';
 a[1].i = 1;
 a[1].c = 'a';

 int i = __SYMBOLIC() % 2;

 char c = a[i].c;

 __ASSERT(c == 'a');

 return 0;
}

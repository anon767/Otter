#pragma no_other_abandoned

void __SYMBOLIC(void *);

struct A {
 int i;
 char c;
};

int main(){
 struct A a[2] = {{0, 'a'}, {1, 'b'}};

 int i;
 __SYMBOLIC(&i);
 i %= 2;

 struct A b = {8, 'Z'};

 a[i] = b;

 char c = ((char*)(a))[i*sizeof(b)+sizeof(i)];

 __ASSERT(c == 'Z');

 return 0;
}

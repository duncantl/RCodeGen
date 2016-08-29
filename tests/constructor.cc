
class Foo {
 
public:
    Foo() {}
    Foo(int x) { a = x;}

    int bar();
    static int foo(int x, double y);

    double def(int x, double y = 2.0, char *str = "xyz");
protected:
    int a;
};

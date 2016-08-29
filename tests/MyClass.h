class MyClass {

public:
    virtual int a(int x);
    virtual int a(int x, double y);
    virtual double b(int x, double y);
    virtual void d(int x);
    virtual void c(int x) = 0;
    virtual int  e(int x) = 0;

    static char *getName(int other);
};

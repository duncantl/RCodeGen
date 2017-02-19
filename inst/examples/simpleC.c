

int
foo(int x, int y)
{
    return(x + y);
}


int
bar(int *x, int len)
{
    x[0] = len;
    return(0);
}


int
cbar(const int *x, int len)
{
    return(x[0] + len);
}


int *
ptr(int len)
{
    int *x = (int*) malloc(sizeof(len) * sizeof(int));
    return(x);
}


void
byref()
{

}


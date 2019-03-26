#include <iostream>
#include <jazz-jit.hpp>
void *
malloc(int size);

using namespace jazz_jit;

int (*f)(void);

typedef union {
  double input;
  long output;
} conv;

typedef union {
  long input;
  double output;
} conv2;

double
double_from_long(long i)
{
  conv2 c;
  c.input = i;
  return c.output;
}

int main()
{
}
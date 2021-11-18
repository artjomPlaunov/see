int main() {
  int e[3] = {5,6,7};
  int a = 1, b = -1;
  int product = a;
  int inc;
  int i;
  int j;
  if (e[0] > 0) {
    product = a;
  } else {
    product = b;
  }
  for (i=0; i <= 2; i=i+1) {
    inc = i + 5;
    product = product * e[inc];
  }
  return 0;
}

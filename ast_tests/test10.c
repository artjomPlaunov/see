int test(int a, int b) {
  int c = 0;
  c = a - b;
  c = a + b; 
  c = a * b;
  return c;
} 

int main() {
  int a[5] = {1,2,3,4,5};
  test(a[0], 100);
  return 0;
}

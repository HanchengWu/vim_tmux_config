#include <stdio.h>
#include <sys/types.h>
extern int test_05(int);

int test_function(int x)
{
  int j;
  j=0;
  while(x>0) {
    x--;
    if (x>10) continue; else ;
    j++;
  }
  return j;
}

int main()
{
  
  int i, j, k;
  int errors=0;
  int success=0;

  for (i=-2; i<20; i++)
	if (test_05(i)!=test_function(i)){
//	 	printf("\nerror at iteration:%d\n",i);
		 errors++;
	}
	else{

//	 	printf("\nOK at iteration:%d\n",i);
	  success++;
	}

  printf("success,%d\nerrors,%d\ntotal,%d\n",success,errors,success+errors);

  return 0;
}

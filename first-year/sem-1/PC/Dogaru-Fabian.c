#include <stdio.h>

int prim(int n)
{int ok=1;
  for(int i=2;i*i<=n;++i)
    if(n%i==0)
      ok=0;
  return ok;
}

int suma_cif(int n)
{int nr=0;
  while(n)
    {nr=nr+n%10;
      n=n/10;
    }
  return nr;
}

int main()
{int n;
  printf("Citeste un numar: ");
  scanf("%d",&n);
  if(prim(n)!=1 || n==1)
    printf("Numarul nu este special");
  else
    {int scif=suma_cif(n);
      if(prim(scif)==1)
	printf("Numarul este special");
      else printf("Numarul nu este special");
    }
  return 0;
}

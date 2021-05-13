
/* 2012/08/18 Yamashita */

int convend_(char buf[], char bufi[]){
  int i;
  int bufsize=sizeof(buf);
  for (i=0; i<=bufsize/2-1; i++){
    buf[i] = bufi[bufsize/2-i-1];
  }
}


/* 2012/08/18 Yamashita */
/* 2026/08/28: fixed two bugs found under a modern 64-bit gcc/gfortran:
   1) sizeof() on a function parameter declared as an array type
      returns the POINTER size (8 bytes on every current 64-bit
      target), not the caller's real buffer size, so the old
      2-argument form always byte-swapped 8 bytes regardless of
      whether util.f90's conv_r4_endian (real*4, 4 bytes) or
      conv_r8_endian (real*8, 8 bytes) was calling it. For real*4
      that reads/writes past the end of the actual variable:
      undefined behavior, silently corrupting adjacent stack memory,
      or a runtime "stack smashing detected" abort under
      -fstack-protector (the default in most current gcc/gfortran
      builds). It happened to look correct only for real*8 on 64-bit
      platforms, since sizeof(pointer) == 8 == sizeof(double) there
      by coincidence.
   2) independent of (1), the loop only reversed the first half of
      the buffer into itself (i=0..n/2-1, reading bufi[n/2-i-1])
      instead of a full n-byte reversal, so even given the correct n
      it produced the wrong byte order; compare with the correct
      algorithm in ~/fortran_grads/convend/convend_f/convend.f90.
   the byte count is now an explicit 3rd argument instead of being
   inferred via sizeof(); see util.f90's conv_r4_endian/conv_r8_endian
   for the updated call. */

int convend_(char *buf, const char *bufi, const int *nbyte){
  int i, n = *nbyte;
  for (i = 0; i < n; i++){
    buf[i] = bufi[n-i-1];
  }
  return 0;
}

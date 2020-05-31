
extern long i0, i1, i2, i3, i4, i5, i6, i7, i8;

extern void callee_0(void);
extern void callee_1(long i0);
extern void callee_2(long i0, long i1);
extern void callee_9(long i0, long i1, long i2, long i3, long i4, long i5, long i6, long i7, long i8);
extern void old_callee_9();


void call_0(void)
{
  callee_0();
}

void call_1(void)
{
  callee_1(i0);
}

void call_2(void)
{
  callee_2(i0, i1);
}

void call_9(void)
{
  callee_9(i0, i1, i2, i3, i4, i5, i6, i7, i8);
}

void old_call_9(void)
{
  old_callee_9(i0, i1, i2, i3, i4, i5, i6, i7, i8);
}

extern float f0, f1, f2, f3, f4, f5, f6, f7, f8;
extern double d0, d1, d2, d3, d4, d5, d6, d7, d8, d9;


extern void callee_2f_2i(float f0, float f1, long i0, long i1, long i2, long i3, long i4);
extern void callee_2d_2i(double d0, double d1, long i0, long i1, long i2, long i3, long i4);
extern void old_callee_2f_2i();
extern void old_callee_2d_2i();
extern void callee_i_2f_4i(long i0, float f0, float f1, int i1, long i2, long i3, long i4);
extern void old_callee_i_2f_4i();
extern void var_callee_i_2f_4i(long i0, ...);
extern void callee_9d(double d0, double d1, double d2, double d3, double d4, double d5, double d6, double d7, double d8);
extern void callee_9f(float f0, float f1, float f2, float f3, float f4, float f5, float f6, float f7, float f8);
extern void old_callee_9f();
extern void old_callee_9d();

void call_2f_2i(void)
{
  callee_2f_2i(f0, f1, i0, i1, i2, i3, i4);
}

void old_call_2f_2i(void)
{
  old_callee_2f_2i(f0, f1, i0, i1, i2, i3, i4);
}

void call_i_2f_4i(void)
{
  callee_i_2f_4i(i0, f0, f1, i1, i2, i3, i4);
}

void var_call_i_2f_4i(void)
{
  var_callee_i_2f_4i(i0, f0, f1, i1, i2, i3, i4);
}

void old_call_i_2f_4i(void)
{
  old_callee_i_2f_4i(i0, f0, f1, i1, i2, i3, i4);
}

void call_2d_2i(void)
{
  callee_2d_2i(d0, d1, i0, i1, i2, i3, i4);
}

void old_call_2d_2i(void)
{
  old_callee_2d_2i(d0, d1, i0, i1, i2, i3, i4);
}

void call_9d(void)
{
  callee_9d(d0, d1, d2, d3, d4, d5, d6, d7, d8);
}

void old_call_9d(void)
{
  old_callee_9d(d0, d1, d2, d3, d4, d5, d6, d7, d8);
}

void call_9f(void)
{
  callee_9f(f0, f1, f2, f3, f4, f5, f6, f7, f8);
}

void old_call_9f(void)
{
  old_callee_9f(f0, f1, f2, f3, f4, f5, f6, f7, f8);
}

extern void callee_1f_1d_2f(float f0, double d1, float f2, float f3);

void call_1f_1d_2f(void)
{
  callee_1f_1d_2f(f0, d1, f2, f3);
}

extern long long ll0, ll1, ll2;

extern void callee_1l_1ll_2l(long i0, long long ll1, long l2, long l3);

void call_1l_1ll_2l(void)
{
  callee_1l_1ll_2l(i0, ll1, i2, i3);
}

extern void callee_9d_1f_1d_2f(double d0, double d1, double d2, double d3, double d4, double d5, double d6, double d7, double d8, float f9, double d10, float f11, float f12);

void call_9d_1f_1d_2f(void)
{
  callee_9d_1f_1d_2f(d0, d1, d2, d3, d4, d5, d6, d7, d8, f0, d9, f1, f2);
}


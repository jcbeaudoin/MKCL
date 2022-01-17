/* -*- mode: c -*- */
/*
    cfun_dispatch.c -- Trampolines for functions
*/

static mkcl_object dispatch0 (MKCL, mkcl_narg narg) {
  mkcl_object fun = env->function;
  if (narg != 0) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env);
}

static mkcl_object dispatch1 (MKCL, mkcl_narg narg, mkcl_object x0) {
  mkcl_object fun = env->function;
  if (narg != 1) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0);
}

static mkcl_object dispatch2 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1) {
  mkcl_object fun = env->function;
  if (narg != 2) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1);
}

static mkcl_object dispatch3 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2) {
  mkcl_object fun = env->function;
  if (narg != 3) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2);
}

static mkcl_object dispatch4 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3) {
  mkcl_object fun = env->function;
  if (narg != 4) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3);
}

static mkcl_object dispatch5 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4) {
  mkcl_object fun = env->function;
  if (narg != 5) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4);
}

static mkcl_object dispatch6 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5) {
  mkcl_object fun = env->function;
  if (narg != 6) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5);
}

static mkcl_object dispatch7 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6) {
  mkcl_object fun = env->function;
  if (narg != 7) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6);
}

static mkcl_object dispatch8 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7) {
  mkcl_object fun = env->function;
  if (narg != 8) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7);
}

static mkcl_object dispatch9 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8) {
  mkcl_object fun = env->function;
  if (narg != 9) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8);
}

static mkcl_object dispatch10 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9) {
  mkcl_object fun = env->function;
  if (narg != 10) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9);
}

static mkcl_object dispatch11 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10) {
  mkcl_object fun = env->function;
  if (narg != 11) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10);
}

static mkcl_object dispatch12 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11) {
  mkcl_object fun = env->function;
  if (narg != 12) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11);
}

static mkcl_object dispatch13 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12) {
  mkcl_object fun = env->function;
  if (narg != 13) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12);
}

static mkcl_object dispatch14 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13) {
  mkcl_object fun = env->function;
  if (narg != 14) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13);
}

static mkcl_object dispatch15 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14) {
  mkcl_object fun = env->function;
  if (narg != 15) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14);
}

static mkcl_object dispatch16 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15) {
  mkcl_object fun = env->function;
  if (narg != 16) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15);
}

static mkcl_object dispatch17 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16) {
  mkcl_object fun = env->function;
  if (narg != 17) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16);
}

static mkcl_object dispatch18 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17) {
  mkcl_object fun = env->function;
  if (narg != 18) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17);
}

static mkcl_object dispatch19 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18) {
  mkcl_object fun = env->function;
  if (narg != 19) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18);
}

static mkcl_object dispatch20 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19) {
  mkcl_object fun = env->function;
  if (narg != 20) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19);
}

static mkcl_object dispatch21 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20) {
  mkcl_object fun = env->function;
  if (narg != 21) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20);
}

static mkcl_object dispatch22 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21) {
  mkcl_object fun = env->function;
  if (narg != 22) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21);
}

static mkcl_object dispatch23 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22) {
  mkcl_object fun = env->function;
  if (narg != 23) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22);
}

static mkcl_object dispatch24 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23) {
  mkcl_object fun = env->function;
  if (narg != 24) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23);
}

static mkcl_object dispatch25 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24) {
  mkcl_object fun = env->function;
  if (narg != 25) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24);
}

static mkcl_object dispatch26 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25) {
  mkcl_object fun = env->function;
  if (narg != 26) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25);
}

static mkcl_object dispatch27 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26) {
  mkcl_object fun = env->function;
  if (narg != 27) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26);
}

static mkcl_object dispatch28 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27) {
  mkcl_object fun = env->function;
  if (narg != 28) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27);
}

static mkcl_object dispatch29 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28) {
  mkcl_object fun = env->function;
  if (narg != 29) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28);
}

static mkcl_object dispatch30 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29) {
  mkcl_object fun = env->function;
  if (narg != 30) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29);
}

static mkcl_object dispatch31 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30) {
  mkcl_object fun = env->function;
  if (narg != 31) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30);
}

static mkcl_object dispatch32 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31) {
  mkcl_object fun = env->function;
  if (narg != 32) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31);
}

static mkcl_object dispatch33 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32) {
  mkcl_object fun = env->function;
  if (narg != 33) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32);
}

static mkcl_object dispatch34 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33) {
  mkcl_object fun = env->function;
  if (narg != 34) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33);
}

static mkcl_object dispatch35 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34) {
  mkcl_object fun = env->function;
  if (narg != 35) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34);
}

static mkcl_object dispatch36 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35) {
  mkcl_object fun = env->function;
  if (narg != 36) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35);
}

static mkcl_object dispatch37 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36) {
  mkcl_object fun = env->function;
  if (narg != 37) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36);
}

static mkcl_object dispatch38 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37) {
  mkcl_object fun = env->function;
  if (narg != 38) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37);
}

static mkcl_object dispatch39 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38) {
  mkcl_object fun = env->function;
  if (narg != 39) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38);
}

static mkcl_object dispatch40 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39) {
  mkcl_object fun = env->function;
  if (narg != 40) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39);
}

static mkcl_object dispatch41 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40) {
  mkcl_object fun = env->function;
  if (narg != 41) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40);
}

static mkcl_object dispatch42 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41) {
  mkcl_object fun = env->function;
  if (narg != 42) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41);
}

static mkcl_object dispatch43 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42) {
  mkcl_object fun = env->function;
  if (narg != 43) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42);
}

static mkcl_object dispatch44 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43) {
  mkcl_object fun = env->function;
  if (narg != 44) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43);
}

static mkcl_object dispatch45 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44) {
  mkcl_object fun = env->function;
  if (narg != 45) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44);
}

static mkcl_object dispatch46 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45) {
  mkcl_object fun = env->function;
  if (narg != 46) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45);
}

static mkcl_object dispatch47 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46) {
  mkcl_object fun = env->function;
  if (narg != 47) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46);
}

static mkcl_object dispatch48 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47) {
  mkcl_object fun = env->function;
  if (narg != 48) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47);
}

static mkcl_object dispatch49 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48) {
  mkcl_object fun = env->function;
  if (narg != 49) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48);
}

static mkcl_object dispatch50 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49) {
  mkcl_object fun = env->function;
  if (narg != 50) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49);
}

static mkcl_object dispatch51 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50) {
  mkcl_object fun = env->function;
  if (narg != 51) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50);
}

static mkcl_object dispatch52 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51) {
  mkcl_object fun = env->function;
  if (narg != 52) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51);
}

static mkcl_object dispatch53 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52) {
  mkcl_object fun = env->function;
  if (narg != 53) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52);
}

static mkcl_object dispatch54 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53) {
  mkcl_object fun = env->function;
  if (narg != 54) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53);
}

static mkcl_object dispatch55 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54) {
  mkcl_object fun = env->function;
  if (narg != 55) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54);
}

static mkcl_object dispatch56 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54, mkcl_object x55) {
  mkcl_object fun = env->function;
  if (narg != 56) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55);
}

static mkcl_object dispatch57 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54, mkcl_object x55, mkcl_object x56) {
  mkcl_object fun = env->function;
  if (narg != 57) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56);
}

static mkcl_object dispatch58 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54, mkcl_object x55, mkcl_object x56, mkcl_object x57) {
  mkcl_object fun = env->function;
  if (narg != 58) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57);
}

static mkcl_object dispatch59 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54, mkcl_object x55, mkcl_object x56, mkcl_object x57, mkcl_object x58) {
  mkcl_object fun = env->function;
  if (narg != 59) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58);
}

static mkcl_object dispatch60 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54, mkcl_object x55, mkcl_object x56, mkcl_object x57, mkcl_object x58, mkcl_object x59) {
  mkcl_object fun = env->function;
  if (narg != 60) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59);
}

static mkcl_object dispatch61 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54, mkcl_object x55, mkcl_object x56, mkcl_object x57, mkcl_object x58, mkcl_object x59, mkcl_object x60) {
  mkcl_object fun = env->function;
  if (narg != 61) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60);
}

static mkcl_object dispatch62 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54, mkcl_object x55, mkcl_object x56, mkcl_object x57, mkcl_object x58, mkcl_object x59, mkcl_object x60, mkcl_object x61) {
  mkcl_object fun = env->function;
  if (narg != 62) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61);
}

static mkcl_object dispatch63 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54, mkcl_object x55, mkcl_object x56, mkcl_object x57, mkcl_object x58, mkcl_object x59, mkcl_object x60, mkcl_object x61, mkcl_object x62) {
  mkcl_object fun = env->function;
  if (narg != 63) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62);
}

#if 0
static mkcl_object dispatch64 (MKCL, mkcl_narg narg, mkcl_object x0, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4, mkcl_object x5, mkcl_object x6, mkcl_object x7, mkcl_object x8, mkcl_object x9, mkcl_object x10, mkcl_object x11, mkcl_object x12, mkcl_object x13, mkcl_object x14, mkcl_object x15, mkcl_object x16, mkcl_object x17, mkcl_object x18, mkcl_object x19, mkcl_object x20, mkcl_object x21, mkcl_object x22, mkcl_object x23, mkcl_object x24, mkcl_object x25, mkcl_object x26, mkcl_object x27, mkcl_object x28, mkcl_object x29, mkcl_object x30, mkcl_object x31, mkcl_object x32, mkcl_object x33, mkcl_object x34, mkcl_object x35, mkcl_object x36, mkcl_object x37, mkcl_object x38, mkcl_object x39, mkcl_object x40, mkcl_object x41, mkcl_object x42, mkcl_object x43, mkcl_object x44, mkcl_object x45, mkcl_object x46, mkcl_object x47, mkcl_object x48, mkcl_object x49, mkcl_object x50, mkcl_object x51, mkcl_object x52, mkcl_object x53, mkcl_object x54, mkcl_object x55, mkcl_object x56, mkcl_object x57, mkcl_object x58, mkcl_object x59, mkcl_object x60, mkcl_object x61, mkcl_object x62, mkcl_object x63) {
  mkcl_object fun = env->function;
  if (narg != 64) mkcl_FEwrong_num_arguments(env, fun, fun->cfun.narg, fun->cfun.narg, narg);
  return fun->cfun.old_entry_fixed(env, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63);
}
#endif

static const mkcl_objectfn const mkcl_cfun_dispatch_table[64] = {
(mkcl_objectfn)dispatch0, 
(mkcl_objectfn)dispatch1, 
(mkcl_objectfn)dispatch2, 
(mkcl_objectfn)dispatch3, 
(mkcl_objectfn)dispatch4, 
(mkcl_objectfn)dispatch5, 
(mkcl_objectfn)dispatch6, 
(mkcl_objectfn)dispatch7, 
(mkcl_objectfn)dispatch8, 
(mkcl_objectfn)dispatch9, 
(mkcl_objectfn)dispatch10, 
(mkcl_objectfn)dispatch11, 
(mkcl_objectfn)dispatch12, 
(mkcl_objectfn)dispatch13, 
(mkcl_objectfn)dispatch14, 
(mkcl_objectfn)dispatch15, 
(mkcl_objectfn)dispatch16, 
(mkcl_objectfn)dispatch17, 
(mkcl_objectfn)dispatch18, 
(mkcl_objectfn)dispatch19, 
(mkcl_objectfn)dispatch20, 
(mkcl_objectfn)dispatch21, 
(mkcl_objectfn)dispatch22, 
(mkcl_objectfn)dispatch23, 
(mkcl_objectfn)dispatch24, 
(mkcl_objectfn)dispatch25, 
(mkcl_objectfn)dispatch26, 
(mkcl_objectfn)dispatch27, 
(mkcl_objectfn)dispatch28, 
(mkcl_objectfn)dispatch29, 
(mkcl_objectfn)dispatch30, 
(mkcl_objectfn)dispatch31, 
(mkcl_objectfn)dispatch32, 
(mkcl_objectfn)dispatch33, 
(mkcl_objectfn)dispatch34, 
(mkcl_objectfn)dispatch35, 
(mkcl_objectfn)dispatch36, 
(mkcl_objectfn)dispatch37, 
(mkcl_objectfn)dispatch38, 
(mkcl_objectfn)dispatch39, 
(mkcl_objectfn)dispatch40, 
(mkcl_objectfn)dispatch41, 
(mkcl_objectfn)dispatch42, 
(mkcl_objectfn)dispatch43, 
(mkcl_objectfn)dispatch44, 
(mkcl_objectfn)dispatch45, 
(mkcl_objectfn)dispatch46, 
(mkcl_objectfn)dispatch47, 
(mkcl_objectfn)dispatch48, 
(mkcl_objectfn)dispatch49, 
(mkcl_objectfn)dispatch50, 
(mkcl_objectfn)dispatch51, 
(mkcl_objectfn)dispatch52, 
(mkcl_objectfn)dispatch53, 
(mkcl_objectfn)dispatch54, 
(mkcl_objectfn)dispatch55, 
(mkcl_objectfn)dispatch56, 
(mkcl_objectfn)dispatch57, 
(mkcl_objectfn)dispatch58, 
(mkcl_objectfn)dispatch59, 
(mkcl_objectfn)dispatch60, 
(mkcl_objectfn)dispatch61, 
(mkcl_objectfn)dispatch62, 
(mkcl_objectfn)dispatch63};


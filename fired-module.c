#include "fired-module.h"


void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qsym = env->intern(env, name);
  env->funcall(env, Qfset, 2, (emacs_value[]){Qsym, Sfun});
}


void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  env->funcall(env, Qprovide, 1, (emacs_value[]){Qfeat});
}

emacs_value Ffired_test(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                        void *data) {
    emacs_value arg = args[0];
    return arg;
}

int emacs_module_init(struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);

  // Symbols;
  Qfset = env->make_global_ref(env, env->intern(env, "fset"));
  Qprovide = env->make_global_ref(env, env->intern(env, "provide"));

  // Bind functions
  emacs_value fun;
  fun =
      env->make_function(env, 1, 1, Ffired_test, "Test from fired!", NULL);
  bind_function(env, "fired-test", fun);

  // done!

  provide(env, "fired-module");
  return 0;
}

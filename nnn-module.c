#include <dirent.h>
#include <stdlib.h>

#include "nnn-module.h"


void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qsym = env->intern(env, name);
  env->funcall(env, Qfset, 2, (emacs_value[]){Qsym, Sfun});
}


void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  env->funcall(env, Qprovide, 1, (emacs_value[]){Qfeat});
}

emacs_value Fnnn_make_context(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                              emacs_value args[], void *data __attribute__((unused))) {

  context *pctx = malloc(sizeof(context));
  if (!pctx) return Qnil;
  pctx = make_context(pctx, "/home/ym/.config/doom/packages/nnn/nnn", default_cfg);
  if (!pctx) return Qnil;
  ptrdiff_t pathlen = sizeof(pctx->c_path);
  env->copy_string_contents(env, args[0], pctx->c_path, &pathlen);
  if (!pctx)
    return Qnil;
  return env->make_user_ptr(env, clean_context, pctx);
}

emacs_value Fnnn_set_context_sort_flags(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  for (int i = 1; i < nargs; i++) {
      int flag = env->extract_integer(env, args[i]);
      set_sort_flags(pctx, flag);
  }
  return Qnil;
}

emacs_value Fnnn_populate_context(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                                  emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  populate(pctx);
  return Qnil;
}

emacs_value Fnnn_ls(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                    emacs_value args[], void *data __attribute__((unused))) {

  context* pctx = env->get_user_ptr(env, args[0]);
  size_t nameslen = 0;
  for (int i = 0; i < pctx->ndents; i++) {
    nameslen += pctx->pdents[i].nlen + 5; // nlen includes the null byte
  }
  char *pconcatbuf = malloc(nameslen);
  char *cursor = pconcatbuf;
  for (int i = 0; i < pctx->ndents; i++) {
    cursor = printent(cursor, &(pctx->pdents[i]));
    *(cursor++) = '\n';
  }
  // terminate
  *cursor = '\0';

  emacs_value res = env->make_string(env, pconcatbuf, cursor - pconcatbuf);
  return res;
}

#define BIND(c_name, emacs_name, arg_min, arg_max, docstring) \
  bind_function(env, emacs_name, \
                env->make_function(env, arg_min, arg_max, \
                                   c_name, docstring, NULL));


int emacs_module_init(struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);

  // Symbols;
  Qnil = env->make_global_ref(env, env->intern(env, "nil"));
  Qfset = env->make_global_ref(env, env->intern(env, "fset"));
  Qprovide = env->make_global_ref(env, env->intern(env, "provide"));

  // Bind functions
  BIND(Fnnn_make_context, "nnn-make-context", 1, 1,
       "Make a new context directory path PATH.");
  BIND(Fnnn_set_context_sort_flags, "nnn-set-context-sort-flags", 1,
       emacs_variadic_function,
       "Set flags for CTX: TODO what each key does.");
  BIND(Fnnn_populate_context, "nnn-populate-context", 1, 1,
       "Populate CTX with directory information.");
  BIND(Fnnn_ls, "nnn-ls", 1, 1,
       "Return directory listing for CTX.");

  // done!

  provide(env, "nnn-module");
  return 0;
}



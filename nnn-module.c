#include <dirent.h>
#include <stdlib.h>
#include <stdio.h>

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

  return env->make_string(env, pconcatbuf, cursor - pconcatbuf);
}

emacs_value Fnnn_get_file_at_index(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                                   emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  int i = env->extract_integer(env, args[1]);
  if (!(i >= 0 && i < pctx->ndents)) {
    char range[NAME_MAX];
    int n = sprintf(range, "0-%d, %d", pctx->ndents, i);
    emacs_value err_data = env->make_string(env, range, n-1); // not including null byte
    env->non_local_exit_signal(env, Qargs_out_of_range, err_data);
    return Qnil;
  }
  return env->make_string(env, pctx->pdents[i].name,
                          pctx->pdents[i].nlen - 1); // not including null byte
}

emacs_value Fnnn_number_of_entries(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                                   emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  return env->make_integer(env, pctx->ndents);
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
  Qargs_out_of_range = env->make_global_ref(env, env->intern(env, "args-out-of-range"));
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
  BIND(Fnnn_get_file_at_index, "nnn-get-file-at-index", 2, 2,
       "Return filename for file entry in context at index i.");
  BIND(Fnnn_number_of_entries, "nnn-number-of-entries", 1, 1,
       "Return the number of file entries in CTX.");


  // done!

  provide(env, "nnn-module");
  return 0;
}



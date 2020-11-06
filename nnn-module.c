#include <dirent.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "nnn-module.h"


static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qsym = env->intern(env, name);
  env->funcall(env, Qfset, 2, (emacs_value[]){Qsym, Sfun});
}


static void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  env->funcall(env, Qprovide, 1, (emacs_value[]){Qfeat});
}

int check_range(emacs_env *env, int i, int min_, int max_) {
  if (!(i >= min_ && i < max_)) {
    char range[NAME_MAX];
    int n = sprintf(range, "%d-%d, %d", min_, max_, i);
    emacs_value err_data = env->make_string(env, range, n-1); // not including null byte
    env->non_local_exit_signal(env, Qargs_out_of_range, err_data);
    return 0;
  }
  return 1;
}

static emacs_value Fnnn_make_context(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                              emacs_value args[], void *data __attribute__((unused))) {
  context *pctx = malloc(sizeof(context));
  if (!pctx) return Qnil;
  pctx = make_context(pctx, " ", default_cfg);
  if (!pctx) return Qnil;
  ptrdiff_t pathlen = sizeof(pctx->c_path);
  env->copy_string_contents(env, args[0], pctx->c_path, &pathlen);
  if (!pctx)
    return Qnil;
  return env->make_user_ptr(env, clean_context, pctx);
}

static emacs_value Fnnn_set_context_sort_flags(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  for (int i = 1; i < nargs; i++) {
      int flag = env->extract_integer(env, args[i]);
      set_sort_flags(pctx, flag);
  }
  return Qnil;
}

static emacs_value Fnnn_populate_context(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                                  emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  populate(pctx);
  return Qnil;
}

static emacs_value Fnnn_ls(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
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

static emacs_value Fnnn_get_file_at_index(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                                   emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  int i = env->extract_integer(env, args[1]);
  check_range(env, i, 0, pctx->ndents);
  return env->make_string(env, pctx->pdents[i].name,
                          pctx->pdents[i].nlen - 1); // not including null byte
}

static emacs_value Fnnn_number_of_entries(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                                   emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  return env->make_integer(env, pctx->ndents);
}

emacs_value Fnnn_get_entry_index(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                                 emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  char path[PATH_MAX];
  ptrdiff_t pathlen = sizeof(path);
  env->copy_string_contents(env, args[1], path, &pathlen);
  return env->make_integer(env, dentfind(pctx, path));
}

emacs_value Fnnn_make_extended_context(emacs_env *env, ptrdiff_t nargs __attribute__((unused)),
                                       emacs_value args[], void *data __attribute__((unused))) {
  context* pctx = env->get_user_ptr(env, args[0]);
  int i = env->extract_integer(env, args[1]);
  check_range(env, i, 0, pctx->ndents);
  char newpath[PATH_MAX];
  struct entry *pent = &pctx->pdents[i];
  mkpath(pctx->c_path, pent->name, newpath);
  if ((pent->flags & DIR_OR_LINK_TO_DIR) && (chdir(newpath) != -1)) {
    /* Valid dir, make the new context */
    context *new_pctx = malloc(sizeof(context));
    if (!new_pctx) return Qnil;
    if (!make_context(new_pctx, newpath, default_cfg)) return Qnil;
    return env->make_user_ptr(env, clean_context, new_pctx);
  } else {
    return Qnil;
  }
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
  BIND(Fnnn_get_entry_index, "nnn-get-entry-index", 2, 2,
       "Return the index for FILENAME (a small file name with no parent\n"
       "directories that doesn't end with a slash).");
  BIND(Fnnn_make_extended_context, "nnn-make-extended-context", 2, 2,
       "Return a new context corresponding to the subdir of index I at CTX.\n"
       "If at index I there's no subdir return nil.");


  // done!

  provide(env, "nnn-module");
  return 0;
}



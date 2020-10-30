#ifndef __FIRED_MODULE_H
#define __FIRED_MODULE_H
#include "emacs-module.h"

int plugin_is_GPL_compatible;

int emacs_module_init(struct emacs_runtime *ert);

void bind_function(emacs_env *env, const char *name, emacs_value Sfun);
void provide(emacs_env *env, const char *feature);

emacs_value Qfset;
emacs_value Qprovide;

#endif // __FIRED_MODULE_H

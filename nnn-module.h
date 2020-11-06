#ifndef __NNN_MODULE_H
#define __NNN_MODULE_H
#include "emacs-module.h"
#include "nnn/src/nnn.h"

int plugin_is_GPL_compatible;

int emacs_module_init(struct emacs_runtime *ert);

emacs_value Qnil;
emacs_value Qargs_out_of_range;
emacs_value Qfset;
emacs_value Qprovide;

#endif // __NNN_MODULE_H

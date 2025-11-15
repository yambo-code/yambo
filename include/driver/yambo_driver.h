/*
  License-Identifier: GPL

  Copyright (C) 2019 The Yambo Team

  Authors (see AUTHORS file for details): AM
*/
#pragma once

#include "kind.h"

int load_environments(char *file_name);

typedef void OptionsFn(struct options_struct *options, int *i_opt);

// Declare all option handler functions
extern OptionsFn options_control;
extern OptionsFn options_yambo;
extern OptionsFn options_projects;
extern OptionsFn options_interfaces;
extern OptionsFn options_ypp;
extern OptionsFn options_help;


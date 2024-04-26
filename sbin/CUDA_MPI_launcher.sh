#!/bin/bash 
#
# License-Identifier: GPL
#
# Copyright (C) 2018 The Yambo Team
#
# Authors (see AUTHORS file for details): AF
#
LRANK=$OMPI_COMM_WORLD_LOCAL_RANK
APP=$*
export CUDA_VISIBLE_DEVICES=$LRANK
$APP


## Load your packages, e.g. library(drake).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## _drake.R must end with a call to drake_config().
## The arguments to drake_config() are basically the same as those to make().
## lock_envir allows functions that alter the random seed to be used. The biggest
## culprits of this seem to be interactive graphics e.g. plotly and mapdeck.
drake_config(bind_plans(the_plan,
                        em_plan,
                        # plan_elastic_eval,
                        plan_lasso,
                        plan_iter_select_glm,
                        plan_ucd_and_challenge_best_glm,
                        bl_plan
                        ),
             lock_envir = FALSE)

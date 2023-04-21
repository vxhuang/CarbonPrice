import numpy as np

def factorial_design(nlevels):
    exp_design = np.mgrid[[slice(0, j) for j in nlevels]]
    exp_design = np.rollaxis(exp_design, 0, len(nlevels) + 1)
    exp_design = exp_design.reshape(tuple([np.product(nlevels), len(nlevels)]))
    return exp_design

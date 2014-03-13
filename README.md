lsttheory
=========

This package is a supplement for the article 'A Theory of States and Traits -- Revised' (SMGC; Steyer, Mayer, Geiser, & Cole, in press). It is based on the structural equation modeling package lavaan (Rosseel, 2012) and provides a convenient interface to compute some common models of the revised latent state-trait theory (LST-R theory). The main function of the package lsttheory() allows for easy specification of multistate, multistate-singletrait, and multistate-multitrait models. It automatically generates lavaan syntax for these models, runs the models, and returns model estimates together with reliability, occasion specificity, and consistency coefficients for the respective models. 

Installation
=========

lsttheory can be installed from this Github repository. For those not familiar with installing packages from Github, the subfolder /old contains tar.gz files (for installing from source) and windows binaries. Please make sure all dependencies are installed (lavaan, methods), and for the shiny interface, the additional packages shiny and semPlot are required.


Run lsttheory
=========

The main function of the package is lsttheory(). Type example(lsttheory) or see the vignette (subfolder vignette/) for examples. The shiny interface can be called by lsttheoryGUI() after having loaded the package lsttheory.
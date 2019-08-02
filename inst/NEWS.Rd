\name{NEWS}
\title{News for Package \pkg{spsurvey}}

\section{CHANGES IN spsurvey VERSION 4.1.0 (Released 2019-08-02)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Functions that create survey designs were modified to replace use of
        shapefiles to provide the survey frame with use of simple features (sf)
        objects to provide the survey frame.  To accommodate this change,
        argument src.frame for functions \code{grts} and \code{irs} now will
        accept the choice "sf.object" to indicate that an sf object will be used
        as the source of the survey frame.  In addition, a new argument named
        sf.object was added to functions \code{grts} and \code{irs} to allow
        input of an sf object as the survey frame.  Also, note that arguments
        id, maxtry, and prjfilename for functions \code{grts} and \code{irs}
        have been depricated since they are no longer needed.  Finally, the
        three rda files in the data directory that contain survey frame objects
        (\code{NE_lakes}, \code{Luck_Ash_streams}, and \code{UT_ecoregions})
        were revised to contain sf objects rather than objects belonging to
        classes that are defined in the sp package. 
      \item The package no longer contains any functions written in C.  The C
        functions were either no longer required due to the transition to use of
        sf objects to contain the survey frame or were replaced with functions
        written in R.  The new R code functions are: \code{cellWeight},
        \code{constructAddr}, \code{insideAreaGridCell},
        \code{insideLinearGridCell}, \code{make_grid}, \code{numLevels},
        \code{pickFiniteSamplePoints}, \code{pickGridCells},
        \code{pickSamplePoints}, \code{ranho}, and \code{selectFeatureID}. 
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item The new R code function named \code{numLevels} that determines the
        number of hierarchical levels for a generalized random-tesselation
        stratified (GRTS) survey design now includes code to ensure that the
        maximum number of levels (which is currently 11) is not bypassed when
        creating a survey design.
    }
  }
}

\section{CHANGES IN spsurvey VERSION 4.0.0 (Released 2019-04-04)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Necessary changes were enacted so that the package can be built
        within RStudio, e.g., roxygen comments were added to R source files.

      \item Created functions that create panel designs and that compute and
        plot power for panel designs given a linear trend.  Function
        \code{revisit_dsgn} creates a panel revisit design.  Function
        \code{revisit_bibd} creates a balanced incomplete block panel revisit
        design.  Function \code{revisit_rand} creates a revisit design with
        random assignment to panels and time periods.  Function
        \code{panel_summary} summarizes characteristics of a revisit panel
        design.  Function \code{power.dsgn} calculates power for multiple panel
        designs.  Function \code{cov.panel.dsgn} creates the covariance matrix
        for a panel design.  Function \code{plot.powerpaneldesign} plots power
        curves for panel designs.  
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified C function \code{readDbfFile} to avoid PROTECT errors.
    }
  }
}

\section{CHANGES IN spsurvey VERSION 3.4 (Released 2018-06-12)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item None.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified C functions \code{insideAreaGridCell},
        \code{insideLinearGridCell}, \code{linSample}, \code{linSampleIRS},
        \code{numLevels}, \code{pickAreaSamplePoints},
        \code{pickLinearSamplePoints}, and \code{pointInPolygonFile} to avoid
        PROTECT errors.

      \item Modified function \code{change.analysis} to assign the support_2
        variable from the design_2 data frame rather than from the design_1 data
        frame.

      \item Modified function \code{change.analysis} to use a revised procedure
        for ensuring that each repeated visit site is present in both surveys
        for subpopulation estimation.

      \item Modified C functions \code{writeShapeFilePoint} and
        \code{writeShapeFilePolygon} to correct memory access errors.

      \item Modified C function \code{readDbfFile} to ensure that missing values
        (NA) are handled correctly.
    }
  }
}

\section{CHANGES IN spsurvey VERSION 3.3 (Released 2016-08-19)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Inserted a SystemRequirements field in the DESCRIPTION file and
        modified functions \code{grts}, \code{grtsarea}, \code{grtslin},
        \code{grtspts}, \code{irs}, \code{irsarea}, \code{irslin},
        \code{read.dbf}, and \code{read.shape} to prevent the functions from
        being executed on big-endian processors.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Removed the C header file named \code{order.h} and replaced C
        functions \code{readBigEndian} and \code{readLittleEndian} with the
        version of those functions from \pkg{spsurvey} version 3.1.  
    }
  }
}

\section{CHANGES IN spsurvey VERSION 3.2 (Released 2016-08-16)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Created a vignette for change estimation.

      \item Modified functions \code{change.analysis} and \code{change.est} to
        include estimation for difference in median values for change estimation
        using continuous variables.

      \item Created a function named \code{examine} that examines variables in a
        data frame by printing either a table or the results of a call to the
        describe function in the Hmisc package
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified function \code{sp2shape} to accommodate objects of class
        "SpatialDesign" created by either the grts or irs functions.

      \item Added a C header file named \code{order.h} that determines whether
        the CPU employs big-endian or little-endian byte order.  Also, modified
        C functions \code{readBigEndian} and \code{readLittleEndian} to use
        constants created by the header file to ensure that computer words are
        read in the correct byte order.

      \item Modified function \code{input.check} to ensure that use of numeric
        variables for arguments \code{stratum} and \code{cluster} are
        handled correctly when finite or continuous population correction
        factors are utilized.

      \item Modified function \code{cdf.test} to ensure upper bounds that define
        classes for the CDFs are unique. Also, modified functions
        \code{cont.cdftest} and \code{cdf.test} to reduce the minimum number of
        upper bounds that define classes for the CDFs from three to two.

      \item Modified functions \code{sp2shape} and \code{read.dbf} to ensure
        that missing values are handled without error for character vectors.

      \item Modified functions \code{grts} and \code{irs} to ensure that a data
        frame object is assigned to argument att.frame when argument src.frame
        is assigned value "att.frame".

      \item Modified function \code{change.analysis} to ensure that each
        repeated visit site is present in both surveys for subpopulation
        estimation.

      \item Modified function \code{read.sas} to use 
        "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe" as the default
        value for argument \code{sascmd}. 
    }
  }
}


\section{CHANGES IN spsurvey VERSION 3.1 (Released 2015-10-23)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Modified vignettes to use data sets from the data directory for the
        package.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified C functions to ensure that variables passed to function
        malloc are of type unsigned integer.
    }
  }
}


\section{Changes in spsurvey VERSION 3.0 (Released 2015-05-21)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Created a class named SpatialDesign that contains class
        SpatialPointsDataFrame, which is defined in package sp.  The class is
        used for the output objects created by functions \code{grts} and
        \code{irs}.  Objects of class SpatialDesign include the survey design
        list as a slot rather than as an attribute, which was used previously.

      \item Created S3 and S4 methods for generic functions plot and summary for
        objects of class SpatialDesign.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified C functions to change the type declaration for variable
        \code{shpFileName} from "const char *" to "char * restrict".

      \item Modified C functions \code{writeDbfFile},
        \code{writeShapeFilePoint}, and \code{writeShapeFilePolygon} to change
        the return type from void to SEXP NULL, i.e., R_NilValue.

      \item Modified function \code{input.check} to remove missing values from
        final adjusted weights prior to checking that all values are positive.

      \item Modified functions \code{grtspts}, \code{grtslin}, and
        \code{grtsarea} to ensure that argument nlev (number of hierarchical
        levels) for  C fucntion constructAddr was stored as type integer.
    }
  }
}


\section{CHANGES IN spsurvey VERSION 2.7 (Released 2014-06-26)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added a cex.main argument to the \code{cont.cdfplot} and
      \code{cdf.plot} functions.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified function \code{warnprnt} to use correct variable names in
      the output data frame so that partial matching warnings for the names are
      not generated.
    }
  }
}


\section{Changes in spsurvey VERSION 2.6 (Released 2013-09-20)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Created a data directory that contain rda versions of the data
      files used by vignettes.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified function \code{change.analysis} to allow analysis of
      surveys with no repeat visit sites. 

      \item Modified function \code{change.est} to ensure that levels for a
      categorical response variable are listed in the desired order rather than
      sorted order.

      \item Modified function \code{localmean.weight} to allow recovery from
      an error in the singular value decomposition function (La.svd) that is
      called by the generalized inverse function (ginv) in the MASS package.
      Also, created a support function named \code{localmean.weight2}.
    }
  }
}


\section{Changes in spsurvey VERSION 2.5 (Released 2012-10-10)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Modified function \code{grtspts} and C function \code{numLevels} to
      calculate the sampling grid random shift values only once rather than each
      time the number of hierarchical levels is incremented.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified functions \code{attrisk.analysis}, \code{cat.analysis},
      \code{change.analysis}, \code{cont.analysis}, and \code{relrisk.analysis}
      to replace NA values with FALSE in the indicator variables for
      subpopulations.

      \item Modified function \code{spbalance} to include all grid cells with
      nonzero extent for the frame when calculating the Pielou evenness measure
      for a rectangular grid.

      \item Modified function \code{localmean.weight} to ensure that the initial
      set of weights are inversely proportional to distance.

      \item Modified functions \code{cont.cdftest} and \code{cdf.test} to ensure
      that the number of classes into which the CDFs will be divided (binned) is
      at least three.

      \item Modified function \code{dframe.check} to ensure proper handling of
      repeated site ID values in the sites data frame.
    }
  }
}


\section{Changes in spsurvey VERSION 2.4 (Released 2012-05-23)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Created a vignette that presents deconvolution of a cumulative
      distribution function (CDF) generated by a GRTS survey design.

      \item Created a function named \code{spbalance} that calculates spatial
      balance metrics for a survey design.

      \item Created functions named \code{sbcframe} and \code{sbcsamp} that
      calculate spatial balance grid cell extent and proportions for a sample
      frame and for a survey design, respectively.

      \item Modified function \code{change.est} to include calculation of
      resource size change estimates for categorical variables.
 
      \item Created a function named \code{changevar.size} to calculate
      covariance or correlation estimates of the estimated change in class
      resource size estimates between two probability surveys
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified function \code{change.est} to correct errors in the output
      values for categorical variables from survey designs that lack repeat
      visit sites.
 
      \item Modified the following functions to assign consecutive numbers to
      the row names for the output data frame(s): attrisk.analysis,
      cat.analysis, change.analysis, cont.analysis, cont.cdftest, and
      relrisk.analysis.
    }
  }
}


\section{Changes in spsurvey VERSION 2.3 (Released 2012-02-03)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Created a function named \code{change.analysis} that conducts change
      analysis for a collection of response variables (categorical and
      continuous) generated by two probability surveys.

      \item Created functions named \code{change.est}, \code{changevar.prop},
      and \code{changevar.mean} to calculate estimates of change and its
      associated variance.

      \item Created a vignette that presents cumulative distribution function
      (CDF) analysis of a GRTS survey designs.

      \item Added a function named ash1.wgt that computes the average shifted
      histogram (ASH) for weighted data.

      \item Created an .onAttach function that prints a message when the
      spsurvey package is attached.  Removed the .onLoad function, which prior
      versions used to to print a startup message.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified function \code{grtspts} to correct an error that occurs
      when argument \code{src.frame} equals "att.frame" and the number of
      hierarchical levels equals eleven.

      \item Modified C functions \code{printAddrList}, \code{printColCharList},
      and \code{printDbf} to replace calls to the printf function with calls to
      the Rprintf function.

      \item Modified C function \code{pickAreaSamplePoints} to correct an error
      that occurs when selecting sample points for PolygonZ and PolygonM type
      shapefiles.
    }
  }
}


\section{Changes in spsurvey VERSION 2.2 (Released 2011-05-16)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item In order to reduce package size, removed demonstration (demo) R
      scripts and the associated data directory.

      \item Created vignettes that present analyses of GRTS survey designs for
      finite, linear, and areal resources.

      \item Created a function named \code{attrisk.analysis} that conducts
      attributable risk analysis for a collection of response variables
      generated by a probability survey.

      \item Created functions named \code{attrisk.est} and \code{attrisk.var} to
      calculate estimates of attributable risk and its associated variance.

      \item Modified function \code{relrisk.est} to change the way the relative
      risk estimate is calculated for a stratified sample.

      \item Modified functions \code{cat.analysis}, \code{cont.analysis},
      \code{cont.cdftest}, \code{relrisk.analysis}, \code{category.est},
      \code{cdf.est}, \code{cdf.decon}, \code{total.est}, \code{cdf.test},
      \code{relrisk.est}, \code{catvar.prop}, \code{catvar.size},
      \code{cdfvar.prop}, \code{cdfvar.total}, \code{cdfvar.size.prop},
      \code{cdfvar.size.total}, \code{dcdfvar.prop}, \code{dcdfvar.total},
      \code{dcdfvar.size.prop}, \code{dcdfvar.size.total}, \code{total.var},
      \code{cdfvar.test}, and \code{relrisk.var} to allow variance estimates to
      be calculated when a two stage sample has stage one sampling units that
      contain a single stage two sampling unit.  Variance for those stage one
      sampling units is calculated using the mean of the variance estimates for
      stage one sampling units that contain two or more stage two sampling
      units.

      \item Modified function \code{grtslin} to improve efficiency of sample
      point selection.  Created a C function named \code{insideLinearGridCell}
      that, for each grid cell selected to receive a sample point, returns the
      ID value for shapefile records contained in the cell and the clipped
      length of the polyline segments within the cell for each record.  Created
      a C function named \code{pickLinearSamplePoints} that selects sample
      points.

      \item Modified function \code{grtsarea} to improve efficiency of sample
      point selection.  Created a C function named \code{insideAreaGridCell}
      that, for each grid cell selected to receive a sample point, returns the
      ID value for shapefile records contained in the cell and the clipped area
      of the polygon within the cell for each record.  Created a C function
      named \code{pickAreaSamplePoints} that selects sample points.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified function \code{input.check} to include determination of
      whether a valid value was provided for argument \code{vartype}.

      \item Modified functions \code{grts} and \code{irs} so that correct survey
      design weights are created for linear and areal designs when the type of
      random selection is "continuous".

      \item Modified functions \code{grtspts}, \code{grtslin}, and
      \code{grtsarea} to execute without error for a stratum sample size of one.

      \item Modified function \code{dframe.check} to terminate execution when
      missing values are encountered in the logical variable of the sites data
      frame.
    }
  }
}


\section{Changes in spsurvey VERSION 2.1 (Released 2009-10-29)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Created vignettes that present examples of GRTS survey designs for
      finite, linear, and areal resources.

      \item Created a function named \code{geodalbers} that projects latitude
      and longitude (spheroid) models of the globe to Albers projection in the
      plane.

      \item Modified function \code{dsgnsum} to produce summary tables for the
      actual set of design variables that are present rather than a standard set
      of design variables.

      \item Modified function \code{dframe.check} to terminate execution when
      missing site ID values or repeated site ID values are encountered in the
      \code{sites}, \code{design}, \code{subpop}, \code{data.cat},
      \code{data.cont}, or \code{data.rr} data frames.  Note that
      \code{dframe.check} is called by functions \code{cat.analysis},
      \code{cont.analysis}, \code{cont.cdftest}, and \code{relrisk.analysis}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified function \code{cont.cdftest} to correct an error caused by
      improper handling of an empty subpopulation.

      \item Modified function \code{relrisk.analysis} to correct an error
      resulting from inclusion of more than one response variable in the
      \code{response.var} argument.
    }
  }
}

\section{Changes in spsurvey VERSION 2.0 (Released 2008-06-16)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Eliminated use of argument \code{unitsize} (known sum of
      size-weights) by package functions.  Restricted argument \code{popsize} to
      provide only the known size of the resource for use in ratio adjustment of
      estimators.  Created a new argument named \code{pcfsize} to provide
      resource size for calculation of finite and continuous population
      correction factors for single-stage samples.

      \item Modified functions \code{cat.analysis}, \code{cont.analysis},
      \code{cont.cdftest}, and \code{relrisk.analysis} to add logical variables
      to their argument lists that specify use of finite or continuous
      population correction factors or use of size-weights in analysis routines.

      \item Modified functions \code{category.est}, \code{cdf.decon},
      \code{cdf.est}, \code{cdf.test}, \code{relrisk.est} and \code{total.est}
      to add logical variables to their argument lists that specify use of
      finite or continuous population correction factors or use of size-weights.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified function \code{irs} to eliminate checking for existence of
      x-coordinates and y-coordinates when the type of frame is "finite" and the
      frame is included in the \code{att.frame} data frame.

      \item Modified C functions \code{parseFields} and \code{readDbfFile} to
      ensure that blank values in the shapefile attributes (dbf) file are
      converted to R missing values (NA).

      \item Modified C function \code{writeDbfFile} to output blank values for R
      missing values (NA) when creating the shapefile attributes (dbf) file.

      \item Modified functions \code{grts}, \code{irs}, and \code{sp2shape} to
      ensure that an output shapefile attributes (dbf) file containing character
      variables with missing values can be read without error.
    }
  }
}

\section{Changes in spsurvey VERSION 1.7 (Released 2007-11-09)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Created a function named \code{cont.cdftest} that tests for
      differences between cumulative distribution functions (CDFs) for pairs of
      subpopulations within a population \code{Type} for a collection of
      response variables generated by a probability survey.  Also, modified
      function \code{cdf.test} so that it is consistent with functions that are
      called by high-level functions \code{cat.analysis} and
      \code{cont.analysis}.

      \item Modified function \code{grtspts} so that, when source of the frame
      equals "att.frame", the current number of grid levels for hierarchical
      randomization and the final number of grid levels is printed to the
      console while the function is executing.

      \item Created a function named \code{relrisk.analysis} that conducts
      relative risk analysis for a collection of response variables generated by
      a probability survey.  Also, renamed function \code{relrisk} to
      \code{relrisk.est} and modified the function so that it is consistent with
      functions that are called by high-level functions \code{cat.analysis} and
      \code{cont.analysis}.

      \item Modified function \code{grtspts} and C function \code{numLevels} to
      terminate the algorithm for determining the number of grid levels for
      hierarchical randomization when the maximum value of total inclusion
      probability for the grid cells stops changing as the number of levels
      increases.

      \item Created functions named \code{cdf.plot} and \code{cont.cdfplot} that
      create cumulative distribution function (CDF) plots using the data frame
      named "CDF" contained in the output object created by function
      \code{cont.analysis}.  Function \code{cdf.plot} creates a single CDF plot,
      and function \code{cont.cdfplot} creates a set of CDF plots.

      \item Created a function named \code{read.sas} that can read SAS datasets
      or a SAS XPORT (transport) file.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified C functions \code{intersect}, \code{linSampleIRS},
      \code{lintFcn}, and \code{linSample} to eliminate warning messages that
      were generated during package creation.

      \item Modified function \code{grts} so that argument \code{do.sample}
      provides a value for each stratum.

      \item Modified C functions \code{getRecordShapeSizes}, \code{readDbfFile},
      \code{parsePoints}, \code{parsePointsZ}, \code{parsePointsM},
      \code{parsePolygon}, \code{parsePolygonZ}, and \code{parsePolygonM} to
      generate error messages and terminate execution when a shapefile
      containing a Null record is encountered.

      \item Modified functions \code{irslin} and \code{irsarea} in addition to C
      function \code{getRecordIDs} to ensure that sample points are selected in
      random order for linear and areal IRS designs.

      \item Modified function \code{grts} to ensure that, when the type of
      random selection is "unequal", an oversample is apportioned correctly
      whenever the category sample sizes are proportional to the oversample
      size.
    }
  }
}

\section{Changes in spsurvey VERSION 1.6 (Released 2007-01-18)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item For C functions that read shapefiles from the current directory,
      replaced calls to \code{_findfirst} and \code{_findnext} with code using
      calls to \code{readdir}.  These changes were implemented to facilitate
      portability of the package.

      \item Created a C function named \code{matchFiles} that determines whether
      file names in the current directory have a desired file extension.  This
      change was implemented to facilitate portability of the package.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified functions \code{grts} and \code{irs} to accommodate use of
      a factor for the ID variable in the attributes data frame.

      \item Modified functions \code{grts} and \code{irs} to ensure that sample
      weights are correctly adjusted when an oversample is present and the type
      of random selection is "Continuous".
    }
  }
}

\section{Changes in spsurvey VERSION 1.5 (Released 2006-12-06)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item None.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified C functions \code{getRecordShapeSizes} and \code{lintFcn}
      to accommodate Polyline shapefiles that have multiple parts.
    }
  }
}

\section{Changes in spsurvey VERSION 1.4 (Released 2006-10-10)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Modified functions \code{dsgnsum} and \code{sp2shape} to accommodate
      the change in representation from \code{AttributeList} to
      \code{data.frame} for the data slot of sp package objects belonging to
      class \code{SpatialPointsDataFrame}.

      \item Modified functions \code{grts} and \code{irs} to print a warning
      message when the type of frame equals "finite" and a stratum name in the
      design list matches only a single value in the stratum column of the
      attributes data frame.  For this case, function \code{grtspts} or
      \code{irspts} is not called since the sample will be composed of a single
      point.

      \item Modified functions \code{grts}, \code{grtspts}, \code{grtslin}, and
      \code{grtsarea} to change the maximum value allowed for arguments startlev
      (the initial number of hierarchical levels to use for the GRTS grid) and
      maxlev (the maximum number of hierarchical levels to use for the GRTS
      grid) from 12 to 11.

      \item Added an example polylines dataset to the data directory.

      \item Modified functions \code{grts} and \code{irs} to allow use of an sp
      package object as the source of the frame.  An argument named
      \code{sp.object} was added to the argument list for \code{grts} and
      \code{irs}.

      \item Modified functions \code{grts}, \code{grtspts}, \code{grtslin},
      \code{grtsarea}, \code{irs}, \code{irspts}, \code{irslin}, and
      \code{irsarea} to remove use of argument \code{xy.frame} as an option for
      source of the frame.  Then modified functions \code{grts}, \code{grtspts},
      \code{irs}, and \code{irspts} to allow incorporation of frame coordinates
      into the attributes data frame when the type of frame equals "finite". 
      Also, removed argument \code{elmsize} from functions \code{grts} and
      \code{irs} since the argument no longer was required.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified functions \code{grts} and \code{irs} to print a warning
      message when the type of frame equals "finite" and a stratum name in the
      design list matches only a single value in the stratum column of the
      attributes data frame.  For this case, function \code{grtspts} or
      \code{irspts} is not called since the sample will be composed of a single
      point.

      \item Modified functions \code{grts} and \code{irs} to ensure that the ID
      values for elements of the frame provided in att.frame are unique.

      \item Modified functions \code{grts} and \code{irs} to ensure that valid
      values are provided for the \code{panel} and \code{caty.n} arguments in
      the design list.
    }
  }
}

\section{Changes in spsurvey VERSION 1.3 (Released 2006-08-01)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added an example polygons dataset to the data directory.

      \item Incorporated the CHANGES, README, and UserGuide files into the help
      page.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item None.
    }
  }
}

\section{Changes in spsurvey VERSION 1.2 (Released 2006-06-27)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Created a function named \code{sp2shape} and a C function named
      \code{writeShapeFilePolygon} that convert objects created by package
      sp to ESRI shapefiles.  Also, renamed the C function \code{writeShapeFile}
      to \code{writeShapeFilePoint}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified function \code{irsarea} and created a C function
      \code{named getRecordIDs} to ensure that an IRS sample is selected when
      argument \code{type.frame} is set to "area" in function \code{irs}.
    }
  }
}

\section{Changes in spsurvey VERSION 1.1 (Released 2006-05-31)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Modified the C functions so that the package can accommodate M-type
      shapefiles.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Modified functions \code{grts} and \code{irs} to ensure that the
      levels of \code{mdcaty} (the variable in the attributes data frame that
      specifies the unequal probability category for each element in the frame)
      are equivalent to the names in \code{caty.n} (the vector of sample sizes
      for each category in \code{mdcaty}, which is required for each element of
      the design list for which the selection type is "Unequal").

      \item Modified functions \code{grts} and \code{irs} to ensure that the
      columns of \code{xy.frame} are named "x" and "y" when \code{xy.frame} is
      provided and \code{type.frame} equals "finite".

      \item Modified functions \code{grts} and \code{irs} so that the sample
      weights are correctly adjusted when an oversample is requested and the
      realized sample size is less than the desired sample size.
    }
  }
}

\section{Changes in spsurvey VERSION 1.0 (Released 2006-05-05)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item This is the original version of the package.

    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item None.
    }
  }
}

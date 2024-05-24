import std / tables
import datamancer, unchained
import arraymancer, numericalnim

import flatBuffers
import flatBuffers / flatBuffers_tensor

defUnit(keV⁻¹•cm⁻²)

type
  LogLFlagKind* = enum
    # vetoes
    fkTracking, fkLogL, fkMLP, fkConvNet, fkFadc, fkScinti, fkSeptem, fkLineVeto, fkAggressive,
    # other options
    fkRocCurve, fkComputeLogL, fkPlotLogL, fkPlotSeptem,
    fkEstRandomCoinc, # used to estimate the random coincidence of the septem & line veto
    fkEstRandomFixedEvent, # use a fixed center cluster and only vary around the outer ring
    fkReadOnly # makes the input file read only

  ChipCoord = range[0.0 .. 14.0]

  Candidate = object
    energy: keV
    pos: tuple[x, y: ChipCoord]

  ## TODO: split the different fields based on the method we want to use?
  ## SamplingKind represents the different types of candidate from background sampling we can do
  SamplingKind = enum
    skConstBackground, ## uses the constant background over the gold region. Only allows sampling in the gold region.
    skInterpBackground ## uses the interpolated background over the whole chip.

  UncertaintyKind = enum
    ukCertain, # no uncertainties
    ukUncertainSig, # uncertainty only on signal (integrated analytically)
    ukUncertainBack, # uncertainty only on background (integrated numerically)
    ukUncertain # uncertainty on both. Analytical result of ukUncertainSig integrated numerically

  PositionUncertaintyKind = enum
    puCertain   # no uncertainty
    puUncertain # use uncertainty on position

  ## Stores the relevant context variables for the interpolation method
  Interpolation = object
    kd: KDTree[float]        ## we use a KDTree to store the data & compute interpolation on top of
    backCache: Table[Candidate, keV⁻¹•cm⁻²] ## cache for the background values of a set of candidates. Used to avoid
                             ## having to recompute the values in a single MC iteration (within limit computation).
                             ## Only the signal values change when changing the coupling constants after all.
    radius: float            ## radius of background interpolation (σ is usually radius / 3.0)
    sigma: float             ## σ of the weight for, usually radius / 3.0 as mentioned above
    energyRange: keV         ## energy range of the background interpolation
    nxy: int                 ## number of points at which to sample the background interpolation in x/y
    nE: int                  ## number of points at which to sample the background interpolation in E
    xyOffset: float          ## Offset in x/y coordinates (to not sample edges). Is `coords[1] - coords[0] / 2`
    eOffset: float           ## Offset in E coordinates (to not sample edges). Is `energies[1] - energies[0] / 2`
    coords: seq[float]       ## the coordinates at which the background interpolation was evaluated to
                             ## compute the the expected counts tensor
    energies: seq[float]     ## the energy values at which the background interpolation was evaluated
                             ## to compute the expected counts tensor
    expCounts: Tensor[float] ## the tensor containing the expected counts at different (x, y, E) pairs
    backgroundTime: Hour     ## time of background data (same value as in `Context`)
    trackingTime: Hour       ## time of solar tracking (same value as in `Context`)
    # these are always valid for a single `computeLimit` call!
    zeroSig: int             ## counts the number of times the expected signal was 0
    zeroBack: int            ## counts the number of times the background was 0
    zeroSigBack: int         ## counts the number of times the signal & background was zero

  ## Stores the current state of the systematics. Allows to easier replace the kind &
  ## value of systematics at RT without having to change the kind of the `Context`.
  ## Note: for backward compat there are templates to access `ctx.systematics.X`.
  Systematics = object
    case uncertainty: UncertaintyKind
    of ukUncertainSig:
      σs_sig: float # Uncertainty on signal in relative terms, percentage
    of ukUncertainBack:
      σb_back: float # Uncertainty on background in relative terms, percentage
    of ukUncertain: ## annoying....
      σsb_sig: float
      σsb_back: float
    else: discard
    # uncertainty on the center position of the signal
    case uncertaintyPosition: PositionUncertaintyKind
    of puUncertain:
      σ_p: float # relative uncertainty away from the center of the chip, in units of
                # ???
      ϑ_x: float
      ϑ_y: float
    of puCertain: discard # no uncertainty


  Efficiency = object
    totalEff: float # total efficiency multiplier based on signal efficiency of lnL cut, FADC & veto random coinc rate
    signalEff: float # the lnL cut signal efficiency used in the inputs
    nnSignalEff: float # target signal efficiency of MLP
    nnEffectiveEff: float # effective efficiency based on
    nnEffectiveEffStd: float
    eccLineVetoCut: float # the eccentricity cutoff for the line veto (affects random coinc.)
    vetoPercentile: float # if FADC veto used, the percentile used to generate the cuts
    septemVetoRandomCoinc: float # random coincidence rate of septem veto
    lineVetoRandomCoinc: float # random coincidence rate of line veto
    septemLineVetoRandomCoinc: float # random coincidence rate of septem + line veto


  Context = ref object ## XXX: make ref object
    mcIdx: int # monte carlo index, just for reference
    # input file related information
    logLFlags: set[LogLFlagKind]
    eff: Efficiency
    switchAxes: bool # If true will exchange X and Y positions of clusters, to view as detector installed at CAST
                     # (see sec. `sec:limit:candidates:septemboard_layout_transformations` in thesis)
    # time information
    totalBackgroundClusters: int # total number of background clusters in non-tracking time
    totalBackgroundTime: Hour # total time of background data taking
    totalTrackingTime: Hour # total time of solar tracking
    # tracking related
    trackingDf: DataFrame
    # energy information
    energyMin, energyMax: keV
    # axion signal info
    axionModelFile: string # The filename we read for the differential solar axion flux
    axionImageFile: string # The filename we read for the axion image (raytracing result)
    axionModel: DataFrame
    integralBase: float # integral of axion flux using base coupling constants
    # detector related
    windowRotation: Degree # rotation of the window during data taking
    # efficiency
    combinedEfficiencyFile: string # file storing detector efficiency including LLNL effective area
    # interpolators
    axionSpl: InterpolatorType[float]
    efficiencySpl: InterpolatorType[float]
    raytraceSpl: Interpolator2DType[float]
    backgroundSpl: InterpolatorType[float]
    # background candidate sampling
    backgroundDf: DataFrame # the DataFrame containing all background cluster data
    backgroundCDF: seq[float] # CDF of the background
    energyForBCDF: seq[float] # energies to draw from for background CDF
    case samplingKind: SamplingKind # the type of candidate sampling we do
    of skInterpBackground:
      interp: Interpolation ## A helper object to store all interpolation fields
    else: discard # skConstant doesn't need
    # limit related
    g_aγ²: float # the ``reference`` g_aγ (squared)
    g_ae²: float # the ``reference`` g_ae value (squared)
    β²: float    # the ``reference`` β value (squared) for the chameleon coupling XXX: not in use yet
    coupling: float # the ``current`` coupling constant in use. Can be a value of
                    # `g_ae²`, `g_aγ⁴`, `g_ae²·g_aγ²`, `β⁴` depending on use case!
                    # Corresponds to first entry of MCMC chain vector!
    m_a: eV = -1.eV # Axion mass for which we compute a limit. Default negative value implies we use
                    # low mass approximation for converison probability
    couplingKind: CouplingKind # decides which coupling to modify
    couplingReference: float # the full reference coupling. `g_ae²·g_aγ²` if `ck_g_ae²·g_aγ²`
    mcmcCouplingTarget: float # The target value used as reference for the MCMC
                              # For `g_ae²` searches: `1e-21 * 1e-12^2`
                              # Should be larger than the typical expected coupling constant. Provides
                              # the range of interest in which MCMC will effectively sample.
    # systematics and noise
    systematics: Systematics
    noiseFilter: NoiseFilter
    # additional fields for computation & input data storage
    rombergIntegrationDepth: int ## only for case of logLFullUncertain integration!
    filePath: string   ## The path to the data files
    files: seq[string] ## The data files we read
    tracking: seq[string] ## The H5 files containing the real tracking candidates
    # the following are old parameters that are not in use anymore (lkSimple, lkScan etc)
    #couplingStep: float # a step we take in the couplings during a scan
    #logLVals: Tensor[float] # the logL values corresponding to `couplings`
    #maxIdx: int # index of the maximum of the logL curve

  CouplingKind = enum
    ck_g_ae²       ## We vary the `g_ae²` and leave `g_aγ²` fully fixed
    ck_g_aγ⁴       ## We vary the `g_aγ⁴` and leave `g_ae²` fully fixed (and effectively 'disabled'); for axion-photon searches
    ck_g_ae²·g_aγ² ## We vary the *product* of `g_ae²·g_aγ²`, i.e. direct `g⁴` proportional search.
                   ## Note that this is equivalent in terms of the limit!
    ck_g_ae·g_aγ   ## We vary the *product* of `g_ae·g_aγ`, but in `g²` form. This is *NOT* equivalent and gives the ``WRONG``
                   ## limit. Only for illustration!
    ck_β⁴          ## We vary `β`, the chameleon coupling. For chameleon searches.

  ## For now a noise filter only defines a single set of pixels that are applied to
  ## all files in `fnames`. In the future we could generalize to specific sets of pixels
  ## for individual files.
  NoiseFilter = object
    pixels: seq[(int, int)] # the pixels to filter
    fnames: seq[string] # the filenames this filter should be applied to

  LimitKind = enum
    lkSimple,     ## purely physical region going down to 95% equivalent
    lkScan,       ## proper scan for maximum using a binary approach
    lkLinearScan, ## limit based on linear scan in pre defined range
    lkBayesScan,  ## limit based on integrating bayes theorem (posterior prob.)
    lkMCMC        ## limit based on Metropolis-Hastings Markov Chain Monte Carlo

  LimitData = object
    m_a: eV
    bins: int
    upper: float # upper cut value (99.9 percentile of prebinned data)
    gs: seq[float] # coupling constants
    histo: seq[int]

  MassScanLimitOutput = object
    ctx: Context
    burnIn: int
    nChains: int # number of chains built for each mass
    limitKind: LimitKind
    massLow: eV
    massHigh: eV
    massSteps: int
    lData: Table[eV, LimitData]
    dfLimit: DataFrame # DF of the computed limits for each mass, columns `[limit, masses]`

const path = "tMcmcLimit.dat"
let output = loadBuffer[MassScanLimitOutput](path)

#echo output
#echo output.ctx.repr
echo output.ctx.systematics

## XXX: turn me into a test!

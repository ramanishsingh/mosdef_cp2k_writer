import datetime
import cssi_cp2k.utilities as utilities


def _validate_band(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "BAND iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'BAND','ErrorMessage':errorMessage})
    raise TypeError

def _validate_bsse(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "BSSE iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'BSSE','ErrorMessage':errorMessage})
    raise TypeError

def _validate_cell_opt(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "CELL_OPT iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'CELL_OPT','ErrorMessage':errorMessage})
    raise TypeError

def _validate_ep_lin_solver(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "EP_LIN_SOLVER iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'EP_LIN_SOLVER','ErrorMessage':errorMessage})
    raise TypeError

def _validate_geo_opt(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "GEO_OPT iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'GEO_OPT','ErrorMessage':errorMessage})
    raise TypeError

def _validate_just_energy(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "JUST_ENERGY iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'JUST_ENERGY','ErrorMessage':errorMessage})
    raise TypeError

def _validate_md(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "MD iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'MD','ErrorMessage':errorMessage})
    raise TypeError

def _validate_metadynamics(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "METADYNAMICS iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'METADYNAMICS','ErrorMessage':errorMessage})
    raise TypeError

def _validate_pint(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "PINT iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'PINT','ErrorMessage':errorMessage})
    raise TypeError

def _validate_powell_opt(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "POWELL_OPT iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'POWELL_OPT','ErrorMessage':errorMessage})
    raise TypeError

def _validate_qs_scf(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "QS_SCF iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'QS_SCF','ErrorMessage':errorMessage})
    raise TypeError

def _validate_replica_eval(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "REPLICA_EVAL iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'REPLICA_EVAL','ErrorMessage':errorMessage})
    raise TypeError

def _validate_rot_opt(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "ROT_OPT iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'ROT_OPT','ErrorMessage':errorMessage})
    raise TypeError

def _validate_shell_opt(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "SHELL_OPT iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'SHELL_OPT','ErrorMessage':errorMessage})
    raise TypeError

def _validate_spline_find_coeffs(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "SPLINE_FIND_COEFFS iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'SPLINE_FIND_COEFFS','ErrorMessage':errorMessage})
    raise TypeError

def _validate_tddft_scf(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "TDDFT_SCF iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'TDDFT_SCF','ErrorMessage':errorMessage})
    raise TypeError

def _validate_xas_scf(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "XAS_SCF iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'XAS_SCF','ErrorMessage':errorMessage})
    raise TypeError

def _validate_root(val):
  if utilities.is_positive_integer(val):
    return val
  else:
    errorMessage = "ROOT iteration level must be a positive integer."
    self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'init','Module':'EACH',
                            'Variable':'ROOT','ErrorMessage':errorMessage})
    raise TypeError

class EACH:

  def __init__(self,BAND=1,BSSE=1,CELL_OPT=1,EP_LIN_SOLVER=1,GEO_OPT=1,JUST_ENERGY=1,MD=1,POWELL_OPT=1,
               METADYNAMICS=1,PINT=1,QS_SCF=1,REPLICA_EVAL=1,ROT_OPT=1,SHELL_OPT=1,SPLINE_FIND_COEFFS=1,
               TDDFT_SCF=1,XAS_SCF=1,ROOT=1,errorLog=[],changeLog=[]):

    self.__BAND               = _validate_band(BAND)
    self.__BSSE               = _validate_bsse(BSSE)
    self.__CELL_OPT           = _validate_cell_opt(CELL_OPT)
    self.__EP_LIN_SOLVER      = _validate_ep_lin_solver(EP_LIN_SOLVER)
    self.__GEO_OPT            = _validate_geo_opt(GEO_OPT)
    self.__JUST_ENERGY        = _validate_just_energy(JUST_ENERGY)
    self.__MD                 = _validate_md(MD)
    self.__METADYNAMICS       = _validate_metadynamics(METADYNAMICS)
    self.__PINT               = _validate_pint(PINT)
    self.__POWELL_OPT         = _validate_powell_opt(POWELL_OPT)
    self.__QS_SCF             = _validate_qs_scf(QS_SCF)
    self.__REPLICA_EVAL       = _validate_replica_eval(REPLICA_EVAL)
    self.__ROT_OPT            = _validate_rot_opt(ROT_OPT)
    self.__SHELL_OPT          = _validate_shell_opt(SHELL_OPT)
    self.__SPLINE_FIND_COEFFS = _validate_spline_find_coeffs(SPLINE_FIND_COEFFS)
    self.__TDDFT_SCF          = _validate_tddft_scf(TDDFT_SCF)
    self.__XAS_SCF            = _validate_xas_scf(XAS_SCF)
    self.__ROOT               = _validate_root(ROOT)
    self.__errorLog           = errorLog
    self.__changeLog          = changeLog

  @property
  def BAND(self):
    return self.__BAND

  @property
  def BSSE(self):
    return self.__BSSE

  @property
  def CELL_OPT(self):
    return self.__CELL_OPT

  @property
  def EP_LIN_SOLVER(self):
    return self.__EP_LIN_SOLVER

  @property
  def GEO_OPT(self):
    return self.__GEO_OPT

  @property
  def JUST_ENERGY(self):
    return self.__JUST_ENERGY

  @property
  def MD(self):
    return self.__MD

  @property
  def METADYNAMICS(self):
    return self.__METADYNAMICS

  @property
  def PINT(self):
    return self.__PINT

  @property
  def POWELL_OPT(self):
    return self.__POWELL_OPT

  @property
  def QS_SCF(self):
    return self.__QS_SCF

  @property
  def REPLICA_EVAL(self):
    return self.__REPLICA_EVAL

  @property
  def ROT_OPT(self):
    return self.__ROT_OPT

  @property
  def SHELL_OPT(self):
    return self.__SHELL_OPT

  @property
  def SPLINE_FIND_COEFFS(self):
    return self.__SPLINE_FIND_COEFFS

  @property
  def TDDFT_SCF(self):
    return self.__TDDFT_SCF

  @property
  def XAS_SCF(self):
    return self.__XAS_SCF

  @property
  def ROOT(self):
    return self.__ROOT

  @BAND.setter
  def BAND(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'BAND',
                               'Success':True,'Previous':self.__BAND,'New':val,
                               'ErrorMessage':None})
      self.__BAND = val
    else:
      errorMessage = "BAND iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'BAND',
                               'Success':False,'Previous':self.__BAND,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'BAND','ErrorMessage':errorMessage})

  @BSSE.setter
  def BSSE(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'BSSE',
                               'Success':True,'Previous':self.__BSSE,'New':val,
                               'ErrorMessage':None})
      self.__BSSE = val
    else:
      errorMessage = "BSSE iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'BSSE',
                               'Success':False,'Previous':self.__BSSE,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'BSSE','ErrorMessage':errorMessage})

  @CELL_OPT.setter
  def CELL_OPT(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'CELL_OPT',
                               'Success':True,'Previous':self.__CELL_OPT,'New':val,
                               'ErrorMessage':None})
      self.__CELL_OPT = val
    else:
      errorMessage = "CELL_OPT iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'CELL_OPT',
                               'Success':False,'Previous':self.__CELL_OPT,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'CELL_OPT','ErrorMessage':errorMessage})

  @EP_LIN_SOLVER.setter
  def EP_LIN_SOLVER(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'EP_LIN_SOLVER',
                               'Success':True,'Previous':self.__EP_LIN_SOLVER,'New':val,
                               'ErrorMessage':None})
      self.__EP_LIN_SOLVER = val
    else:
      errorMessage = "EP_LIN_SOLVER iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'EP_LIN_SOLVER',
                               'Success':False,'Previous':self.__EP_LIN_SOLVER,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'EP_LIN_SOLVER','ErrorMessage':errorMessage})

  @GEO_OPT.setter
  def GEO_OPT(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'GEO_OPT',
                               'Success':True,'Previous':self.__GEO_OPT,'New':val,
                               'ErrorMessage':None})
      self.__GEO_OPT = val
    else:
      errorMessage = "GEO_OPT iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'GEO_OPT',
                               'Success':False,'Previous':self.__GEO_OPT,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'GEO_OPT','ErrorMessage':errorMessage})

  @JUST_ENERGY.setter
  def JUST_ENERGY(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'JUST_ENERGY',
                               'Success':True,'Previous':self.__JUST_ENERGY,'New':val,
                               'ErrorMessage':None})
      self.__JUST_ENERGY = val
    else:
      errorMessage = "JUST_ENERGY iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'JUST_ENERGY',
                               'Success':False,'Previous':self.__JUST_ENERGY,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'JUST_ENERGY','ErrorMessage':errorMessage})
 
  @MD.setter
  def MD(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'MD',
                               'Success':True,'Previous':self.__MD,'New':val,
                               'ErrorMessage':None})
      self.__MD = val
    else:
      errorMessage = "MD iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'MD',
                               'Success':False,'Previous':self.__MD,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'MD','ErrorMessage':errorMessage})

  @METADYNAMICS.setter
  def METADYNAMICS(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'METADYNAMICS',
                               'Success':True,'Previous':self.__METADYNAMICS,'New':val,
                               'ErrorMessage':None})
      self.__METADYNAMICS = val
    else:
      errorMessage = "METADYNAMICS iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'METADYNAMICS',
                               'Success':False,'Previous':self.__METADYNAMICS,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'METADYNAMICS','ErrorMessage':errorMessage})

  @PINT.setter
  def PINT(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'PINT',
                               'Success':True,'Previous':self.__PINT,'New':val,
                               'ErrorMessage':None})
      self.__PINT = val
    else:
      errorMessage = "PINT iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'PINT',
                               'Success':False,'Previous':self.__PINT,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'PINT','ErrorMessage':errorMessage})

  @POWELL_OPT.setter
  def POWELL_OPT(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'POWELL_OPT',
                               'Success':True,'Previous':self.__POWELL_OPT,'New':val,
                               'ErrorMessage':None})
      self.__POWELL_OPT = val
    else:
      errorMessage = "POWELL_OPT iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'POWELL_OPT',
                               'Success':False,'Previous':self.__POWELL_OPT,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'POWELL_OPT','ErrorMessage':errorMessage})

  @QS_SCF.setter
  def QS_SCF(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'QS_SCF',
                               'Success':True,'Previous':self.__QS_SCF,'New':val,
                               'ErrorMessage':None})
      self.__QS_SCF = val
    else:
      errorMessage = "QS_SCF iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'QS_SCF',
                               'Success':False,'Previous':self.__QS_SCF,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'QS_SCF','ErrorMessage':errorMessage})

  @REPLICA_EVAL.setter
  def REPLICA_EVAL(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'REPLICA_EVAL',
                               'Success':True,'Previous':self.__REPLICA_EVAL,'New':val,
                               'ErrorMessage':None})
      self.__REPLICA_EVAL = val
    else:
      errorMessage = "REPLICA_EVAL iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'REPLICA_EVAL',
                               'Success':False,'Previous':self.__REPLICA_EVAL,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'REPLICA_EVAL','ErrorMessage':errorMessage})

  @ROT_OPT.setter
  def ROT_OPT(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'ROT_OPT',
                               'Success':True,'Previous':self.__ROT_OPT,'New':val,
                               'ErrorMessage':None})
      self.__ROT_OPT = val
    else:
      errorMessage = "ROT_OPT iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'ROT_OPT',
                               'Success':False,'Previous':self.__ROT_OPT,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'ROT_OPT','ErrorMessage':errorMessage})

  @SHELL_OPT.setter
  def SHELL_OPT(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'SHELL_OPT',
                               'Success':True,'Previous':self.__SHELL_OPT,'New':val,
                               'ErrorMessage':None})
      self.__SHELL_OPT = val
    else:
      errorMessage = "SHELL_OPT iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'SHELL_OPT',
                               'Success':False,'Previous':self.__SHELL_OPT,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'SHELL_OPT','ErrorMessage':errorMessage})

  @SPLINE_FIND_COEFFS.setter
  def SPLINE_FIND_COEFFS(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH',
                               'Variable':'SPLINE_FIND_COEFFS','Success':True,
                               'Previous':self.__SPLINE_FIND_COEFFS,'New':val,'ErrorMessage':None})
      self.__SPLINE_FIND_COEFFS = val
    else:
      errorMessage = "SPLINE_FIND_COEFFS iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH',
                               'Variable':'SPLINE_FIND_COEFFS','Success':False,
                               'Previous':self.__SHELL_OPT,'New':val,'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'SPLINE_FIND_COEFFS','ErrorMessage':errorMessage})

  @TDDFT_SCF.setter
  def TDDFT_SCF(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'TDDFT_SCF',
                               'Success':True,'Previous':self.__TDDFT_SCF,'New':val,
                               'ErrorMessage':None})
      self.__TDDFT_SCF = val
    else:
      errorMessage = "TDDFT_SCF iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'TDDFT_SCF',
                               'Success':False,'Previous':self.__TDDFT_SCF,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'TDDFT_SCF','ErrorMessage':errorMessage})

  @XAS_SCF.setter
  def XAS_SCF(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'XAS_SCF',
                               'Success':True,'Previous':self.__XAS_SCF,'New':val,
                               'ErrorMessage':None})
      self.__XAS_SCF = val
    else:
      errorMessage = "XAS_SCF iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'XAS_SCF',
                               'Success':False,'Previous':self.__XAS_SCF,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'XAS_SCF','ErrorMessage':errorMessage})

  @ROOT.setter
  def ROOT(self,val):
    if utilities.is_positive_integer(val):
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'ROOT',
                               'Success':True,'Previous':self.__ROOT,'New':val,
                               'ErrorMessage':None})
      self.__ROOT = val
    else:
      errorMessage = "ROOT iteration level must be a positive integer."
      self.__changeLog.append({'Date':datetime.datetime.now(),'Module':'EACH','Variable':'ROOT',
                               'Success':False,'Previous':self.__ROOT,'New':val,
                               'ErrorMessage':errorMessage})
      self.__errorLog.append({'Date':datetime.datetime.now(),'Type':'Setter','Module':'EACH',
                              'Variable':'ROOT','ErrorMessage':errorMessage})
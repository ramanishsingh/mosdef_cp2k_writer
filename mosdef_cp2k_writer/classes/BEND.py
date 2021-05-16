import datetime
import mosdef_cp2k_writer.utilities as utilities


BOOL_VALS = [".TRUE.", ".FALSE.", "TRUE", "FALSE"]

KIND_LIST=["AMBER","CHARMM","CUBIC","G87","G96","HARMONIC","LEGENDRE","MIXED_BEND_STRETCH","MM3"]
def _validate_ATOMS(val, errorLog=[]):
    if utilities.is_string(val) or (val is None):
        return val
    else:
        errorMessage = "ATOMS should be a string containing atom names"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "ATOMS",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_CB(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "CB should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "CB",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_K(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "K should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "K",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_KBS12(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "KBS12 should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "KBS12",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError
        
        
def _validate_KBS32(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "KBS32 should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "KBS32",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError
        
        

        
def _validate_KIND(val, errorLog=[]):

    if val is not None:
        val = str(val).upper()

    if val in KIND_VALS or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for KIND BEND: {}. Valid options are: {}".format(
                val, KIND_VALS
            )
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "KIND",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError
        
        
def _validate_KSS(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "KSS should be a number"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "KSS",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError       
        
def _validate_LEGENDRE(val, errorLog=[]):
    if utilities.is_string(val) or (val is None):
        return val
    else:
        errorMessage = "LEGENDRE should be string"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "LEGENDRE",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError
        
def _validate_R012(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "R012 should be number"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "R012",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError
        
        
def _validate_R013(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "R013 should be a number"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BEND",
                "Variable": "R013",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError
        
        
def _validate_THETA0(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "THETA0 should be number"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "THETA0",
                "Variable": "KIND",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError        

class BEND:
    def __init__(
        self,
        ATOMS=None,
        CB=None,
        K=None,
        KBS12=None,
        KBS32=None,
        KIND=None,
        KSS=None,
        LEGENDRE=None,
        R012=None,
        R032=None,
        THETA0=None,
        errorLog=[],
        changeLog=[],
        location="",
    ):
        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__location = "{}/BEND".format(location)
        self.__ATOMS = _validate_ATOMS(ATOMS, errorLog=self.__errorLog)
        self.__CB = _validate_CB(CB, errorLog=self.__errorLog)
        self.__K = _validate_K(K, errorLog=self.__errorLog)
        self.__KBS12 = _validate_KBS12(KBS12, errorLog=self.__errorLog)
        self.__KBS32 = _validate_KBS32(KBS32, errorLog=self.__errorLog)
        self.__KIND = _validate_KIND(KIND, errorLog=self.__errorLog)
        self.__KSS = _validate_KSS(KSS, errorLog=self.__errorLog)
        self.__LEGENDRE = _validate_LEGENDRE(LEGENDRE, errorLog=self.__errorLog)
        self.__R012 = _validate_R012(R012, errorLog=self.__errorLog)
        self.__R032 = _validate_R032(R012, errorLog=self.__errorLog)
        self.__THETA0 = _validate_THETA0(THETA0, errorLog=self.__errorLog)

       

    @property
    def errorLog(self):
        return self.__errorLog

    @property
    def changeLog(self):
        return self.__changeLog

    @property
    def location(self):
        return self.__location

    @property
    def ATOMS(self):
        return self.__ATOMS

    @property
    def CB(self):
        return self.__CB

    @property
    def K(self):
        return self.__K

    @property
    def KBS12(self):
        return self.__KBS12

    @property
    def KBS32(self):
        return self.__KBS32

    @property
    def KIND(self):
        return self.__KIND

    @property
    def KSS(self):
        return self.__KSS
    
    @property
    def LEGENDRE(self):
        return self.__LEGENDRE

    @property
    def R012(self):
        return self.__R012

    @property
    def R032(self):
        return self.__R032

    @property
    def THETA0(self):
        return self.__THETA0

    @ATOMS.setter
    def ATOMS(self, val):
        if utilities.is_string(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "ATOMS",
                    "Success": True,
                    "Previous": self.__ATOMS,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__ATOMS = val
        else:
            errorMessage = "ATOMS must be a string containing the name of the atoms in the bend angle separated by spaces."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "ATOMS",
                    "Success": False,
                    "Previous": self.__ATOMS,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "ATOMS",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @CB.setter
    def CB(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "CB",
                    "Success": True,
                    "Previous": self.__CB,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__CB = val
        else:
            errorMessage = "CB must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "CB",
                    "Success": False,
                    "Previous": self.__CB,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "CB",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
            
    @K.setter
    def K(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "K",
                    "Success": True,
                    "Previous": self.__K,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__K = val
        else:
            errorMessage = "K in BEND must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "K",
                    "Success": False,
                    "Previous": self.__K,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "K",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
    @KBS12.setter
    def KBS12(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "KBS12",
                    "Success": True,
                    "Previous": self.__KBS12,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__KBS12 = val
        else:
            errorMessage = "KBS12 must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "KBS12",
                    "Success": False,
                    "Previous": self.__KBS12,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "KBS12",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
            
            
    @KBS32.setter
    def KBS32(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "KBS32",
                    "Success": True,
                    "Previous": self.__KBS32,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__KBS32 = val
        else:
            errorMessage = "KBS32 must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "KBS32",
                    "Success": False,
                    "Previous": self.__KBS32,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "KBS32",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
            
    @KIND.setter
    def KIND(self, val):
        if utilities.is_string(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "KIND",
                    "Success": True,
                    "Previous": self.__KIND,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__KIND = val
        else:
            errorMessage = "KIND must be a string ."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "KIND",
                    "Success": False,
                    "Previous": self.__KIND,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "KIND",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
    @KSS.setter
    def KSS(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "KSS",
                    "Success": True,
                    "Previous": self.__KSS,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__KSS = val
        else:
            errorMessage = "KSS must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "KSS",
                    "Success": False,
                    "Previous": self.__KSS,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "KSS",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
    @LEGENDRE.setter
    def LEGENDRE(self, val):
        if utilities.is_string(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "LEGENDRE",
                    "Success": True,
                    "Previous": self.__LEGENDRE,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__LEGENDRE = val
        else:
            errorMessage = "LEGENDRE must be a string (of numbers)."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "LEGENDRE",
                    "Success": False,
                    "Previous": self.__LEGENDRE,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "LEGENDRE",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )      
            
            
    @R012.setter
    def R012(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "R012",
                    "Success": True,
                    "Previous": self.__R012,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__R012 = val
        else:
            errorMessage = "R012 must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "R012",
                    "Success": False,
                    "Previous": self.__R012,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "R012",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
            
    @R032.setter
    def R032(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "R032",
                    "Success": True,
                    "Previous": self.__R032,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__R032 = val
        else:
            errorMessage = "R032 must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "R032",
                    "Success": False,
                    "Previous": self.__R032,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "R032",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
            
            
    @THETA0.setter
    def THETA0(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "THETA0",
                    "Success": True,
                    "Previous": self.__THETA0,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__THETA0 = val
        else:
            errorMessage = "THETA0 must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BEND",
                    "Variable": "THETA0",
                    "Success": False,
                    "Previous": self.__THETA0,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BEND",
                    "Variable": "THETA0",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
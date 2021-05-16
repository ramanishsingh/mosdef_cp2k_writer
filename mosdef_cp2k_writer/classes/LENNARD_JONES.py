import datetime
import mosdef_cp2k_writer.utilities as utilities


BOOL_VALS = [".TRUE.", ".FALSE.", "TRUE", "FALSE"]




def _validate_ATOMS(val, errorLog=[]):
    if utilities.is_string(val) or (val is None):
        return val
    else:
        errorMessage = "ATOMS should be a string containing atom names separated by space"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "LENNARD-JONES",
                "Variable": "ATOMS",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_EPSILON(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "EPSILON should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "LENNARD-JONES",
                "Variable": "EPSILON",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError

def _validate_RCUT(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "RCUT should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "LENNARD-JONES",
                "Variable": "RCUT",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError

def _validate_RMAX(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "RMAX should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "LENNARD-JONES",
                "Variable": "RMAX",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError

        
def _validate_RMIN(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "RMIN should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "LENNARD-JONES",
                "Variable": "RMIN",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError
        
def _validate_SIGMA(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "SIGMA should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "LENNARD-JONES",
                "Variable": "SIGMA",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError  
        
        
class LENNARD_JONES:
    def __init__(
        self,
        ATOMS=None,
        EPSILON=None,
        RCUT=None,
        RMAX=None,
        RMIN=None,
        SIGMA=None,
        
        errorLog=[],
        changeLog=[],
        location="",
    ):
        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__location = "{}/CHARGE".format(location)
        self.__ATOMS = _validate_ATOMS(ATOMS, errorLog=self.__errorLog)
        self.__EPSILON = _validate_EPSILON(EPSILON, errorLog=self.__errorLog)
        self.__RCUT = _validate_RCUT(RCUT, errorLog=self.__errorLog)
        self.__RMAX = _validate_RMAX(RMAX, errorLog=self.__errorLog)
        self.__RMIN = _validate_RMIN(RMIN, errorLog=self.__errorLog)
        self.__SIGMA = _validate_SIGMA(SIGMA, errorLog=self.__errorLog)



       

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
    def EPSILON(self):
        return self.__EPSILON

    @property
    def RCUT(self):
        return self.__RCUT
    @property
    def RMAX(self):
        return self.__EPRMAX  
    
    @property
    def RMIN(self):
        return self.__RMIN
    
    @property
    def SIGMA(self):
        return self.__SIGMA
    
    
    @ATOMS.setter
    def ATOMS(self, val):
        if utilities.is_string(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
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
            errorMessage = "ATOMS must be a string containing the names of the atoms."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
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
                    "Module": "LENNARD-JONES",
                    "Variable": "ATOMS",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @EPSILON.setter
    def EPSILON(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "EPSILON",
                    "Success": True,
                    "Previous": self.__EPSILON,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__EPSILON = val
        else:
            errorMessage = "EPSILON must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "EPSILON",
                    "Success": False,
                    "Previous": self.__EPSILON,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "LENNARD-JONES",
                    "Variable": "EPSILON",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @RCUT.setter
    def RCUT(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "RCUT",
                    "Success": True,
                    "Previous": self.__RCUT,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__RCUT = val
        else:
            errorMessage = "RCUT must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "RCUT",
                    "Success": False,
                    "Previous": self.__RCUT,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "LENNARD-JONES",
                    "Variable": "RCUT",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
            
            
    @RMIN.setter
    def RMIN(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "RMIN",
                    "Success": True,
                    "Previous": self.__RMIN,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__RMIN = val
        else:
            errorMessage = "RMIN must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "RMIN",
                    "Success": False,
                    "Previous": self.__RMIN,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "LENNARD-JONES",
                    "Variable": "RMIN",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
            
    @RMAX.setter
    def RMAX(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "RMAX",
                    "Success": True,
                    "Previous": self.__RMAX,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__RMAX = val
        else:
            errorMessage = "RMAX must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "RMAX",
                    "Success": False,
                    "Previous": self.__RMAX,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "LENNARD-JONES",
                    "Variable": "RMAX",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
            
            
            
    @SIGMA.setter
    def SIGMA(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "SIGMA",
                    "Success": True,
                    "Previous": self.__SIGMA,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__SIGMA = val
        else:
            errorMessage = "SIGMA must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "LENNARD-JONES",
                    "Variable": "SIGMA",
                    "Success": False,
                    "Previous": self.__EPSILON,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "LENNARD-JONES",
                    "Variable": "SIGMA",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
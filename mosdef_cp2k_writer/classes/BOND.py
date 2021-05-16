import datetime
import mosdef_cp2k_writer.utilities as utilities


BOOL_VALS = [".TRUE.", ".FALSE.", "TRUE", "FALSE"]

KIND_VALS=["AMBER","CHARMM","CUBIC","FUES", "G87","G96","HARMONIC","MORSE","QUARTIC"]


def _validate_ATOMS(val, errorLog=[]):
    if utilities.is_string(val) or (val is None):
        return val
    else:
        errorMessage = "ATOMS should be a string containing atom names"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BOND",
                "Variable": "ATOMS",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_CS(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "CB should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BOND",
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
                "Module": "BOND",
                "Variable": "K",
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
            "Invalid option for KIND BOND: {}. Valid options are: {}".format(
                val, KIND_VALS
            )
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BOND",
                "Variable": "KIND",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError
        
        

        
def _validate_R0(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "R0 should be number"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BOND",
                "Variable": "R0",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError        

class BOND:
    def __init__(
        self,
        ATOMS=None,
        CS=None,
        K=None,
        KIND=None,
        R0=None,

        errorLog=[],
        changeLog=[],
        location="",
    ):
        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__location = "{}/BOND".format(location)
        self.__ATOMS = _validate_ATOMS(ATOMS, errorLog=self.__errorLog)
        self.__CS = _validate_CS(CS, errorLog=self.__errorLog)
        self.__K = _validate_K(K, errorLog=self.__errorLog)
        self.__KIND = _validate_KIND(KIND, errorLog=self.__errorLog)
        self.__R0 = _validate_R0(R0, errorLog=self.__errorLog)


       

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
    def CS(self):
        return self.__CS

    @property
    def K(self):
        return self.__K



    @property
    def KIND(self):
        return self.__KIND


    @property
    def R0(self):
        return self.__R0

    @ATOMS.setter
    def ATOMS(self, val):
        if utilities.is_string(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOND",
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
                    "Module": "BOND",
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
                    "Module": "BOND",
                    "Variable": "ATOMS",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @CS.setter
    def CS(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOND",
                    "Variable": "CS",
                    "Success": True,
                    "Previous": self.__CS,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__CS = val
        else:
            errorMessage = "CS must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOND",
                    "Variable": "CS",
                    "Success": False,
                    "Previous": self.__CS,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BOND",
                    "Variable": "CS",
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
                    "Module": "BOND",
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
            errorMessage = "K in BOND must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOND",
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
                    "Module": "BOND",
                    "Variable": "K",
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
                    "Module": "BOND",
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
                    "Module": "BOND",
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
                    "Module": "BOND",
                    "Variable": "KIND",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            
 
    @R0.setter
    def R0(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOND",
                    "Variable": "R0",
                    "Success": True,
                    "Previous": self.__R0,
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
                    "Module": "BOND",
                    "Variable": "R0",
                    "Success": False,
                    "Previous": self.__R0,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BOND",
                    "Variable": "R0",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
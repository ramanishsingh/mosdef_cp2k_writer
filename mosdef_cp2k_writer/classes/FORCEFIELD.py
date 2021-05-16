import datetime
import mosdef_cp2k_writer.utilities as utilities
from mosdef_cp2k_writer.classes import SPLINE
from mosdef_cp2k_writer.classes import BENDs
from mosdef_cp2k_writer.classes import BONDs
from mosdef_cp2k_writer.classes import CHARGEs
from mosdef_cp2k_writer.classes import NONBONDED
from mosdef_cp2k_writer.utilities1 import oneDimArray as oda
from mosdef_cp2k_writer.utilities1 import objectArray as oba

BOOL_VALS = [".TRUE.", ".FALSE.", "TRUE", "FALSE"]

PARMTYPE_VALS = ["AMBER", "CHM", "G87", "G96", "OFF"]


def _validate_PARM_FILE_NAME(val, errorLog=[]):
    if utilities.is_string(val) or (val is None):
        return val
    else:
        errorMessage = "PARM_FILE_NAME must be string."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "FORCEFIELD",
                "Variable": "PARM_FILE_NAME",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_PARMTYPE(val, errorLog=[]):
    if val is not None:
        val = str(val).upper()

    if val in PARMTYPE_VALS or (val is None):
        return val
    else:
        errorMessage = "Invalid option for PARMTYPE: {}. Valid options are: {}".format(
            val, PARMTYPE_VALS
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "FORCEFIELD",
                "Variable": "PARMTYPE",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


class FORCEFIELD:
    def __init__(
        self, PARM_FILE_NAME=None, PARMTYPE=None, errorLog=[], changeLog=[], location=""
    ):
        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__location = "{}/FORCEFIELD".format(location)
        self.__PARM_FILE_NAME = _validate_PARM_FILE_NAME(
            PARM_FILE_NAME, errorLog=self.__errorLog
        )
        self.__PARMTYPE = _validate_PARMTYPE(PARMTYPE, errorLog=self.__errorLog)
        self.__SPLINE = SPLINE.SPLINE(
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )
        self.__BEND = None
        self.__BOND = None
        
        self.__CHARGE = None
        
        self.__NONBONDED = NONBONDED.NONBONDED(
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )


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
    def PARMTYPE(self):
        return self.__PARMTYPE

    @property
    def PARM_FILE_NAME(self):
        return self.__PARM_FILE_NAME

    @property
    def SPLINE(self):
        return self.__SPLINE
    
    @property
    def BEND(self):
        return self.__BEND
    
    
    def init_bends(self, natomty):
        BEND = []
        for i in range(natomty):
            BEND.append(
                BENDs.BENDs(
                    number=i + 1,
                    errorLog=self.__errorLog,
                    changeLog=self.__changeLog,
                    location=self.__location,
                )
            )
        self.__BEND = oba.objectArray.listToOBA(
            BEND,
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )

    
    @property
    def BOND(self):
        return self.__BOND
    
    def init_bonds(self, natomty):
        BOND = []
        for i in range(natomty):
            BOND.append(
                BONDs.BONDs(
                    number=i + 1,
                    errorLog=self.__errorLog,
                    changeLog=self.__changeLog,
                    location=self.__location,
                )
            )
        self.__BOND = oba.objectArray.listToOBA(
            BOND,
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )

    
    
    @property
    def CHARGE(self):
        return self.__CHARGE
    
    def init_charges(self, natomty):
        CHARGE = []
        for i in range(natomty):
            CHARGE.append(
                CHARGEs.CHARGEs(
                    number=i + 1,
                    errorLog=self.__errorLog,
                    changeLog=self.__changeLog,
                    location=self.__location,
                )
            )
        self.__CHARGE = oba.objectArray.listToOBA(
            CHARGE,
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )

        
    @property
    def NONBONDED(self):
        return self.__NONBONDED
    
    
    @PARMTYPE.setter
    def PARMTYPE(self, val):
        val = str(val).upper()
        if val in PARMTYPE_VALS:
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "FORCEFIELD",
                    "Variable": "PARMTYPE",
                    "Success": True,
                    "Previous": self.__PARMTYPE,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PARMTYPE = val
        else:
            errorMessage = (
                "Invalid option for PARMTYPE: {}. Valid options are: {}".format(
                    val, PARMTYPE_VALS
                )
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "FORCEFIELD",
                    "Variable": "PARMTYPE",
                    "Success": False,
                    "Previous": self.__PARMTYPE,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "FORCEFIELD",
                    "Variable": " PARMTYPE",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @PARM_FILE_NAME.setter
    def PARM_FILE_NAME(self, val):
        if utilities.is_string(val) or (val is None):

            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "FORCEFIELD",
                    "Variable": "PARM_FILE_NAME",
                    "Success": True,
                    "Previous": self.__PARM_FILE_NAME,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PARM_FILE_NAME = val

        else:
            errorMessage = "Invalid option for PARM_FILE_NAME. It should be a string."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "FORCEFIELD",
                    "Variable": "PARM_FILE_NAME",
                    "Success": False,
                    "Previous": self.__PARM_FILE_NAME,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "FORCEFIELD",
                    "Variable": "PARM_FILE_NAME",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

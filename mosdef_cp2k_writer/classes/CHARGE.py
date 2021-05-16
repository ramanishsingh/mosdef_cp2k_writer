import datetime
import mosdef_cp2k_writer.utilities as utilities


BOOL_VALS = [".TRUE.", ".FALSE.", "TRUE", "FALSE"]




def _validate_ATOM(val, errorLog=[]):
    if utilities.is_string(val) or (val is None):
        return val
    else:
        errorMessage = "ATOM should be a string containing atom name"
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "CHARGE",
                "Variable": "ATOM",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_CHARGE(val, errorLog=[]):
    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "CHARGE should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "CHARGE",
                "Variable": "CHARGE",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError



class CHARGE:
    def __init__(
        self,
        ATOM=None,
        CHARGE=None,

        errorLog=[],
        changeLog=[],
        location="",
    ):
        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__location = "{}/CHARGE".format(location)
        self.__ATOM = _validate_ATOM(ATOM, errorLog=self.__errorLog)
        self.__CHARGE = _validate_CHARGE(CHARGE, errorLog=self.__errorLog)



       

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
    def ATOM(self):
        return self.__ATOM

    @property
    def CHARGE(self):
        return self.__CHARGE


    @ATOM.setter
    def ATOM(self, val):
        if utilities.is_string(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "CHARGE",
                    "Variable": "ATOM",
                    "Success": True,
                    "Previous": self.__ATOM,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__ATOM = val
        else:
            errorMessage = "ATOM must be a string containing the name of the atom."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "CHARGE",
                    "Variable": "ATOM",
                    "Success": False,
                    "Previous": self.__ATOM,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "CHARGE",
                    "Variable": "ATOM",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @CHARGE.setter
    def CHARGE(self, val):
        if utilities.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "CHARGE",
                    "Variable": "CHARGE",
                    "Success": True,
                    "Previous": self.__CHARGE,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__CHARGE = val
        else:
            errorMessage = "CHARGE must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "CHARGE",
                    "Variable": "CHARGE",
                    "Success": False,
                    "Previous": self.__CHARGE,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "CHARGE",
                    "Variable": "CHARGE",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
     
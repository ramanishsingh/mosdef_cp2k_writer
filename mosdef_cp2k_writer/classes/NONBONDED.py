import datetime
import mosdef_cp2k_writer.utilities as utilities
from mosdef_cp2k_writer.classes import LENNARD_JONESs
from mosdef_cp2k_writer.utilities1 import oneDimArray as oda
from mosdef_cp2k_writer.utilities1 import objectArray as oba
BOOL_VALS = [".TRUE.", ".FALSE.", "TRUE", "FALSE"]




class NONBONDED:
    def __init__(self, errorLog=[], changeLog=[], location=""):
        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__location = "{}/NONBONDED".format(location)
        
        self.__LENNARD_JONES =None

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
    def LENNARD_JONES(self):
        return self.__LENNARD_JONES
    
    def init_lennard_joness(self, natomty):
        LENNARD_JONES = []
        for i in range(natomty):
            LENNARD_JONES.append(
                LENNARD_JONESs.LENNARD_JONESs(
                    number=i + 1,
                    errorLog=self.__errorLog,
                    changeLog=self.__changeLog,
                    location=self.__location,
                )
            )
        self.__LENNARD_JONES = oba.objectArray.listToOBA(
            LENNARD_JONES,
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
        )
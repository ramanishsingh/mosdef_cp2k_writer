#quick test that works on my laptop
from mosdef_cp2k_writer.classes import SIM as sim

mySim = sim.SIM()
mySim.GLOBAL.RUN_TYPE = "MC"




mySim = sim.SIM()
mySim.GLOBAL.PRINT_LEVEL="LOW"
mySim.GLOBAL.RUN_TYPE = "MC"
mySim.GLOBAL.PROJECT_NAME = "test_bias"
mySim.GLOBAL.PRINT_LEVEL = "LOW"
mySim.GLOBAL.SEED = 2

mySim.FORCE_EVAL.METHOD="FIST"



mySim.FORCE_EVAL.MM.FORCEFIELD.init_bonds(2)

mySim.FORCE_EVAL.MM.FORCEFIELD.BOND[1].K=1
mySim.FORCE_EVAL.MM.FORCEFIELD.BOND[2].K=1

mySim.FORCE_EVAL.MM.FORCEFIELD.BOND[1].ATOMS="Ar Ar"
mySim.FORCE_EVAL.MM.FORCEFIELD.BOND[2].ATOMS="Ar Ar"


mySim.FORCE_EVAL.MM.FORCEFIELD.init_charges(2)

mySim.FORCE_EVAL.MM.FORCEFIELD.CHARGE[1].CHARGE=1

mySim.FORCE_EVAL.MM.FORCEFIELD.CHARGE[2].CHARGE=1


mySim.FORCE_EVAL.MM.FORCEFIELD.NONBONDED.init_lennard_joness(2)

mySim.FORCE_EVAL.MM.FORCEFIELD.NONBONDED.LENNARD_JONES[1].SIGMA=2
mySim.FORCE_EVAL.MM.FORCEFIELD.NONBONDED.LENNARD_JONES[2].RCUT=7



mySim.write_changeLog(fn="iodine-changeLog.out")
mySim.write_errorLog()
mySim.write_inputFile(fn="sample.inp")


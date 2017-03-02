def VASP(cpu, jobname, queue, dirpath):
    queue = queue
    cpu = int(cpu)
    jobname = dirpath.split("/")[-1]
    mpi = """#!/bin/csh
# pe request
#$ -pe mpi_%d %d
# our Job name
#$ -N V%s
#$ -S /bin/csh
#$ -q %s
#$ -V
#$ -cwd\necho "Got $NSLOTS slots."
cat $TMPDIR/machines
cd %s
mpirun -np $NSLOTS /opt/vasp/vasp.5.4.1/bin/vasp
"""%(cpu, cpu, jobname, queue, dirpath)

    return mpi

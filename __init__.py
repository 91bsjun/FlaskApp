import os

from flask import Flask, render_template, flash, request, url_for, redirect, session, send_from_directory
from werkzeug import secure_filename


app = Flask(__name__)

# ---------------------------------------------------------------------- #
#                               Main page                                #
# ---------------------------------------------------------------------- #
@app.route('/')
def homepage():
    try:
        username=session['username']
    except:
        username="not login"
    return render_template("01_main.html", username=username)


# ---------------------------------------------------------------------- #
#                              Log-in page                               #
# ---------------------------------------------------------------------- #
@app.route('/login/')
def login():
    return render_template("03_login.html")


# ---------------------------------------------------------------------- #
#                               VASP page                                #
# ---------------------------------------------------------------------- #
@app.route('/vasp/', methods=['POST', 'GET'])
def vasp():
    status = "init"

    return render_template("11_vasp.html", status=status)

app.config['VASP_ALLOWED_EXTENSIONS'] = set(["cif","xsd","POSCAR"])
def vasp_allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1] in app.config['VASP_ALLOWED_EXTENSIONS']

@app.route('/vasp/vasp_upload', methods=['POST', 'GET'])
def vasp_upload():
    # ---------------- get username ---------------- #
    try:
        username = "bsjun"
    except:
        username = "guest"
    try:
        os.mkdir("C:\\Users\\bsjun\\Documents\\Codes\\FlaskApp\\static\\VASP\\" + username)
    except:
        pass

    # --------------- Check jobname ---------------- #
    jobname = request.form['jobname']
    if len(jobname) <= 1:
        flash("Please fill the \"Job Name\" field.")
        return redirect(url_for('vasp'))

    # ---------------- File upload ---------------- #
    source_path = "C:\\Users\\bsjun\\Documents\\Codes\\FlaskApp\\static\\VASP\\" + username + "\\"+jobname
    try:
        os.mkdir(source_path)
    except:
        pass
    os.chdir(source_path)

    uploaded_files = request.files.getlist("file[]")
    filenames = []
    for file in uploaded_files:
        if file and vasp_allowed_file(file.filename):
            filename = secure_filename(file.filename)
            file.save(os.path.join(source_path, filename))
            filenames.append(filename)

    status = "file_uploaded"

    return render_template("11_vasp.html", jobname=jobname, filenames=filenames, status=status,
                           incar_dict=incar_dict, incar_keys=incar_keys, incar_description_dict=incar_description_dict,
                           magmom_dict=magmom_dict, magmom_keys=magmom_keys,
                           LDAUU_dict=LDAUU_dict, LDAUU_keys=LDAUU_keys)


@app.route('/vaspinputgen', methods=['POST', 'GET'])
def vasp_inputGen():
    # ---------------- get username ---------------- #
    try:
        username = "bsjun"
    except:
        username = "guest"

    # --------------- get INCAR dict --------------- #
    new_incar_dict = {}
    for key in incar_keys:
        new_incar_dict[key] = request.form[key]

    # ---------- parse INCAR radio button ---------- #
    magmom_radio = request.form["magmom"]
    if magmom_radio == "magmom":
        magmom_use = True
        new_magmom_dict = {}
        for key in magmom_keys:
            new_magmom_dict[key] = request.form['mag_'+key]
    else:
        magmom_use = False
        new_magmom_dict = None

    ldau_radio = request.form["ldau"]
    if ldau_radio == "ldau":
        ldau_use = True
        new_LDAUU_dict = {}
        for key in LDAUU_keys:
            new_LDAUU_dict[key] = request.form['ldau_'+key]
    else:
        ldau_use = False
        new_LDAUU_dict = {}

    grimme_radio = request.form["grimme"]
    if grimme_radio == "grimme":
        grimme_use = True
    else:
        grimme_use = False

    # ---------------- parse POTCAR ---------------- #
    functional = request.form['vasp_potential']

    # ---------------- parse KPOINTS --------------- #
    kpts_radio = request.form["kpts"]
    if kpts_radio == "kpts":
        a = request.form["kpt_a"]
        b = request.form["kpt_b"]
        c = request.form["kpt_c"]
        kpoints = [a,b,c]
    else:
        kpoints=False

    # ---------------- get job name ---------------- #
    jobname = request.form['jobname']
    source_path = "C:\\Users\\bsjun\\Documents\\Codes\\FlaskApp\\static\\VASP\\" + username + "\\" + jobname
    os.chdir(source_path)

    # ------------ make VASP inputfiles ------------ #
    from CCpy.VASP.VASPio import VASPInput
    input_files = [f for f in os.listdir(source_path) if not os.path.isdir(f)]
    for each_input in input_files:
        VI = VASPInput(each_input)
        VI.cms_vasp_set(vdw=grimme_use, mag=magmom_use, ldau=ldau_use,
                        functional=functional,
                        incar_dict=new_incar_dict, magmom_dict=new_magmom_dict, ldau_dict=new_LDAUU_dict,
                        kpoints=kpoints, flask_app=True)

    status="input_generated"

    return render_template("11_vasp.html", status=status, jobname=jobname)


@app.route('/vaspdownload/', methods=['POST', 'GET'])
def vasp_input_download():
    # ---------------- get username ---------------- #
    try:
        username = "bsjun"
    except:
        username = "guest"

    method_radio = request.form['scp_method']
    if method_radio == "cmsuser":
        pass
    elif method_radio == "otherserver":
        pass

    return redirect(url_for('vasp'))

incar_dict = {
    "NWRITE":2,"LPETIM":"F","ISTART":0,"INIWAV":1,"IWAVPR":1,"ICHARG":2,"LCHARG":".TRUE.","LWAVE":".FALSE.",
    "ALGO":"FAST","NELM":100,"EDIFF":0.0001,"BMIX":3.00,"ENCUT":500,"GGA":"PE","ISYM":2,
    "LDIAG":"T","LREAL":"auto","PREC":"Medium",
    "NSW":200,"NBLOCK":1,"KBLOCK":10,"IBRION":2,"ISIF":3,"EDIFFG":-0.04,"POTIM":0.5,"SMASS":3.0,
    "ISMEAR":0,"SIGMA":0.05,"LORBIT":11,
    "NPAR":8,"LPLANE":"T","ISPIN":1,
    "LDAU":".FALSE.","LMAXMIX":4,"LDAUTYPE":2,
    "LVDW":".FALSE.", "VDW_RADIUS":30.0, "VDW_SCALING":0.75, "VDW_D":20.0}
incar_description_dict = {
    "NWRITE":"write-flag",
    "LPETIM":"timer",
    "ISTART":"job : 0-new  1-contEcut  2-sameBS",
    "INIWAV":"0-jellium  1-random",
    "IWAVPR":"prediction:  0-non 1-charg 2-wave 3-comb",
    "ICHARG":"0-from WF  1-from CHGCAR  2-from atom  11-12-fixed",
    "LCHARG":"determine whether charge densities (file CHGCAR and CHG) are written",
    "LWAVE":"determine whether the orbitals (file WAVECAR) are written",
    "ALGO":"specify the electronic minimisation algorithm",
    "NELM":"number of iterations",
    "EDIFF":"stopping-criterion for ELM",
    "BMIX":"sets the cutoff wave vector for Kerker mixing scheme",
    "ENCUT":"Cut-Off Energy",
    "GGA":"GGA functional",
    "ISYM":"switch symmetry stuff ON (1 or 2) or OFF (0)",
    "LDIAG":"sub-space diagonalisation",
    "LREAL":"real-space projection",
    "PREC":"accuracy",
    "NSW":"number of steps for IOM",
    "NBLOCK":"inner block",
    "KBLOCK":"outer block",
    "IBRION":"ionic relax: 0-MD 1-quasi-New 2-CG",
    "ISIF":"ion&cell relax: 0-MD 2-ion&stress 3-ion&cell&stress",
    "EDIFFG":"Criterion for geom opt (eV/Ang)",
    "POTIM":"time-step for ionic motion (fs)",
    "SMASS":"Nose mass-parameter (am)",
    "ISMEAR":"Broadening methode -5-tet -1-fermi 0-gaus 1-mp 2-mp2",
    "SIGMA":"Broadening in eV",
    "LORBIT":"l-decomposed DOS",
    "NPAR":"Parallelization for Bands",
    "LPLANE":"Parallelization for PWs",
    "ISPIN":"1-no spin  2-spin",
    "LDAU":"use LDA+U use or not.",
    "LMAXMIX":"",
    "LDAUTYPE":"",
    "LVDW":"use DFT-D2 or not",
    "VDW_RADIUS":"",
    "VDW_SCALING":"",
    "VDW_D":""}
incar_keys = list(incar_dict.keys())
incar_keys.sort()

# Magnetic moment parameters : from Pymatgen
magmom_dict = {'Mn3+': 4, 'Ni4+': 0.6, 'Cr': 5, 'Mn4+': 3, 'Ta': 5, 'Ni3+': 1, 'Mo': 5,
               'Ni': 2, 'V': 5, 'Mn2+': 5, 'Co': 5, 'Co4+': 1, 'W': 5, 'Fe3+': 5, 'Fe2+': 4,
               'Mn': 5, 'Fe4+': 4, 'Fe': 5, 'Co3+': 0.6}
magmom_keys = list(magmom_dict.keys())
magmom_keys.sort()
# LDA+U parameters : from Pymatgen
LDAUL_dict = {'Mo': 2, 'V': 2, 'Cu': 2, 'W': 2, 'Ag': 2, 'Cr': 2, 'Ta': 2,
              'Nb': 2, 'Mn': 2, 'Re': 2, 'Co': 2, 'Ni': 2, 'Fe': 2}
LDAUL_keys = list(LDAUL_dict.keys())
LDAUL_keys.sort()

LDAUU_dict = {'Mo': 4.38, 'V': 3.1, 'Cu': 4, 'W': 4.0, 'Ag': 1.5, 'Cr': 3.5, 'Ta': 2,
              'Nb': 1.5, 'Mn': 3.9, 'Re': 2, 'Co': 3.4, 'Ni': 6, 'Fe': 4.0}
LDAUU_keys = list(LDAUU_dict.keys())
LDAUU_keys.sort()

LDAUJ_dict = {'Mo': 0, 'V': 0, 'Cu': 0, 'W': 0, 'Ag': 0, 'Cr': 0, 'Ta': 0,
              'Nb': 0, 'Mn': 0, 'Re': 0, 'Co': 0, 'Ni': 0, 'Fe': 0,
              'Li': 0, 'O': 0}
LDAUJ_keys = list(LDAUJ_dict.keys())
LDAUJ_keys.sort()






if __name__=="__main__":
	app.run()
	# app.run(host="172.30.1.5", port=int("80"))
    # app.run(host="172.230.44.58", port=int("80"))
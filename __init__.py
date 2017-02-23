import os

from flask import Flask, render_template, flash, request, url_for, redirect, session, send_from_directory
from werkzeug import secure_filename


app = Flask(__name__)

# ------------------------------------------------------------ #
#                          Main page                           #
# ------------------------------------------------------------ #
@app.route('/')
def homepage():
    try:
        username=session['username']
    except:
        username="not login"
    return render_template("01_main.html", username=username)


# ------------------------------------------------------------ #
#                         Log-in page                          #
# ------------------------------------------------------------ #
@app.route('/login/')
def login():
    return render_template("03_login.html")


# ------------------------------------------------------------ #
#                          VASP page                           #
# ------------------------------------------------------------ #
@app.route('/vasp/', methods=['POST', 'GET'])
def vasp():
    return render_template("11_vasp.html", incar_dict=incar_dict, incar_keys=incar_keys)

app.config['VASP_ALLOWED_EXTENSIONS'] = set(["cif","xsd","POSCAR"])
def vasp_allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1] in app.config['VASP_ALLOWED_EXTENSIONS']

@app.route('/vasp/vasp_upload', methods=['POST', 'GET'])
def vasp_upload():
    # -- Jobname check
    jobname = request.form['jobname']
    if len(jobname) <= 1:
        flash("Please fill the \"Job Name\" field.")
        return redirect(url_for('vasp'))

    # -- File upload
    source_path = "C:\\Users\\bsjun\\Documents\\Codes\\FlaskApp\\VASP\\" + jobname
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

    return render_template("11_vasp.html", jobname=jobname, filenames=filenames, status=status)


incar_dict = {
    "NWRITE":2,"LPETIM":"F","ISTART":0,"INIWAV":1,"IWAVPR":1,"ICHARG":2,"LWAVE":".FALSE.",
    "ALGO":"FAST","NELM":100,"EDIFF":0.0001,"BMIX":3.00,"ENCUT":500,"GGA":"PE","ISYM":2,
    "LDIAG":"T","LREAL":"auto","PREC":"Medium",
    "NSW":200,"NBLOCK":1,"KBLOCK":10,"IBRION":2,"ISIF":3,"POTIM":0.5,"SMASS":3.0,
    "ISMEAR":0,"SIGMA":0.05,"LORBIT":11,
    "NPAR":8,"LPLANE":"T","ISPIN":1,
    "LDAU":".FALSE.","LMAXMIX":4,"LDAUTYPE":2,
    "LVDW":".FALSE.", "VDW_RADIUS":30.0, "VDW_SCALING":0.75, "VDW_D":20.0}
incar_keys = list(incar_dict.keys())
incar_keys.sort()








if __name__=="__main__":
	app.run()
	# app.run(host="172.30.1.5", port=int("80"))
    # app.run(host="172.230.44.58", port=int("80"))
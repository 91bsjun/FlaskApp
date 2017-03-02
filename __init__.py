import os, sys

# -- Flask modules
from flask import Flask, render_template, flash, request, url_for, redirect, session, send_from_directory
from werkzeug import secure_filename

# -- MySQL-python
sys.path.append('/usr/lib64/python2.6/site-packages/')
from MySQLdb import escape_string as thwart
from userDBconnect import connection
from passlib.hash import sha256_crypt                                    # encrypt
import gc

# -- time
import datetime
now = datetime.datetime.now()
now = str(now).split(".")[0]

# -- Config
app = Flask(__name__)
app.config['MAX_CONTENT_LENGTH'] = 300 * 1024 * 1024                     # limitation of upload length (300mb)
root_path = "/var/www/QuantumEngine/FlaskApp"                            # initial directory
sys.path.append(root_path)                                               # append FlaskApp path

# -- CCpy modules
from CCpy.Tools.CCpyTools import linux_command as lc
from CCpy.Tools.CCpyTools import ssh_command, ssh_send_file, ssh_send_directory


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
@app.route('/login/', methods = ["GET","POST"])
def login():
    error = ''
    try:
        c, conn = connection()
        if request.method == "POST":
            data = c.execute("SELECT * FROM users WHERE username = (%s)",
                             [thwart(request.form['username'])])            
            data = c.fetchone()[1]
            if sha256_crypt.verify(request.form['password'], data):
                session['logged_in'] = True
                session['username'] = request.form['username']
                session['password'] = request.form['password']
                data = c.execute("UPDATE users SET last_login=(%s) WHERE username = (%s)",
                                 (thwart(now), thwart(request.form['username'])))
                flash("You are now logged-in.")
                return redirect(url_for("homepage"))
            else:
                error = "Invalid credentials, try again."
                flash(error)
                return render_template("03_login.html")
        gc.collect()
        return render_template("03_login.html", error=error)

    except Exception as e:        
        error = "Invalid credentials, try again."
        flash(error)
        return render_template("03_login.html")

@app.route("/logout/")
def logout():
    session.clear()
    flash("You have been logged out. Have a nice day.")
    gc.collect()
    return redirect(url_for('homepage'))

# ---------------------------------------------------------------------- #
#                           Register page                                #
# ---------------------------------------------------------------------- #
@app.route('/register', methods = ["GET","POST"])
def register():

    return render_template("04_register.html")

@app.route('/register_action', methods = ["GET","POST"])
def register_action():
    # ---------------- get informations ---------------- #
    username = request.form['username']
    password = request.form['password']
    password_chk = request.form['password_chk']
    email = request.form['e-mail']
    country = request.form['country']

    # -------------- check unvalid form --------------- #
    if password != password_chk:
        flash("Password not matched.")
        return render_template("04_register.html",username=username,email=email)
    elif len(username) < 4:
        flash("Length of username must be at least 4.")
        return render_template("04_register.html",username=username,email=email)
    elif len(password) < 6:
        flash("Length of password must be at least 6.")
        return render_template("04_register.html",username=username,email=email)
    elif "@" not in email:
        flash("Invalid e-mail address.")
        return render_template("04_register.html",username=username,email=email)
    # ------------------- add to DB ------------------- #
    else:
        password = sha256_crypt.encrypt((str(password)))
        c, conn = connection()
        x = c.execute("SELECT * FROM users WHERE username = (%s)",[(thwart(username))])
        if int(x) > 0:
            flash("That username is already taken, please try another")
            return render_template('04_register.html',email=email,password=password_chk)
        else:
            c.execute("INSERT INTO users (username, password, email, country, register_time, last_login) VALUES (%s,%s,%s,%s,%s,%s)",
                      (thwart(username), thwart(password), thwart(email), thwart(country), thwart(now), thwart(now)))
            conn.commit()            
            c.close()
            conn.close()
            gc.collect()
            
            flash("Thanks for registering.")
            return redirect(url_for('login'))


# ---------------------------------------------------------------------- #
#                      Account management page                           #
# ---------------------------------------------------------------------- #
@app.route('/account', methods = ["GET","POST"])
def account():
    try:
        username = session['username']
    except:
        flash("Please log-in.")
        return redirect(url_for('login'))

    c, conn = connection()
    data = c.execute("SELECT * FROM users WHERE username = (%s)",
                     [thwart(username)])            
    data = c.fetchone()
    email, country = data[2], data[3]
    return render_template("05_account.html", username=username, email=email, country=country)

@app.route('/account_action', methods = ["GET","POST"])
def account_action():
    # ---------------- get informations ---------------- #
    username = session['username']
    password = request.form['password']
    password_chk = request.form['password_chk']
    email = request.form['e-mail']
    country = request.form['country']

    # -------------- check unvalid form --------------- #
    if password != password_chk:
        flash("Password not matched.")
        return render_template("05_account.html",username=username,email=email)
    elif len(username) < 4:
        flash("Length of username must be at least 4.")
        return render_template("05_account.html",username=username,email=email)
    elif len(password) < 6:
        flash("Length of password must be at least 6.")
        return render_template("05_account.html",username=username,email=email)
    elif "@" not in email:
        flash("Invalid e-mail address.")
        return render_template("05_account.html",username=username,email=email)
    # ------------------- add to DB ------------------- #
    else:
        password = sha256_crypt.encrypt((str(password)))
        c, conn = connection()
        c.execute("UPDATE users SET password=(%s),email=(%s),country=(%s) WHERE username=(%s)",
                  (thwart(password), thwart(email), thwart(country), thwart(username)))
        conn.commit()            
        c.close()
        conn.close()
        gc.collect()
        
        flash("Account information have been editted.")
        return redirect(url_for('account'))



# ---------------------------------------------------------------------- #
#                          VASP Input page                               #
# ---------------------------------------------------------------------- #
# VASP main page
@app.route('/vasp/', methods=['POST', 'GET'])
def vasp():    
    status = "init"
    # ---------------- get username ---------------- #
    try:
        username = session['username']
    except:
        username = "guest"

    # -------------- clear directory --------------- #
    if username != "guest":
        os.chdir(root_path)
        lc("rm -rf ./static/VASP/"+ username +"/*")

    return render_template("11_vasp.html", status=status)


app.config['VASP_ALLOWED_EXTENSIONS'] = set(["cif","xsd","POSCAR"])
def vasp_allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1] in app.config['VASP_ALLOWED_EXTENSIONS']


# VASP input option setting page
@app.route('/vasp/vasp_upload', methods=['POST', 'GET'])
def vasp_upload():
    # ---------------- get username ---------------- #
    try:
        username = session['username']
    except:
        username = "guest"

    # -------- create and move to directory -------- #
    os.chdir(root_path)             # pwd : FlaskApp
    os.chdir("static")              # pwd : FlaskApp/static

    if not os.path.exists("VASP"):
        os.mkdir("VASP")
    os.chdir("VASP")                # pwd : FlaskApp/static/VASP

    if not os.path.exists(username):
        os.mkdir(username)
    os.chdir(username)              # pwd : FlaskApp/static/VASP/username

    # --------------- Check jobname ---------------- #
    jobname = request.form['jobname']
    if len(jobname) <= 1:
        flash("Please fill the \"Job Name\" field.")
        return redirect(url_for('vasp'))

    # ---------------- File upload ---------------- #
    if os.path.exists(jobname):
        flash("Jobname: "+jobname+" is already exist.\nPlease try again.")
        return redirect(url_for('vasp'))
    else:
        os.mkdir(jobname)
    os.chdir(jobname)               # pwd         : FlaskApp/static/VASP/username/jobname
    source_path = os.getcwd()       # source_path : FlaskApp/static/VASP/username/jobname

    uploaded_files = request.files.getlist("file[]")
    filenames = []
    for file in uploaded_files:
        if file and vasp_allowed_file(file.filename):
            filename = secure_filename(file.filename)
            file.save(os.path.join(source_path, filename))
            filenames.append(filename)
    if len(filenames) == 0:
        flash("You probably have uploaded an incorrect file format. Please, try again.")
        return redirect(url_for('vasp'))
    status = "file_uploaded"
    flash("File was uploaded successfully. Check input options and click \"Make Inputs\" button.")

    return render_template("11_vasp.html", jobname=jobname, filenames=filenames, status=status, source_path=source_path,
                           incar_dict=incar_dict, incar_keys=incar_keys, incar_description_dict=incar_description_dict,
                           magmom_dict=magmom_dict, magmom_keys=magmom_keys,
                           LDAUU_dict=LDAUU_dict, LDAUU_keys=LDAUU_keys)

# VASP get input options and make input sets
@app.route('/vaspinputgen', methods=['POST', 'GET'])
def vasp_inputGen():
    # ---------------- get username ---------------- #
    try:
        username = session['username']
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
    source_path = request.form['source_path']        # source_path : FlaskApp/static/VASP/username/jobname
    os.chdir(source_path)

    # ------------ make VASP inputfiles ------------ #

    input_files = [f for f in os.listdir(source_path) if not os.path.isdir(f)]
    from CCpy.VASP.VASPio import VASPInput
    for each_input in input_files:
        VI = VASPInput(each_input)
        VI.cms_vasp_set(vdw=grimme_use, mag=magmom_use, ldau=ldau_use,
                        functional=functional,
                        incar_dict=new_incar_dict, magmom_dict=new_magmom_dict, ldau_dict=new_LDAUU_dict,
                        kpoints=kpoints, flask_app=True)    

    # --------------- make tar file ---------------- #
    lc("tar -zcvf "+jobname+".tar.gz *")
    tar_path = "/static/VASP/" + username + "/" + jobname +"/"+jobname+".tar.gz"

    flash("Inputs were generated successfully.\nNow, send to your server or download them.")    
    status="input_generated"
    
    
    return render_template("11_vasp.html", status=status, jobname=jobname, tar_path=tar_path)


# VASP generated inputs download page
@app.route('/vaspdownload/', methods=['POST', 'GET'])
def vasp_input_download():
    jobname = request.form['jobname']
    tar_path = request.form['tar_path']             # /static/VASP/username/jobname/jobname.tar.gz
    tarfile = tar_path.split("/")[-1]
    tar_fullpath = root_path + tar_path             # /var/www/QuantumEngine/FlaskApp/static/VASP/username/jobname/jobname.tar.gz
    method_radio = request.form['scp_method']

    # --------------- get SSH info ---------------- #
    if method_radio == "cmsuser":
        servername = "166.104.249.31"
        portnum = "7001"
        username = session['username']
        password = session['password']
        target_path = request.form['cms_target_pass']        
    elif method_radio == "otherserver":
        servername = request.form['server_ip']
        portnum = request.form['server_port']
        username = request.form['server_id']
        password = request.form['server_passwd']
        target_path = request.form['target_pass']

    ssh_command(servername,portnum,username,password,"mkdir -p "+target_path)
    ssh_send_file(servername,portnum,username,password,tar_fullpath,target_path)
    ssh_command(servername,portnum,username,password,"cd "+target_path+";tar zxvf "+tarfile+";rm "+tarfile)
    flash("Your Job: " + jobname + " was successfully transfered to "+target_path)
    
    # --------- Add to DB (VASPtracker) ---------- #
    c, conn = connection()
    jobpath = root_path+"/static/VASP/"+username+"/"+jobname
    input_names = [i for i in os.listdir(jobpath) if i != "structures" and ".tar.gz" not in i]
    for i in input_names:
        remote_path = target_path+"/"+i
        c.execute("INSERT INTO VASPtracker (username, jobname, path, date, status) VALUES (%s,%s,%s,%s,%s)",
                  (thwart(username), thwart(jobname), thwart(remote_path), thwart(now), thwart("Ready")))

    # ----------- Add to DB (VASPjobs) ----------- #
    items = str(len(input_names))
    x = c.execute("SELECT * FROM VASPjobs WHERE jobname = (%s)",[(thwart(jobname))])
    if int(x) > 0:
        previous_items = c.fetchone()[2]
        items = str(int(previous_items) + int(items))
        c.execute("UPDATE VASPjobs SET items=(%s),date=(%s) WHERE username=(%s) AND jobname=(%s)",
                  (thwart(items), thwart(now), thwart(username), thwart(jobname)))
        conn.commit()            
        c.close()
        conn.close()
        gc.collect()
    else:
        c.execute("INSERT INTO VASPjobs (username, jobname, items, date) VALUES (%s,%s,%s,%s)",
                  (thwart(username), thwart(jobname), thwart(items), thwart(now)))
    
    return redirect(url_for('vasp'))


# ---------------------------------------------------------------------- #
#                          VASP Input page                               #
# ---------------------------------------------------------------------- #
# VASP tracker main page
@app.route('/vaspjobs/', methods=['POST', 'GET'])
def vaspjobs():
    # ---------------- get username ---------------- #
    try:
        username = session['username']
    except:
        username = "guest"
    c, conn = connection()
    ex = c.execute("SELECT * FROM VASPjobs WHERE username = (%s)",
                     (thwart(username)))
    data = []
    for i in range(ex):
        data.append(c.fetchone())

    status = "init"
    return render_template("12_vaspjobs.html", data=data, status=status)

@app.route('/vasptracker/', methods=['POST', 'GET'])
def vasp_tracker(jobname=None):
    # ---------------- get username ---------------- #
    try:
        username = session['username']
    except:
        username = "guest"

    # --------- get DBs from VASP tracker ----------#
    if jobname:
        pass
    else:
        jobname = request.form['dbname']
    c, conn = connection()
    ex = c.execute("SELECT * FROM VASPtracker WHERE username=(%s) AND jobname=(%s)",
                   (thwart(username),thwart(jobname)))
    data = []
    for i in range(ex):
        data.append(c.fetchone())

    # -------- update items in VASPjobs DB ---------#
    c.execute("UPDATE VASPjobs SET items=(%s) WHERE username=(%s) AND jobname=(%s)",
                  (thwart(str(ex)), thwart(username), thwart(jobname)))

    status = "db_selected"
    return render_template("12_vaspjobs.html", jobname=jobname, data=data, status=status)

@app.route('/vasptracker_action/', methods=['POST', 'GET'])
def vasp_tracker_action():
    # ---------------- get username ---------------- #
    try:
        username = session['username']
        password = session['password']
    except:
        username = "guest"

    servername = "166.104.249.249"
    portnum = "7001"

    checked = request.form.getlist('dbchk')
    action = request.form['dbtracker_action']
    jobname = request.form['jobname']

    c, conn = connection()
    # ----------- delete DB and remote ------------ #
    if action == "del_remote":
        targets = ""
        del_items = 0
        for dir_path in checked:
            targets += dir_path+" "
            # -- remove at VASPtracker db
            ex = c.execute("DELETE FROM VASPtracker WHERE path=(%s) AND username=(%s)",
                           (thwart(dir_path), thwart(username)))
            del_items += 1
        # -- remove remote path
        ssh_command(servername,portnum,username,password,"rm -rf "+targets)
        flash(targets+" were removed from DB and remote successfully.")

        # -- update items in VASPjobs
        data = c.execute("SELECT * from VASPtracker WHERE username=(%s) AND jobname=(%s)",
                         (thwart(username), thwart(jobname)))
        c.execute("UPDATE VASPjobs SET items=(%s) WHERE username=(%s) AND jobname=(%s)",
                  (thwart(str(data)), thwart(username), thwart(jobname)))
        return vasp_tracker(jobname=jobname)
    # ---------------- delete DB  ----------------- #
    elif action == "del_db":
        for dir_path in checked:
            targets += dir_path+" "
            ex = c.execute("DELETE FROM VASPtracker WHERE path=(%s) AND username=(%s)",
                           (thwart(dir_path), thwart(username)))
        flash(targets+" were removed from DB successfully.")
        return vasp_tracker(jobname=jobname)
    # ------------- Do calculation  -------------- #
    elif action == "calc":
        from mpiScript import VASP
        queue = request.form['queuename']
        cpu = request.form['cpu_use']
        os.chdir(root_path+"/static/VASP/"+username)
        for dir_path in checked:            
            jobname = dir_path.split("/")[-1]
            # -- create submit script
            mpi = VASP(cpu, jobname, queue, dir_path)
            mpi_filename = "QEVASP.mpi.sh"
            f = open(mpi_filename,"w")
            f.write(mpi)
            f.close()
            # -- send to server
            ssh_send_file(servername,portnum,username,password,mpi_filename,dir_path)
            ssh_command(servername,portnum,username,password,"cd "+dir_path+";qsub "+mpi_filename)
            # -- update DB status
            c.execute("UPDATE VASPtracker SET status=(%s) WHERE username=(%s) AND path=(%s)",
                      (thwart("Submitted"), thwart(username), thwart(dir_path)))
        flash("Job submitted.")
        return vasp_tracker(jobname=jobname)

    elif action == "update":
        from paramiko import SSHClient,AutoAddPolicy

        client = SSHClient()                                                            
        client.load_system_host_keys()       
        client.set_missing_host_key_policy(AutoAddPolicy())
        client.connect(servername, port=int(portnum), username=username, password=password)

        for dir_path in checked:
            stdin, stdout, stderr = client.exec_command('ls '+dir_path)
            out = [l.strip() for l in stdout]
            if len(out) == 0:
                status = "Moved / Removed"
            else:
                if "CONTCAR" in out or "OSZICAR" in out or "OUTCAR" in out or "vasprun.xml" in out:
                    stdin, stdout, stderr = client.exec_command('tail '+dir_path+'/OUTCAR')
                    out = [l.strip() for l in stdout]
                    if len(out) == 0:
                        status = "Running / Error terminated"
                    else:
                        if 'User time (sec)' in out[0]:
                            status = "Finished"
                        else:
                            status = "Running / Error terminated"
                elif "POSCAR" in out and "POTCAR" in out and "KPOINTS" in out and "INCAR" in out and "QEVASP.mpi.sh" in out:
                    status = "Submitted"
                elif "POSCAR" in out and "POTCAR" in out and "KPOINTS" in out and "INCAR" in out:
                    status = "Ready"
                else:
                    status = "Moved / Removed"
                    
    
            c.execute("UPDATE VASPtracker SET status=(%s) WHERE username=(%s) AND path=(%s)",
                      (thwart(status), thwart(username), thwart(dir_path)))
        flash("DB updated.")
        return vasp_tracker(jobname=jobname)










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


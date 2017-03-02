import sys
sys.path.append('/usr/lib64/python2.6/site-packages/')
import MySQLdb

def connection():
    conn = MySQLdb.connect(host="localhost",
                           user = "QuantumEngine",
                           passwd = "cmschem",
                           db = "QuantumEngine")
    c = conn.cursor()

    return c, conn

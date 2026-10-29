"""Generate the movesr DMV golden set with the real MOVES6 model.

Development/validation helper only. movesr itself never needs Java or MariaDB.
Starts a private MariaDB (already loaded with the MOVES default database by
vein/data-raw/moves/generate_reference.py), runs dmv.mrs single-threaded, exports
the rate tables as gzip CSV and always shuts the server down.

Usage: python3 generate_dmv.py [running|events|evap24]   (MOVES_WORK_DIR defaults to ../../../moves_work)
"""
import csv
import gzip
import hashlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import time
import xml.etree.ElementTree as ET

HERE = Path(__file__).resolve().parent
ROOT = Path(os.environ.get('MOVES_WORK_DIR', str(HERE.parents[2] / 'moves_work'))).resolve()
MODEL = ROOT / 'model'
MARIA = ROOT / 'tools/mariadb-10.11.14-linux-systemd-x86_64'
SOCKET = os.environ.get('MOVES_SOCKET', '/tmp/opencode/vein-moves.sock')
PORT = 13316
import sys
MODE = sys.argv[1] if len(sys.argv) > 1 else 'running'
RUNSPEC, OUT, DB, HOURS = {
    'running': ('dmv.mrs', 'exports-dmv', 'movesr_dmv_output', '16,17'),
    'events': ('dmv_events.mrs', 'exports-dmv-events', 'movesr_dmv_events_output', '16,17'),
    # DC only, all 24 hours: vapor venting depends on the selected hours.
    'evap24': ('dmv_evap24.mrs', 'exports-dmv-evap24', 'movesr_dmv_evap24_output',
               ','.join(str(h) for h in range(1, 25))),
}[MODE]
# MOVES_OUT_DIR writes a repeat run elsewhere (e.g. to time MOVES or check
# that a second run gives identical files).
OUT = ROOT / os.environ.get('MOVES_OUT_DIR', OUT)
CLIENT = [str(MARIA / 'bin/mariadb'), '--no-defaults', '--socket=' + SOCKET,
          '--user=root', '--batch', '--raw']
ENV = dict(os.environ, GOMAXPROCS='1', GOMEMLIMIT='512MiB',
           JAVA_TOOL_OPTIONS='-XX:ActiveProcessorCount=1 -Djava.awt.headless=true')


EXEC_TABLES = '''averagetanktemperature coldsoakinitialhourfraction coldsoaktanktemperature
quarterhourtanktemperature fractionofoperating opmodedistributiontemp soakactivityfraction
hotsoakopmodecount ttgeminutes ttgominutes tempvehandttg samplevehicletrip samplevehicletripbyhour
hotsoakeventbyhour operatingtemperature sourcehours shp shpbyagehour shobyageroadwayhour
sourcetypeagepopulation stmytvvequations2025 tankvaporgencoeffs regclasssourcetypefraction
sourcetypemodelyeargroup temperatureprofileid zonemonthhour link zone
averagetankgasoline samplevehiclesoaking'''.split()


def sql(query):
    return subprocess.check_output(CLIENT + ['-e', query], universal_newlines=True)


def export(query, name):
    """Stream a query to gzip CSV (NULL -> empty)."""
    path = OUT / (name + '.csv.gz')
    proc = subprocess.Popen(CLIENT + ['--quick', '-e', query], stdout=subprocess.PIPE,
                            universal_newlines=True)
    n = 0
    with gzip.open(str(path), 'wt', newline='') as target:
        writer = csv.writer(target)
        for row in csv.reader(proc.stdout, delimiter='\t', quoting=csv.QUOTE_NONE):
            writer.writerow(['' if v == 'NULL' else v for v in row])
            n += 1
    if proc.wait() != 0:
        raise RuntimeError('Export failed: ' + name)
    print(name, n - 1, 'rows', flush=True)
    return path


def main():
    if not (ROOT / 'database-imported').exists():
        raise RuntimeError('Run vein/data-raw/moves/generate_reference.py first (database import)')
    config = ROOT / 'my.cnf'
    server = subprocess.Popen([str(MARIA / 'bin/mariadbd'), '--defaults-file=' + str(config)])
    try:
        for _ in range(120):
            if server.poll() is not None:
                raise RuntimeError('Private database exited; see mariadb.log')
            try:
                subprocess.check_output(CLIENT + ['-e', 'SELECT 1'], stderr=subprocess.DEVNULL)
                break
            except subprocess.CalledProcessError:
                time.sleep(1)
        else:
            raise RuntimeError('Private database did not become ready')
        (MODEL / 'MySQL.txt').write_text(str(PORT))
        shutil.copy2(str(HERE / RUNSPEC), str(ROOT / RUNSPEC))
        paths = ['.'] + [node.attrib['location'] for node in
                         ET.parse(str(MODEL / 'build.xml')).find("path[@id='classpath']")]
        print('Executing MOVES6 DMV RunSpec...', flush=True)
        t0 = time.time()
        subprocess.run(['java', '-Xmx1g', '-cp', os.pathsep.join(paths),
                        'gov.epa.otaq.moves.master.commandline.MOVESCommandLine',
                        '-r', str(ROOT / RUNSPEC)], cwd=str(MODEL), env=ENV,
                       check=True, timeout=6 * 3600)
        print('MOVES finished in %.1f min' % ((time.time() - t0) / 60), flush=True)
        run_id = int(sql('SELECT MAX(MOVESRunID) FROM ' + DB + '.MOVESRun').splitlines()[-1])
        where = ' WHERE MOVESRunID=' + str(run_id)
        OUT.mkdir(exist_ok=True)
        errors = sql('SELECT * FROM ' + DB + '.MOVESError' + where)
        (OUT / 'moveserror.txt').write_text(errors)
        files = [
            export('SELECT * FROM ' + DB + '.rateperdistance' + where, 'rateperdistance'),
            export('SELECT * FROM ' + DB + '.rateperstart' + where, 'rateperstart'),
            export('SELECT * FROM ' + DB + '.ratepervehicle' + where, 'ratepervehicle'),
            export('SELECT * FROM ' + DB + '.rateperprofile' + where, 'rateperprofile'),
            export('SELECT * FROM ' + DB + '.MOVESRun' + where, 'movesrun'),
            export('SELECT * FROM ' + DB + '.MOVESOutput' + where, 'movesoutput'),
            export('SELECT * FROM ' + DB + '.MOVESActivityOutput' + where, 'movesactivityoutput'),
            export('SELECT z.* FROM movesdb20260729.zonemonthhour z JOIN movesdb20260729.zone s '
                   'USING (zoneID) WHERE s.countyID IN (11001,24031,51059) AND monthID=7 '
                   'AND hourID IN (' + HOURS + ') ORDER BY zoneID,hourID', 'zonemonthhour'),
        ]
        # MOVES execution intermediates (tank temperatures, soak fractions,
        # activity) for stage-by-stage validation of the evap port.
        # Not every run creates every table (no tank temperatures without
        # parked evaporative processes); skip the ones that are absent.
        for t in EXEC_TABLES:
            try:
                files.append(export('SELECT * FROM movesexecution.' + t, 'exec_' + t))
            except RuntimeError:
                print('skipped', 'exec_' + t, flush=True)
        manifest = {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in files}
        (OUT / 'SHA256.json').write_text(json.dumps(manifest, indent=2) + '\n')
        print('DMV export complete:', OUT, flush=True)
        if len(errors.splitlines()) > 1:
            print('WARNING: MOVES reported errors; see moveserror.txt', flush=True)
    finally:
        subprocess.run([str(MARIA / 'bin/mariadb-admin'), '--no-defaults',
                        '--socket=' + SOCKET, '--user=root', 'shutdown'], timeout=60)
        server.wait(timeout=60)


if __name__ == '__main__':
    main()

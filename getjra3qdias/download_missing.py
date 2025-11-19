#!/usr/bin/env python

import subprocess
import re
import optparse


def download(prefix, time_str, time_end, variable=None):
    ret_code = -1
    if variable is not None:
        args = [
            "./download_auto.py", '-p', prefix, '-s', time_str, '-e', time_end, '-m', '-v', variable
        ]
    else:
        args = [
            "./download_auto.py", '-p', prefix, '-s', time_str, '-e', time_end, '-m'
        ]
    res = subprocess.run(args=args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    print(res.stdout.decode("utf-8"))
    print(res.stderr.decode("utf-8"))
    ret_code = res.returncode
    return ret_code


def read_filelist(dir_file_path):
    with open(dir_file_path, 'r') as fin:
        flist = fin.readlines()
    flist = [u.replace('\n', '') for u in flist]
    return flist


if __name__ == '__main__':

    usage = '''usage: %prog [options]'''
    parser = optparse.OptionParser(usage=usage)
    parser.add_option('-f', '--filepath', default=None,
                      help='specify path of download file list', metavar='FLIST')

    (options, args) = parser.parse_args()

    if options.filepath is not None:
        filepath = options.filepath
    else:
        filepath = './tinfoin.txt'

    # retrieve missing file list
    file_list = read_filelist(filepath)

    regexp = '^([A-Z ]+): +'
    regexp += '((anl_surf|anl_p|fcst_p|fcst_phy2m)+125)(_|)+'
    regexp += '(([a-z_]+|)).(([0-9]+))'

    for fname in file_list:
        m = re.search(regexp, fname, re.IGNORECASE)
        if m:
            prefix = m.group(2)
            var = m.group(6)
            time = m.group(8)
            if var == '':
                ret = download(prefix=prefix, time_str=time, time_end=time)
            else:
                ret = download(prefix=prefix, time_str=time, time_end=time, variable=var)

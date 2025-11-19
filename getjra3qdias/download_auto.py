#!/usr/bin/env python2

import urllib
import urllib2
import urlparse
import cookielib
import HTMLParser
import subprocess
import sys
import os
import base64
from datetime import datetime, timedelta


import optparse
import netrc
import getpass


class CASLoginParser(HTMLParser.HTMLParser):
    def __init__(self):
        HTMLParser.HTMLParser.__init__(self)
        self.action = None
        self.data = {}

    def handle_starttag(self, tagname, attribute):
        if tagname.lower() == 'form':
            attribute = dict(attribute)
            if 'action' in attribute:
                self.action = attribute['action']
        elif tagname.lower() == 'input':
            attribute = dict(attribute)
            if 'name' in attribute and 'value' in attribute:
                self.data[attribute['name']] = attribute['value']

class DIASAccess():
    def __init__(self, username, password):
        self.__cas_url = 'https://auth.diasjp.net/cas/login?'
        self.__username = username
        self.__password = password
        #self.__cj = cookielib.CookieJar()
        self.__cj = cookielib.MozillaCookieJar()
        self.__opener = urllib2.build_opener(
            urllib2.HTTPCookieProcessor(self.__cj))

    def open(self, url, data=None):
        response = self.__opener.open(url, data)
        response_url = response.geturl()

        if response_url != url and response_url.startswith(self.__cas_url):
            # redirected to CAS login page
            response = self.__login_cas(response)
            if data != None:
                # If POST (data != None), need reopen
                response.close()
                response = self.__opener.open(url, data)

        return response

    def __login_cas(self, response):
        parser = CASLoginParser()
        parser.feed(response.read())
        parser.close()

        if parser.action == None:
            raise LoginError('Not login page')

        action_url = urlparse.urljoin(response.geturl(), parser.action)
        data = parser.data
        data['username'] = self.__username
        data['password'] = self.__password

        response.close()
        response = self.__opener.open(action_url, urllib.urlencode(data))

        if response.geturl() == action_url:
            print 'Authorization fail'
            quit()

        return response

    def dl(self, url, path, file, data=None):
        try:
            response = self.__opener.open(url, data)
            if not os.path.exists('.' + path):
                os.makedirs('.' + path)

            f = open('.' + path + file, 'wb')
            file_size_dl = 0
            block_size = 8192
            while True:
                buffer = response.read(block_size)
                if not buffer:
                    break

                file_size_dl += len(buffer)
                f.write(buffer)

            f.close
            print path + file + "  OK"
            return response

        except urllib2.HTTPError,e:
            print path + file + "  NG"


class LoginError(Exception):
    def __init__(self, e):
        Exception.__init__(self, e)

if __name__ == '__main__':

    host = 'data.diasjp.net'

    usage ='''usage: %prog [options]'''
    parser = optparse.OptionParser(usage=usage)
    parser.add_option('-n', '--netrc', default=None,
                      help='specify the netrc file', metavar='FILE')
    parser.add_option('-u', '--user', default=None,
                      help='specify the DIAS account name',
                      metavar='USERNAME')
    parser.add_option('-s', '--time_start', default=None,
                      help='specify start time as YYYYMMDDHH', metavar='TSTA')
    parser.add_option('-e', '--time_end', default=None,
                      help='specify end time as YYYYMMDDHH', metavar='TEND')
    parser.add_option('-p', '--prefix', default=None,
                      help='specify prefix', metavar='PREF')
    parser.add_option('-v', '--variable', default=None,
                      help='variable name if needed (default: auto)', metavar='VAR')
    parser.add_option('-m', '--missing', action='store_true', default=False,
                      help='download missing file only (ignote ctl, idl)')


    (options, args) = parser.parse_args()

    (login, password) = (None, None)

    try:
      auth = netrc.netrc(options.netrc).authenticators(host)
      if auth is not None:
            (login, account, password) = auth
    except (IOError):
      pass

    if options.user is not None:
        login = options.user
        password = None

    if login is None:
        login = raw_input('Username: ')

    if password is None:
        password = getpass.getpass('Password: ')

    access = DIASAccess(login, password)


    targeturl='https://data.diasjp.net/dl/storages/filelist/dataset:645'
    response = access.open(targeturl)
    response.close()

    opt_missing = options.missing

    # prefix = 'anl_p125', 'fcst_p125', 'anl_surf125', 'fcst_phy2m125'
    if options.prefix is not None:
        prefix = options.prefix
    else:
        prefix = 'anl_p125'
    if options.variable is not None:
        variable = options.variable
    else:
        variable = 'auto'
    # start time (tsta = "20250901")
    if options.time_start is not None:
        tsta = options.time_start
    else:
        time_cur = datetime.utcnow() - timedelta(days=5)
        tsta = datetime(time_cur.year, time_cur.month, time_cur.day).strftime("%Y%m%d00")
    # end time (tend = "20250901")
    if options.time_end is not None:
        tend = options.time_end
    else:
        time_cur = datetime.utcnow() - timedelta(days=4)
        tend = datetime(time_cur.year, time_cur.month, time_cur.day).strftime("%Y%m%d18")


    base_url = 'https://data.diasjp.net/dl/storages/downloadCmd/'
    time_str = datetime(int(tsta[0:4]), int(tsta[4:6]), int(tsta[6:8]), int(tsta[8:10]), 0, 0)
    time_end = datetime(int(tend[0:4]), int(tend[4:6]), int(tend[6:8]), int(tend[8:10]), 0, 0)
    time_step = timedelta(hours=6)
    if prefix == 'anl_p125':
        # pressure level data
        if variable == 'auto':
            var_list = ['ugrd', 'vgrd', 'tmp', 'depr', 'hgt', 'rh', 'spfh', 'vvel', 'relv', 'reld', 'vpot', 'strm']
        else:
            var_list = [variable]
    elif prefix == 'fcst_p125':
        if variable == 'auto':
            var_list = ['cdca', 'cwat', 'o3mr']
        else:
            var_list = [variable]
    else:
        var_list = [ None ]

    for var in var_list:
        ofirst = True
        time = time_str
        time_bef = time_str - time_step
        while True:
            if time > time_end:
                break
            # one time per month
            if time.month != time_bef.month or ofirst:
                YYMM = time.strftime('%Y%m')
                dir_path = '/JRA3Q/Hist/Daily/' + prefix + '/' + YYMM + '/'
                if var is None:
                    file_name0 = prefix + '.' + YYMM + '.ctl'
                    file_name1 = prefix + '.' + YYMM + '.idx'
                else:
                    file_name0 = prefix + '_' + var + '_L42.' + YYMM + '.ctl'
                    file_name1 = prefix + '_' + var + '_L42.' + YYMM + '.idx'
                if not opt_missing:
                    for file_name in [file_name0, file_name1]:
                        dir_file = dir_path + file_name
                        enc = base64.b64encode(dir_file)
                        access.dl(base_url + enc, dir_path, file_name)
            #
            # 6 hour data
            ftime = time.strftime('%Y%m%d%H')
            if var is None:
                file_name = prefix + '.' + ftime
            else:
                file_name = prefix + '_' + var + '.' + ftime
            dir_file = dir_path + file_name
            enc = base64.b64encode(dir_file)
            access.dl(base_url + enc, dir_path, file_name)
            time_bef = time
            time = time + time_step
            if ofirst:
                ofirst = False


import datetime, sys, urllib2

from collections import namedtuple
HTML = namedtuple('HTML', 'html datetime')

class Internet(object):
    @staticmethod
    def get_url(url):
        print "getting: ", url,
        request = urllib2.Request(url)
        request.add_header('User-Agent', 'Chrome/24.0.1309.0 Safari/537.17 (compatible; MSIE 6.0; Windows 98)')
        try:
            response = urllib2.urlopen(request)
        except:
            print ""
            print "\terror downloading url", url, "; exiting..."
            sys.exit(1)
        ret = response.read()
        print "done"
        return ret

    @staticmethod
    def get_url_cached(mc, key_base, url, minutes_till_expire = 30):
        key = key_base + url
        key = key[0:249]
        now = datetime.datetime.now()
        if key in mc:
            h = mc[key]
            if(now - h.datetime).total_seconds() < minutes_till_expire*60:
                print "using cached html for url:", url
                return h.html
        html = Internet.get_url(url)
        h = HTML(html, now)
        mc[key] = h
        return html

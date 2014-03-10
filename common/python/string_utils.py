class StringUtils(object):
    @staticmethod
    def to_unicode(str):
        if isinstance(str, unicode):
            return str
        return unicode(str, errors='ignore')


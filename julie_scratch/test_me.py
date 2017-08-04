import json

def hello():
    '''Does nothing.

    >>> hello()
    4

    '''
    return 4

if __name__ == '__main__':
    import doctest
    doctest.testmod()
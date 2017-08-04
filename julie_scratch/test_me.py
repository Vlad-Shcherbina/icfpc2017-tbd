import sys

def hello():
    '''Does nothing.

    >>> hello()
    3

    '''
    return 4

if __name__ == '__main__':
    import doctest
    doctest.testmod()
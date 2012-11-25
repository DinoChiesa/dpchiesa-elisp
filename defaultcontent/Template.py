#!/usr/bin/python
# -*- coding: utf-8 -*-

sresult = stat(filePath)

def main(argv):
    prettyPrintXml(argv[0])


if __name__ == "__main__":
    main(sys.argv[1:])
